## Check_Trawl_Data_Final_1977-1999.R                                       ####
##
## Author:  H Stiff
## Date:    11-Feb-2026
## Notes:   This script reads final trawl data CSV files from the 04_final_output
##          subfolder, and performs some QA/QC checks, principally for extreme
##          values in numeric fields that are not categorical or code fields.
##          E.g., duration, depth, fish lengths and weights.
##          Extreme values outside the 0.001 and 0.999 percentiles (which can be
##          revised in call to univariate_outliers function) are listed
##          for each variable in separate dataframes named <field name>_flags, 
##          and exported to CSVs in folder 07_QC_outputs for further examination.

# ------------------------------------------------------------
# SETUP libraries and functions                                             ####

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(rlang)
  library(robustbase)  # covMcd (robust Mahalanobis)  ← Robust multivariate ϵ 1.5
  library(dbscan)      # lof()                        ← Density-based LOF
})

# Path to subfolder "04_final_output" under current working directory
qc_qa_dir  <- file.path(getwd(), "TRAWL_BIOSAMPLE/07_QC_outputs")
finals_dir <- file.path(getwd(), "TRAWL_BIOSAMPLE/04_final_output")
final_data <- readr::read_csv(file.path(finals_dir, "Trawl_data_FINAL_1977-1999.csv"), show_col_types = FALSE)
names(final_data)

set.seed(42)

# Gather meta-data variables
metadata <- c("lake_name", "lake_code", "ats_year", "trawl_date", "trawl_number", "depth_m",
              "start_time", "end_time", "duration_minutes", "calc_duration_time",
              "species_info_code", "species_code", "species_common_name", "life_stage", "preservative_code")

# ------------------------------------------------------------
# PROCESS Trawl_data_FINAL_1977-1999.csv data                               ####

# Identify numeric columns
num_cols_all <- final_data %>%
  select(where(is.numeric)) %>%
  names()

# Columns to exclude from ALL numeric checks (explicit do-not-analyze list)
exclude_cols <- c(
  "lake_code", "ats_year", "fish_id", "scale", "scale_book",
  "source_line", "fish_total", "species_info_code", "processor")

# Always-include whitelist (these columns must be analyzed even if few-levels)
always_include <- c("duration_minutes", "trawl_number")

# OPTIONAL: treat columns with few unique values as categorical and exclude from outlier checks
exclude_discrete <- TRUE
discrete_threshold <- 15

if (exclude_discrete) {
  few_level_cols <- final_data %>%
    dplyr::summarize(across(all_of(num_cols_all), ~ dplyr::n_distinct(.x, na.rm = TRUE))) %>%
    tidyr::pivot_longer(everything(), names_to = "col", values_to = "n_uniq") %>%
    dplyr::filter(n_uniq <= discrete_threshold) %>%
    dplyr::pull(col)
  
  # Start from all numeric cols minus the few-level ones
  num_cols <- setdiff(num_cols_all, few_level_cols)
  
  # Re-add whitelist columns if they were removed by the few-level filter
  num_cols <- union(num_cols, intersect(always_include, num_cols_all))
} else {
  num_cols <- num_cols_all
}

# Apply explicit exclusions LAST so they always win
num_cols <- setdiff(num_cols, exclude_cols)

# Optional: quick sanity check (should be character(0))
# intersect(num_cols, exclude_cols)

length(num_cols); num_cols

# Helper to compute robust z-scores safely
robust_z <- function(x) {
  med <- median(x, na.rm = TRUE)
  mad_val <- mad(x, constant = 1, na.rm = TRUE)  # raw MAD to pair with 0.6745
  if (is.na(mad_val) || mad_val == 0) return(rep(NA_real_, length(x)))
  0.6745 * (x - med) / mad_val
}

univariate_outliers <- function(df, cols, p_low = 0.001, p_high = 0.999,
                                z_thresh = 4, id_cols = c("fish_unique_ID", "trawl_unique_ID")) {
  n <- nrow(df)
  
  stats_tbl <- purrr::map_dfr(cols, function(cl) {
    v <- df[[cl]]
    if (!is.numeric(v)) return(NULL)
    
    # Skip columns with too few non-NA values (avoid degenerate quantiles/MAD)
    if (sum(!is.na(v)) < 5) return(NULL)
    
    q1  <- as.numeric(quantile(v, probs = p_low,  na.rm = TRUE, names = FALSE))
    q99 <- as.numeric(quantile(v, probs = p_high, na.rm = TRUE, names = FALSE))
    rz  <- robust_z(v)
    
    tibble::tibble(
      column   = cl,
      row_id   = seq_len(n),  # <- align rows back to df by index
      value    = v,
      p1       = q1,
      p99      = q99,
      rz       = rz,
      flag_pctl = (v < q1 | v > q99) & !is.na(v),
      flag_rz   = abs(rz) > z_thresh & !is.na(rz))
  })
  
  # Keep only flagged rows, then join IDs by row_id (NOT bind_cols)
  flagged <- stats_tbl %>%
    dplyr::filter(flag_pctl | flag_rz) %>%
    dplyr::left_join(
      df %>%
        dplyr::mutate(row_id = dplyr::row_number()) %>%
        dplyr::select(dplyr::all_of(c("row_id", id_cols))),
      by = "row_id") %>%
    dplyr::arrange(dplyr::desc(flag_pctl), dplyr::desc(flag_rz), column, row_id)
  
  flagged
}

flag_univariate_outliers <- univariate_outliers(final_data, num_cols)

# Pick any 3 rows and join back to see the original record context
# Which extra columns do you actually want to bring from final_data?
context_cols <- c(
  "lake_name", "ats_year", "trawl_date", "trawl_number", "duration_minutes",
  "species_code", "species_common_name",
  "fish_length_mm", "fish_weight_g", "standardized_weight_g", "calc_std_weight_g")

set.seed(1)
sample_rows <- sample(unique(flag_univariate_outliers$row_id), 3)

# 1) Confirm row_id maps back to your source row
flag_univariate_outliers %>%
  filter(row_id %in% sample_rows) %>%
  left_join(
    final_data %>%
      mutate(row_id = row_number()) %>%
      select(row_id, all_of(context_cols)),  # <-- no fish_unique_ID here
    by = "row_id"
  ) %>%
  select(
    row_id, column, value, p1, p99, rz, flag_pctl, flag_rz,
    all_of(context_cols),
    fish_unique_ID, trawl_unique_ID  # <-- keep the ones already in flag_univariate_outliers
  ) %>%
  print(n = 50)

# 2) See which columns are most often flagged, and by which rule
flag_univariate_outliers %>%
  mutate(rule = case_when(
    flag_pctl & flag_rz ~ "both",
    flag_pctl           ~ "percentile",
    flag_rz             ~ "robust_z",
    TRUE                ~ "none"
  )) %>%
  count(column, rule, name = "n_flagged") %>%
  arrange(desc(n_flagged)) %>%
  print(n = 100)

# 3) Row‑level hot list”: records with many fields flagged
# row_issue_counts <- flag_univariate_outliers %>%
#   count(row_id, name = "n_fields_flagged") %>%
#   left_join(
#     final_data %>%
#       mutate(row_id = row_number()) %>%
#       select(row_id, lake_name, ats_year, trawl_date, trawl_number,
#              species_common_name, fish_unique_ID, trawl_unique_ID),
#     by = "row_id"
#   ) %>%
#   arrange(desc(n_fields_flagged))
# head(row_issue_counts, 20)

# Focus on percentile-triggered flags (includes "both")

# (Re)define the context columns you want to bring from final_data
context_cols <- c(
  "lake_name", "ats_year", "trawl_date", "trawl_number", "duration_minutes",
  "species_code", "species_common_name", "fish_length_mm", "fish_weight_g",
  "standardized_weight_g", "calc_std_weight_g", "source_files")

# 1) Filter to percentile (and "both" i.e., percentile and robust rules) only
all_numeric_cols_percentile_flags <- flag_univariate_outliers %>%
  dplyr::filter(flag_pctl) %>%                         # <- keeps percentile-only + both
  dplyr::mutate(
    rule = dplyr::case_when(
      flag_pctl & flag_rz ~ "both",
      flag_pctl           ~ "percentile",
      TRUE                ~ "other"))

# 2) Join with source rows to add context (no duplicate ID columns)
all_numeric_cols_percentile_flags_ctx <- all_numeric_cols_percentile_flags %>%
  dplyr::left_join(
    final_data %>%
      dplyr::mutate(row_id = dplyr::row_number()) %>%
      dplyr::select(row_id, dplyr::all_of(context_cols)),
    by = "row_id") %>%
  dplyr::select(
    row_id, column, value, p1, p99, rz, rule,
    # keep IDs from flag_univariate_outliers (already present there)
    fish_unique_ID, trawl_unique_ID,
    dplyr::all_of(context_cols))

# Optional: quick head check
# print(all_numeric_cols_percentile_flags_ctx, n = 20)

# 3) Produce a named list of data frames, one per column
flags_by_column <- split(all_numeric_cols_percentile_flags_ctx, f = all_numeric_cols_percentile_flags_ctx$column, drop = TRUE)

# See what you got:
names(flags_by_column)
sapply(flags_by_column, nrow)
# ------------------------------------------------------------

# CREATE one data frame object per data column in Environment window        ####

flags_suffixed <- setNames(flags_by_column, paste0(names(flags_by_column), "_flags"))
list2env(flags_suffixed, envir = .GlobalEnv)

# Reduce meta-data flags to unique values of the meta-data
trawl_number_flags <- trawl_number_flags %>%
  select(lake_name, trawl_date, trawl_number, duration_minutes, 
         trawl_unique_ID, column, value, p1, p99, source_files) %>%
  arrange(lake_name, trawl_date, trawl_unique_ID) %>%
  unique() # displays 3 Owikeno surveys - all legit (260218)
hist(final_data$trawl_number) # check all records for distn of trawl_numbers

depth_m_flags <- depth_m_flags %>%
  select(lake_name, trawl_date, trawl_number, duration_minutes, 
         trawl_unique_ID, column, value, p1, p99, source_files) %>%
  arrange(lake_name, trawl_date, trawl_unique_ID) %>%
  unique()  # shows 2 trawls at depth 65 and 70 which are probably legit (260218)
hist(final_data$depth_m)  # check all records for distn of depths (lots of zeros)

duration_minutes_flags <- duration_minutes_flags %>%
  select(lake_name, trawl_date, trawl_number, duration_minutes, 
         trawl_unique_ID, column, value, p1, p99, source_files) %>%
  arrange(lake_name, trawl_date, trawl_unique_ID) %>%
  unique()  # shows two records: Hen trawl 1986-08-30_3_1_0 at 46 minutes (legit) and 
            # Klukshu 1988-08-31_53_7_0 at zero minutes - but real start- and end-times, so sent AA a message to fix (260218) 
hist(final_data$duration_minutes)
chk <- final_data %>% filter(lake_code == 53) %>%
  select(all_of(metadata)) %>% unique()

chk <- final_data %>% filter(calc_duration_time >50 ) %>% # the large calcs appear to be related to typos in the start/end-times...
#chk<- final_data %>% filter(calc_duration_time != duration_minutes & calc_duration_time <=50 ) %>% # this one for chking observed vs calculated durations...
  select(all_of(metadata), trawl_unique_ID) %>%
  select(-species_code, -species_info_code, -species_common_name, -life_stage) %>%
  unique() %>% 
  mutate(diff_time = calc_duration_time - duration_minutes) %>%
  arrange(lake_name, ats_year, trawl_date)
hist(chk$diff_time) # (chk$calc_duration_time) # sent list of 15 start- or end-time fixes to AA to fix (260218)
dump <- paste0(qc_qa_dir, "/", "fix_trawl_times.csv")
write.csv(chk, dump)


# Arrange fish size flags
fish_length_mm_flags <- fish_length_mm_flags %>%
  mutate(k_factor_prsrv = 100 * fish_weight_g / ((fish_length_mm / 10)^3)) %>%           # based on preserved (measured) weight
  mutate(k_factor_std   = 100 * standardized_weight_g / ((fish_length_mm / 10)^3)) %>%   # based on estimated actual weight - best for K factor 
  select(lake_name, trawl_date, trawl_number, trawl_unique_ID, 
         species_code, species_common_name, p1, p99, fish_length_mm, 
         fish_weight_g, standardized_weight_g, calc_std_weight_g, k_factor_std, k_factor_prsrv,
         fish_unique_ID, source_files) %>%
  arrange(species_code, k_factor_std, lake_name, fish_length_mm, trawl_date) %>%
  unique()

chk_sox_final <- final_data %>% filter(species_code == 1) %>% # review all final SOCKEYE lengths...
  mutate(k_factor_std   = 100 * standardized_weight_g / ((fish_length_mm / 10)^3))
hist(chk_sox_final$fish_length_mm)
boxplot(chk_sox_final$fish_length_mm)
mean(chk_sox_final$k_factor_std, na.rm = TRUE)

chk_sox_flags <- fish_length_mm_flags %>% filter(species_code == 1) 
hist(chk_sox_flags$fish_length_mm)     # at low end, shows two 17 mm sox, one with K>3 (likely measurement error)
boxplot(chk_sox_flags$fish_length_mm)  # at high end,shows 115 sox 109-180mm; 113 have good K-factors (~1.0), 2 skinny fish K<0.50
mean(chk_sox_flags$k_factor_std, na.rm = TRUE)

chk_stx_final <- final_data %>% filter(species_code == 2) %>% # & fish_length_mm < 20) %>% # review all final STICKLEBACK lengths...
  mutate(k_factor_std = 100 * standardized_weight_g / ((fish_length_mm / 10)^3)) %>%
  select(lake_name, lake_code, species_code, species_common_name, fish_length_mm, fish_weight_g, standardized_weight_g, k_factor_std)
hist(chk_stx_final$fish_length_mm)
hist(chk_stx_final$k_factor_std)
boxplot(chk_stx_final$fish_length_mm)

chk_stx_flags <- fish_length_mm_flags %>% filter(species_code == 2) 
hist(chk_stx_flags$fish_length_mm)     # at high end, 1 115 mm stk but with K=0.9, so OKAY
boxplot(chk_stx_flags$fish_length_mm)  # at low end, shows 14-15 fish with len < 12mm, many missing std_weight, so k_prsrv > 3
                                       # thus probably calculated based on observed preserved weight, which would bias K high?

fish_weight_g_flags <- fish_weight_g_flags %>%
  mutate(k_factor_std = 100 * standardized_weight_g / ((fish_length_mm / 10)^3)) %>%
  select(lake_name, trawl_date, trawl_number, trawl_unique_ID, 
         species_code, species_common_name, 
         fish_length_mm, fish_weight_g, p1, p99, 
         standardized_weight_g, calc_std_weight_g, k_factor_std,
         fish_unique_ID, source_files) %>%
  arrange(species_code, lake_name, fish_weight_g, trawl_date) %>%
  unique()

standardized_weight_g_flags <- standardized_weight_g_flags %>%
  mutate(k_factor_std = 100 * standardized_weight_g / ((fish_length_mm / 10)^3)) %>%
  select(lake_name, trawl_date, trawl_number, trawl_unique_ID, 
         species_code, species_common_name, 
         fish_length_mm, fish_weight_g, standardized_weight_g, p1, p99, 
         calc_std_weight_g, k_factor_std,
         fish_unique_ID, source_files) %>%
  arrange(species_code, lake_name, standardized_weight_g, trawl_date) %>%
  unique()

calc_std_weight_g_flags <- calc_std_weight_g_flags %>%
  mutate(k_factor_std = 100 * standardized_weight_g / ((fish_length_mm / 10)^3)) %>%
  select(lake_name, trawl_date, trawl_number, trawl_unique_ID, 
         species_code, species_common_name, 
         fish_length_mm, fish_weight_g, standardized_weight_g,  
         calc_std_weight_g, p1, p99, k_factor_std,
         fish_unique_ID, source_files) %>%
  arrange(species_code, lake_name, calc_std_weight_g, trawl_date) %>%
  unique()

# ------------------------------------------------------------
# EXPORT each column's flagged rows to a CSV for review                     ####
out_qc_dir <- file.path(getwd(), "TRAWL_BIOSAMPLE/07_QC_outputs")
dir.create(out_qc_dir, showWarnings = FALSE, recursive = TRUE)

# 1) Find all object names in the chosen environment that end with "_flags"
env <- .GlobalEnv  # or specify another environment if you used one
flag_objs <- ls(envir = env, pattern = "_flags$")

# 2) Pull those objects into a named list (names are the object names)
flag_list <- mget(flag_objs, envir = env)

# 3) Write each data frame to CSV; name based on the object name
iwalk(
  flag_list,
  ~ {
    stopifnot(is.data.frame(.x))  # simple guard
    # optional: skip empty frames
    if (nrow(.x) == 0) return(invisible(NULL))
    readr::write_csv(.x, file.path(out_qc_dir, paste0(.y, ".csv")))
  }
)

# ------------------------------------------------------------