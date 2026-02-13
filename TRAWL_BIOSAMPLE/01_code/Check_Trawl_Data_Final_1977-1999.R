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
finals_dir <- file.path(getwd(), "TRAWL_BIOSAMPLE/04_final_output")
final_data <- readr::read_csv(file.path(finals_dir, "Trawl_data_FINAL_1977-1999.csv"), show_col_types = FALSE)

names(final_data)

set.seed(42)

# Identify numeric columns
num_cols_all <- final_data %>%
  select(where(is.numeric)) %>%
  names()

# Columns to exclude from ALL numeric checks (explicit do-not-analyze list)
exclude_cols <- c(
  "lake_code", "ats_year", "trawl_number", "fish_id", "scale", "scale_book",
  "source_line", "fish_total", "species_info_code", "processor")

# Always-include whitelist (these columns must be analyzed even if few-levels)
always_include <- c("duration_minutes")

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

uni_flags <- univariate_outliers(final_data, num_cols)

# Pick any 3 rows and join back to see the original record context
# Which extra columns do you actually want to bring from final_data?
context_cols <- c(
  "lake_name", "ats_year", "trawl_date", "trawl_number", "duration_minutes",
  "species_code", "species_common_name",
  "fish_length_mm", "fish_weight_g", "standardized_weight_g", "calc_std_weight_g")

set.seed(1)
sample_rows <- sample(unique(uni_flags$row_id), 3)

# 1) Confirm row_id maps back to your source row
uni_flags %>%
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
    fish_unique_ID, trawl_unique_ID  # <-- keep the ones already in uni_flags
  ) %>%
  print(n = 50)

# 2) See which columns are most often flagged, and by which rule
uni_flags %>%
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
# row_issue_counts <- uni_flags %>%
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

# ------------------------------------------------------------
# Focus on percentile-triggered flags (includes "both")
# ------------------------------------------------------------

# (Re)define the context columns you want to bring from final_data
context_cols <- c(
  "lake_name", "ats_year", "trawl_date", "trawl_number", "duration_minutes",
  "species_code", "species_common_name", "fish_length_mm", "fish_weight_g",
  "standardized_weight_g", "calc_std_weight_g", "source_files")

# 1) Filter to percentile (and "both") only
pctl_flags <- uni_flags %>%
  dplyr::filter(flag_pctl) %>%                         # <- keeps percentile-only + both
  dplyr::mutate(
    rule = dplyr::case_when(
      flag_pctl & flag_rz ~ "both",
      flag_pctl           ~ "percentile",
      TRUE                ~ "other"))

# 2) Join with source rows to add context (no duplicate ID columns)
pctl_flags_ctx <- pctl_flags %>%
  dplyr::left_join(
    final_data %>%
      dplyr::mutate(row_id = dplyr::row_number()) %>%
      dplyr::select(row_id, dplyr::all_of(context_cols)),
    by = "row_id") %>%
  dplyr::select(
    row_id, column, value, p1, p99, rz, rule,
    # keep IDs from uni_flags (already present there)
    fish_unique_ID, trawl_unique_ID,
    dplyr::all_of(context_cols))

# Optional: quick head check
# print(pctl_flags_ctx, n = 20)

# 3) Produce a named list of data frames, one per column
flags_by_column <- split(pctl_flags_ctx, f = pctl_flags_ctx$column, drop = TRUE)

# See what you got:
names(flags_by_column)
sapply(flags_by_column, nrow)

# Optional: create one data frame object per column in your environment
# (CAUTION: this can clutter your workspace; the list is usually better)
flags_suffixed <- setNames(flags_by_column, paste0(names(flags_by_column), "_flags"))
list2env(flags_suffixed, envir = .GlobalEnv)

# Reduce meta-data flags to unique values of the meta-data
depth_m_flags <- depth_m_flags %>%
  select(lake_name, trawl_date, trawl_number, duration_minutes, 
         trawl_unique_ID, column, value, p1, p99, source_files) %>%
  arrange(lake_name, trawl_date, trawl_unique_ID) %>%
  unique()

duration_minutes_flags <- duration_minutes_flags %>%
  select(lake_name, trawl_date, trawl_number, duration_minutes, 
         trawl_unique_ID, column, value, p1, p99, source_files) %>%
  arrange(lake_name, trawl_date, trawl_unique_ID) %>%
  unique()

# Arrange fish size flags
fish_length_mm_flags <- fish_length_mm_flags %>%
  select(lake_name, trawl_date, trawl_number, trawl_unique_ID, 
         species_code, species_common_name, fish_length_mm, p1, p99, 
         fish_weight_g, standardized_weight_g, calc_std_weight_g, 
         fish_unique_ID, source_files) %>%
  arrange(species_code, lake_name, fish_length_mm, trawl_date) %>%
  unique()

fish_weight_g_flags <- fish_weight_g_flags %>%
  select(lake_name, trawl_date, trawl_number, trawl_unique_ID, 
         species_code, species_common_name, 
         fish_length_mm, fish_weight_g, p1, p99, 
         standardized_weight_g, calc_std_weight_g, 
         fish_unique_ID, source_files) %>%
  arrange(species_code, lake_name, fish_weight_g, trawl_date) %>%
  unique()

standardized_weight_g_flags <- standardized_weight_g_flags %>%
  select(lake_name, trawl_date, trawl_number, trawl_unique_ID, 
         species_code, species_common_name, 
         fish_length_mm, fish_weight_g, standardized_weight_g, p1, p99, 
         calc_std_weight_g, 
         fish_unique_ID, source_files) %>%
  arrange(species_code, lake_name, standardized_weight_g, trawl_date) %>%
  unique()

calc_std_weight_g_flags <- calc_std_weight_g_flags %>%
  select(lake_name, trawl_date, trawl_number, trawl_unique_ID, 
         species_code, species_common_name, 
         fish_length_mm, fish_weight_g, standardized_weight_g,  
         calc_std_weight_g, p1, p99,
         fish_unique_ID, source_files) %>%
  arrange(species_code, lake_name, calc_std_weight_g, trawl_date) %>%
  unique()

# Optional: write each column's flagged rows to a CSV for review
out_qc_dir <- file.path(getwd(), "TRAWL_BIOSAMPLE/07_QC_outputs")
dir.create(out_qc_dir, showWarnings = FALSE, recursive = TRUE)
purrr::iwalk(flags_by_column, ~ readr::write_csv(.x, file.path(out_qc_dir, paste0("flag_xtrm_vals_", .y, ".csv"))))
