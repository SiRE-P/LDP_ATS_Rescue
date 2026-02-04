###############################################################################
## TRAWL data — Refactored pipeline (SAS & DAT across years, 1977–1999)
## Authors (original workflow): Alice Assmar, David Hunt, Howard Stiff, Athena Ogden
## Refactor: efficiency + maintainability; same scientific rules
## Source:   CoPilot Artificial Intelligence Program 04-Feb-2026
###############################################################################

suppressPackageStartupMessages({
  libs <- c("dplyr","readr","tidyr","purrr","stringr","tibble","lubridate","janitor")
  to_install <- setdiff(libs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, quiet=TRUE)
  lapply(libs, require, character.only=TRUE)
})

# ------------------------------- Paths ---------------------------------------
working_directory <- "./TRAWL_BIOSAMPLE/02_intermediate_out/"
error_directory   <- "./TRAWL_BIOSAMPLE/03_errors_out/"
final_directory   <- "./TRAWL_BIOSAMPLE/04_final_output/"

dir.create(error_directory, showWarnings = FALSE, recursive = TRUE)
dir.create(final_directory, showWarnings = FALSE, recursive = TRUE)

# ----------------------- Helper: safe CSV read -------------------------------
safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  out <- tryCatch(readr::read_csv(path, show_col_types = FALSE),
                  error = function(e) NULL)
  if (is.null(out) || !nrow(out)) return(NULL)
  out
}

# --------------------- 1) Manifest + vectorized ingestion --------------------
file_manifest <-
  tibble(path = list.files(working_directory,
                           pattern = "(?i)^trawl\\d{2,}_(SAS|DAT)\\.csv$",
                           full.names = TRUE)) %>%
  mutate(file = basename(path),
         year = readr::parse_number(file),
         source = if_else(str_detect(file, "(?i)SAS"), "SAS", "DAT"),
         year_label = sprintf("Trawl_%02d", year)) %>%
  arrange(year, source)

if (!nrow(file_manifest)) stop("No input CSVs found in: ", working_directory)

raw_list <- file_manifest %>%
  mutate(data = map(path, safe_read_csv)) %>%
  filter(!map_lgl(data, is.null))

# ---------------- 2) Coercions applied once to every table -------------------
char_cols <- c("duration_mi","processor","scale_book","sample_number")

coerce_and_pad <- function(df, char_cols) {
  for (nm in char_cols) {
    if (!nm %in% names(df)) df[[nm]] <- NA_character_
    df[[nm]] <- as.character(df[[nm]])
  }
  df
}

raw_list <- raw_list %>%
  mutate(data = map(data, coerce_and_pad, char_cols = char_cols))

# ---------------- 3) Per-year SAS ↔ DAT joins (full) -------------------------
# single list of join keys; safe: missing keys will be added as NA before join
join_keys <- c("process_date","trawl_date","fish_total","fish_length_mm","trawl_unique_ID",
               "fish_unique_ID","species_code","trawl_number","processor","lake_code","lake_name",
               "scale_book","scale","age","preservative_code","duration_mi","depth_m","fish_id",
               "fish_description","preservative_description","weight_conversion_formula","sample_number",
               "fish_weight_g","aging_technique_name","aging_technique","trawl_month","ats_year",
               "program_notes","species_code_comment","start_time","end_time")

# Generalized join (prefer DAT after join via resolver)
joined_by_year <- raw_list %>%
  select(year_label, source, data) %>%
  group_by(year_label) %>%
  summarise(
    data = {
      lst <- setNames(data, source)
      dat <- lst[["DAT"]]; sas <- lst[["SAS"]]
      if (is.null(dat) && is.null(sas)) NULL else {
        if (is.null(dat)) dat <- tibble()
        if (is.null(sas)) sas <- tibble()
        # add any missing keys so join is stable
        add_keys <- function(df) { for (k in join_keys) if (!k %in% names(df)) df[[k]] <- NA; df }
        dat <- add_keys(dat); sas <- add_keys(sas)
        # annotate provenance columns; ensure these names don't collide
        if (!"source_file.dat" %in% names(dat)) dat$source_file.dat <- file_manifest$path[file_manifest$path %in% attr(dat, "file")] # may be NULL
        if (!"source_file.sas" %in% names(sas)) sas$source_file.sas <- file_manifest$path[file_manifest$path %in% attr(sas, "file")]
        # full join with suffixes
        joyn::full_join(dat, sas, suffix = c(".dat",".sas"), by = join_keys)
      }
    },
    .groups = "drop"
  ) %>% filter(!map_lgl(data, is.null))

# -------------- 4) Column resolver (prefer DAT over SAS) ---------------------
prefer_dat <- function(val_dat, val_sas) dplyr::coalesce(val_dat, val_sas)

resolve_all <- function(df) {
  if (!nrow(df)) return(df)
  # track conflicts only where both sides exist and differ
  suffix <- "\\.(dat|sas)$"
  base_names <- unique(gsub(suffix, "", grep(suffix, names(df), value = TRUE)))
  out <- df
  conflict_flags <- vector("list", length(base_names)); names(conflict_flags) <- base_names
  
  for (nm in base_names) {
    d <- paste0(nm, ".dat"); s <- paste0(nm, ".sas")
    has_d <- d %in% names(out); has_s <- s %in% names(out)
    if (has_d && has_s) {
      conflict <- !is.na(out[[d]]) & !is.na(out[[s]]) & out[[d]] != out[[s]]
      conflict_flags[[nm]] <- conflict
      out[[nm]] <- prefer_dat(out[[d]], out[[s]])
      out[[d]] <- NULL; out[[s]] <- NULL
    } else if (has_d) {
      out[[nm]] <- out[[d]]; out[[d]] <- NULL
    } else if (has_s) {
      out[[nm]] <- out[[s]]; out[[s]] <- NULL
    }
  }
  
  any_conflict <- if (length(conflict_flags)) Reduce(`|`, conflict_flags) else rep(FALSE, nrow(out))
  out$merging_update_type <- ifelse(any_conflict, "Conflict within", "No change")
  out
}

joined_by_year <- joined_by_year %>% mutate(data = map(data, resolve_all))

# ---- 5) Early time normalization (SAS decimals, 24:xx:xx -> 00:xx:xx) -------
normalize_times <- function(df) {
  if (!nrow(df)) return(df)
  # normalize once on merged columns
  norm1 <- function(x) {
    x <- stringr::str_replace_all(x, "^24", "00")           # 24:.. -> 00:..
    x
  }
  has <- intersect(c("start_time","end_time"), names(df))
  df[has] <- lapply(df[has], norm1)
  df
}

joined_by_year <- joined_by_year %>% mutate(data = map(data, normalize_times))

# --------------------- 6) Bind all years once --------------------------------
combined <- joined_by_year$data %>% bind_rows() %>% clean_names()

# --------------------- 7) Error files: fish_total conflicts ------------------
# Conflicts where duplicated fish_unique_ID has differing fish_total by source
sas_total_fish_error <- combined %>%
  group_by(fish_unique_id) %>%
  filter(n() > 1) %>%
  distinct(fish_total, .keep_all = TRUE) %>%
  filter(n() > 1) %>%
  arrange(fish_unique_id) %>%
  ungroup()

if (nrow(sas_total_fish_error))
  readr::write_csv(sas_total_fish_error, file.path(error_directory, "sas_total_fish_errors.csv"))

# --------------------- 8) Duplicate collapse (by fish_unique_ID) -------------
# If still duplicated after resolver (e.g., same ID truly repeated), keep first
combined <- combined %>%
  arrange(fish_unique_id) %>%
  group_by(fish_unique_id) %>%
  slice_head(n = 1) %>%  # or a smarter rule if needed
  ungroup()

# keep a duplicate report for audit (in case we want to inspect pre-collapse)
# Note: using original combined pre-slice for this report can be done if needed

# --------------------- 9) Species/lifestage cleanup via dictionaries ---------
dict_species <- c(
  # life-stage harmonization (examples; extend as needed)
  "\\(COHO FRY\\), \\(Fry\\)" = "Fry",
  "\\(COHO SMOLT\\), \\(Smolt\\)" = "Smolt",
  "FRY, \\(Juvenile\\)" = "Fry",
  ", \\(Adult\\)" = "Adult",
  ", \\(Juvenile\\)" = "Juvenile",
  "SUB ADULTS" = "Subadult",
  "ADULTS" = "Adult",
  "ADULT" = "Adult",
  "FRY" = "Fry",
  " \\(Smolt\\)" = "Smolt"
)

combined <- combined %>%
  mutate(
    # move trailing parenthetical to species_code_comment if present
    fish_description_clean = str_extract(fish_description, "\\s\\([:graph:]+\\)$"),
    fish_description = str_trim(str_remove(fish_description, "\\s\\([:graph:]+\\)$")),
    fish_description = if_else(species_code_comment == "KOKANEE", "Kokanee", fish_description),
    fish_name = as.character(fish_description_clean)
  ) %>%
  unite(species_code_comment, species_code_comment, fish_name, sep = ",", na.rm = TRUE) %>%
  select(-fish_description_clean) %>%
  mutate(species_code_comment = str_replace_all(species_code_comment, dict_species))

# --------------------- 10) Lookups (species, lakes, preservative, weight) ----
lut_species <- safe_read_csv("./TRAWL_BIOSAMPLE/00_raw_data/03_AA_look_up_tables/AA_fish_scientific_name_lookup.csv")
lut_lakes   <- safe_read_csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/lake_codes.csv")
lut_pres    <- safe_read_csv("./TRAWL_BIOSAMPLE/00_raw_data/04_YS_look_up_tables/preservative_code_lookup_table.csv")
lut_weight  <- safe_read_csv("./TRAWL_BIOSAMPLE/00_raw_data/03_AA_look_up_tables/AA_calc_std_weight.csv")

combined <- combined %>%
  rename(species_info_code = species_code) %>%
  left_join(lut_species, by = "species_info_code") %>%
  select(-lake_name) %>%
  left_join(lut_lakes, by = "lake_code") %>%
  mutate(age = if_else(is.na(age) & !is.na(age_class), age_class, age),
         species_code_comment = if_else(species_code_comment == "Juvenile", life_stage, species_code_comment)) %>%
  select(-species_common_name, -age_class, -life_stage) %>%
  # preservative lookups later, after ethanol fix
  identity()

# Save + remove rows with no species (audit trail)
no_species <- combined %>% filter(is.na(fish_description))
if (nrow(no_species))
  readr::write_csv(no_species, file.path(error_directory, "no_species_record_rows.csv"))
combined <- combined %>% filter(!is.na(fish_description))

# --------------------- 11) Duration hygiene (vectorized) ---------------------
combined <- combined %>%
  mutate(
    duration_mi_clean = str_extract(duration_mi, "^\\d+"),
    duration_mi = as.integer(duration_mi_clean)
  ) %>% select(-duration_mi_clean)

# Flag outliers / garbles
duration_mi_errors <- combined %>%
  filter(!is.na(duration_mi), duration_mi >= 60 | duration_mi %in% c(365,535))
if (nrow(duration_mi_errors))
  readr::write_csv(duration_mi_errors, file.path(error_directory, "duration_mi_errors.csv"))

# Fix 365/535 -> NA and generic 99/999 placeholders across a few fields
combined <- combined %>%
  mutate(duration_mi = if_else(duration_mi %in% c(365L, 535L), NA_integer_, duration_mi)) %>%
  mutate(across(c(duration_mi, processor, depth_m, scale, trawl_number),
                ~ str_replace_all(as.character(.x), "999", NA_character_))) %>%
  mutate(across(c(duration_mi, processor, depth_m, trawl_number),
                ~ str_replace_all(as.character(.x), "99", NA_character_))) %>%
  mutate(duration_mi = as.integer(duration_mi))

# Compute calculated duration (minutes) from start/end where possible
to_seconds <- function(hms) {
  ifelse(is.na(hms), NA_real_,
         as.numeric(hms %>% lubridate::hms()))
}

combined <- combined %>%
  # treat "00" as 24 for span calc, then revert text back later
  mutate(across(c(start_time, end_time), ~ str_replace_all(.x, "^00", "24")),
         start_t = to_seconds(start_time),
         end_t   = to_seconds(end_time)) %>%
  mutate(across(c(start_time, end_time), ~ str_replace_all(.x, "^24", "00"))) %>%
  mutate(
    calc_duration_time =
      dplyr::case_when(
        !is.na(start_t) & !is.na(end_t) & end_t < start_t ~ (end_t + 24*60*60 - start_t) / 60,
        !is.na(start_t) & !is.na(end_t)                   ~ (end_t - start_t) / 60,
        TRUE                                              ~ NA_real_
      ),
    calc_duration_time = round(calc_duration_time, 1)
  )

# Final duration and comment
combined <- combined %>%
  mutate(
    duration_final = case_when(
      !is.na(duration_mi)                          ~ as.numeric(duration_mi),
      is.na(duration_mi) & !is.na(calc_duration_time) ~ calc_duration_time,
      TRUE ~ NA_real_
    ),
    duration_comment = case_when(
      !is.na(duration_mi) & !is.na(calc_duration_time) &
        abs(duration_mi - calc_duration_time) < 1  ~ "matches calculated start_time and end_time",
      !is.na(duration_mi) & !is.na(calc_duration_time) &
        abs(duration_mi - calc_duration_time) >= 1 ~ "does NOT match calculated start_time and end_time, likely end_time error",
      is.na(duration_mi) & !is.na(calc_duration_time) ~ "duration not provided, calculated from start_time, end_time",
      TRUE ~ "duration could not be calculated"
    )
  )

duration_mismatch <- combined %>%
  filter(duration_comment == "does NOT match calculated start_time and end_time, likely end_time error")
if (nrow(duration_mismatch))
  readr::write_csv(duration_mismatch, file.path(error_directory, "duration_mismatch.csv"))

# Calculate missing end_time from start + duration_mi (when possible)
combined <- combined %>%
  mutate(
    calc_end_time = ifelse(!is.na(start_t) & !is.na(duration_mi),
                           start_t + duration_mi * 60, NA_real_),
    calc_end_time_comment = if_else(!is.na(end_time), "end_time provided",
                                    "end_time calculated from start_time and duration_mi")
  ) %>%
  select(-start_t, -end_t)

seconds_to_hms <- function(x) {
  ifelse(is.na(x), NA_character_,
         sprintf("%02d:%02d:%02d", x %/% 3600, (x %% 3600) %/% 60, x %% 60))
}

combined <- combined %>%
  mutate(calc_end_time = seconds_to_hms(calc_end_time),
         calc_end_time = if_else(calc_end_time == "NA:NA:NA", NA_character_, calc_end_time),
         calc_end_time = str_replace_all(calc_end_time, "^24", "00"),
         calc_end_time = str_replace_all(calc_end_time, "^25", "01"))

# --------------------- 12) Preservative fixes & lookups ----------------------
# ethanol in trawl_location/trawl_comment → set description & code=5
combined <- combined %>%
  mutate(trawl_comment = coalesce(trawl_location, trawl_location),  # alias if needed
         ethanol_hit = grepl("ethanol", trawl_comment, ignore.case = TRUE),
         preservative_description = if_else(ethanol_hit, "95% Ethanol", preservative_description),
         preservative_code = if_else(ethanol_hit, 5L, suppressWarnings(as.integer(preservative_code))),
         preservative_code_comment = if_else(ethanol_hit,
                                             if_else(is.na(preservative_code_comment) | preservative_code_comment == "",
                                                     "Preservative description provided in trawl_comment column",
                                                     paste0(preservative_code_comment, "; Preservative description provided in trawl_comment column")),
                                             preservative_code_comment)) %>%
  select(-ethanol_hit)

# Correct common typos in preservative_code and squish spaces
combined <- combined %>%
  mutate(preservative_code = as.character(preservative_code)) %>%
  mutate(preservative_code = case_when(
    preservative_code %in% c("98","9") ~ NA_character_,
    preservative_code == "971" ~ "97",
    preservative_code == "270" ~ "2",
    preservative_code == "350" ~ "3",
    preservative_code == "11"  ~ "1",
    preservative_code == "35"  ~ "3",
    TRUE ~ preservative_code
  ),
  preservative_code = suppressWarnings(as.integer(preservative_code)),
  preservative_code_comment = stringr::str_squish(preservative_code_comment))

# fill preservative descriptions via lookup
if (!is.null(lut_pres))
  combined <- combined %>% rows_patch(lut_pres, by = "preservative_code", unmatched = "ignore")

# --------------------- 13) Standardized weight QA ----------------------------
if (!is.null(lut_weight))
  combined <- combined %>%
  left_join(lut_weight, by = c("preservative_code","weight_conversion_formula")) %>%
  mutate(calc_std_weight_g = fish_weight_g / calc_value,
         calc_std_weight_g = round(calc_std_weight_g, 2)) %>%
  mutate(std_weight_g_comment = case_when(
    !is.na(standardized_weight_g) & !is.na(calc_std_weight_g) &
      abs(standardized_weight_g - calc_std_weight_g) < 1 ~ "matches calculated standardized weight",
    !is.na(standardized_weight_g) & !is.na(calc_std_weight_g) &
      abs(standardized_weight_g - calc_std_weight_g) >= 1 ~ "does NOT match calculated standardized weight",
    is.na(standardized_weight_g) & !is.na(calc_std_weight_g) ~ "standardized_weight_g calculated from standardized weight formula",
    TRUE ~ "standardized weight could not be calculated"
  ))

std_weight_errors <- combined %>% filter(std_weight_g_comment == "does NOT match calculated standardized weight")
if (nrow(std_weight_errors))
  readr::write_csv(std_weight_errors, file.path(error_directory, "std_weight_errors.csv"))

# --------------------- 14) Depth hygiene ------------------------------------
combined <- combined %>%
  mutate(depth_m = case_when(depth_m == "98.109375" ~ NA_character_,
                             depth_m == "14.83203125" ~ NA_character_,
                             TRUE ~ as.character(depth_m))) %>%
  mutate(depth_m = suppressWarnings(as.numeric(depth_m))) %>%
  mutate(depth_m_flag = case_when(depth_m == 188 ~ "Likely typo, corrected from 188 m to 18 m",
                                  depth_m == 200 ~ "Likely typo, corrected from 200 m to 20 m",
                                  TRUE ~ NA_character_),
         depth_m = case_when(depth_m == 188 ~ 18,
                             depth_m == 200 ~ 20,
                             TRUE ~ depth_m)) %>%
  unite("depth_m_comments", depth_m_flag, depth_m_comment, sep = ", ", na.rm = TRUE)

# --------------------- 15) Gear extraction (seine) ---------------------------
combined <- combined %>%
  mutate(seine_hit = grepl("sein", trawl_comment, ignore.case = TRUE),
         gear_type  = if_else(seine_hit, trawl_comment, NA_character_),
         gear_type  = str_trim(str_remove(gear_type, "NO TIME OR DURATION TIME.NO DEPTH."))) %>%
  select(-seine_hit)

# --------------------- 16) Comments: data_issues + general_comments ----------
collapse_nonempty <- function(...) {
  x <- c(...)
  x <- x[!is.na(x) & x != ""]
  if (length(x)) paste(x, collapse = "; ") else NA_character_
}

combined <- combined %>%
  mutate(
    data_issues = collapse_nonempty(
      ifelse(!is.na(trawl_comment) & trawl_comment != "", paste0("trawl_comment: ", trawl_comment), NA),
      ifelse(duration_comment == "does NOT match calculated start_time and end_time, likely end_time error",
             paste0("duration_comment: ", duration_comment), NA),
      ifelse(std_weight_g_comment == "does NOT match calculated standardized weight",
             paste0("std_weight_g_comment: ", std_weight_g_comment), NA),
      ifelse(!is.na(depth_m_comments) & depth_m_comments != "", paste0("depth_m_comments: ", depth_m_comments), NA)
    ),
    general_comments = collapse_nonempty(
      ifelse(!is.na(merging_update_type) & merging_update_type != "", paste0("merging_update_type: ", merging_update_type), NA),
      ifelse(duration_comment != "does NOT match calculated start_time and end_time, likely end_time error", paste0("duration_comment: ", duration_comment), NA),
      ifelse(!is.na(calc_end_time_comment) & calc_end_time_comment != "", paste0("calc_end_time_comment: ", calc_end_time_comment), NA)
    )
  )

# --------------------- 17) Final schema & export -----------------------------
time_stamp <- format(Sys.time(), "%d-%b-%Y %H:%M")

final_dataframe <- combined %>%
  mutate(time_stamp = time_stamp) %>%
  unite("program_notes", program_notes, time_stamp, sep = ". Data rescued: ", na.rm = TRUE) %>%
  transmute(
    ats_year, lake_code, lake_name, trawl_date, trawl_month,
    trawl_number = suppressWarnings(as.integer(trawl_number)),
    sample_type, gear_type,
    depth_m, start_time, end_time, calc_end_time,
    duration_minutes = duration_mi, calc_duration_time,
    fish_id, species_code = species_info_code, species_common_name = fish_description,
    life_stage = species_code_comment,
    fish_length_mm, fish_weight_g, standardized_weight_g, calc_std_weight_g,
    preservative_code, preservative_description, weight_conversion_formula,
    age_class = age, aging_technique, aging_technique_name,
    scale, scale_book, scale_book_letter,
    general_comments, data_issues,
    source_files = collapse_nonempty(source_file, source_file_sas, source_file_dat),
    source_line,
    trawl_unique_ID = trawl_unique_id,
    fish_unique_ID  = fish_unique_id,
    genus_name, species_name,
    lake_latitude, lake_longitude,
    processor, process_date,
    program_notes
  ) %>%
  arrange(ats_year, lake_name, trawl_date, trawl_number, fish_id)

# Export mid-combined (audit) and final
readr::write_csv(combined,       file.path(working_directory, "combined_inprogress_df_trawl.csv"))
readr::write_csv(final_dataframe, file.path(final_directory,   "Trawl_data_FINAL_1977-1999.csv"))

message("✅ Done. Final: ", normalizePath(file.path(final_directory, "Trawl_data_FINAL_1977-1999.csv")))