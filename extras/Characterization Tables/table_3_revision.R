library(dplyr)
library(readr)
library(stringr)

# 1) Define results directory and get list of databases
results_dir <- "results"
databases <- list.dirs(results_dir, full.names = FALSE, recursive = FALSE)
databases <- databases[!grepl("\\.zip$", databases)]

cat("Found databases:", paste(databases, collapse = ", "), "\n")

# Read ordering table (needed for all databases)
order_tbl <- read_csv("table_order.csv", show_col_types = FALSE) %>%
  mutate(target_cohort_id = as.character(target_cohort_id))

order_vec <- order_tbl %>%
  mutate(target_cohort_id = as.character(target_cohort_id)) %>%
  pull(target_cohort_id)

# Initialize list to store final tables for each database
final_exposure_tabs <- list()

# Loop over databases to process each one separately
for (db in databases) {
  cat("\n========================================\n")
  cat("Processing database:", db, "\n")
  cat("========================================\n")
  
  # Check for strategusOutput or StrategusOutput (handle case variations)
  db_path <- file.path(results_dir, db, "strategusOutput")
  if (!dir.exists(db_path)) {
    db_path <- file.path(results_dir, db, "strategusResults")
  }
  
  if (!dir.exists(db_path)) {
    warning(paste("Directory not found for", db, "- skipping"))
    next
  }
  
  # Paths for CharacterizationModule and CohortGeneratorModule
  char_path <- file.path(db_path, "CharacterizationModule")
  cg_path <- file.path(db_path, "CohortGeneratorModule")
  
  if (!dir.exists(char_path)) {
    warning(paste("CharacterizationModule not found for", db, "- skipping"))
    next
  }
  
  if (!dir.exists(cg_path)) {
    warning(paste("CohortGeneratorModule not found for", db, "- skipping"))
    next
  }
  
  # Load cohort counts
  cohort_counts_file <- file.path(char_path, "c_cohort_counts.csv")
  if (!file.exists(cohort_counts_file)) {
    warning(paste("c_cohort_counts.csv not found for", db, "- skipping"))
    next
  }
  
  cohort_counts <- tryCatch({
    read.csv(cohort_counts_file)
  }, error = function(e) {
    warning(paste("Error reading c_cohort_counts.csv for", db, ":", e$message))
    return(NULL)
  })
  
  if (is.null(cohort_counts) || nrow(cohort_counts) == 0) {
    warning(paste("No cohort counts data for", db, "- skipping"))
    next
  }
  
  # Load cohort definitions
  cohort_defn_file <- file.path(cg_path, "cg_cohort_definition.csv")
  if (!file.exists(cohort_defn_file)) {
    warning(paste("cg_cohort_definition.csv not found for", db, "- skipping"))
    next
  }
  
  cohort_defn <- tryCatch({
    read.csv(cohort_defn_file)
  }, error = function(e) {
    warning(paste("Error reading cg_cohort_definition.csv for", db, ":", e$message))
    return(NULL)
  })
  
  if (is.null(cohort_defn) || nrow(cohort_defn) == 0) {
    warning(paste("No cohort definitions for", db, "- skipping"))
    next
  }
  
  # Filter data
  cohort_counts_filtered <- cohort_counts %>%
    filter(cohort_type == "Target")
  
  # Check for duplicated target_cohort_id × outcome_cohort_id rows (after filtering)
  cat("\n  Checking for duplicated target_cohort_id × outcome_cohort_id pairs (after filtering)...\n")
  
  dup_check <- cohort_counts_filtered %>%
    count(target_cohort_id, outcome_cohort_id) %>%
    filter(n > 1)
  
  if (nrow(dup_check) > 0) {
    cat("  WARNING: Duplicate target_cohort_id × outcome_cohort_id pairs detected in", db, ":\n")
    for (i in 1:nrow(dup_check)) {
      cat("    Target=", dup_check$target_cohort_id[i], 
          ", Outcome=", dup_check$outcome_cohort_id[i], 
          ", Count=", dup_check$n[i], "\n", sep = "")
    }
    warning(
      "Duplicate target_cohort_id × outcome_cohort_id pairs detected in database: ", db, "\n",
      "See details above for specific target × outcome combinations."
    )
  } else {
    cat("  ✓ No duplicates found\n")
  }
  
  # Label cohort_counts table -> get exposure time
  cohort_counts_labeled <- cohort_counts_filtered %>%
    left_join(
      cohort_defn %>% select(target_cohort_id = cohort_definition_id, cohort_name),
      by = "target_cohort_id"
    )
  
  # Create final table for this database
  final_exposure_tab_db <- cohort_counts_labeled %>% 
    filter(target_cohort_id %in% order_vec) %>% 
    mutate( 
      # applying ordering
      target_cohort_id = factor(target_cohort_id, levels = order_vec),
      # get rid of long names
      cohort_name = cohort_name %>%
        str_remove("\\s*-\\s*in cohorts:.*$") %>%   # drop suffix
        str_squish(),                              # normalize spaces
      # add database name
      database_name = db
    ) %>% 
    select(target_cohort_id, cohort_name,
           min_exposure_time, mean_exposure_time, max_exposure_time, database_name, database_id)
  
  # Save as separate object with database name
  # Create a valid R object name (replace special characters)
  db_obj_name <- gsub("[^A-Za-z0-9_]", "_", db)
  obj_name <- paste0("final_exposure_tab_", db_obj_name)
  assign(obj_name, final_exposure_tab_db, envir = .GlobalEnv)
  
  # Also store in list
  final_exposure_tabs[[db]] <- final_exposure_tab_db
  
  cat("  Created", obj_name, "with", nrow(final_exposure_tab_db), "rows\n")
}

cat("\n========================================\n")
cat("Processing complete!\n")
cat("Individual tables saved as: final_exposure_tab_<database_name>\n")
cat("All tables also stored in list: final_exposure_tabs\n")
cat("========================================\n")

# Optional: Create combined table across all databases
final_exposure_tab_combined <- bind_rows(final_exposure_tabs)

# Format and pivot wider: Create "mean exposure (min, max)" format and pivot by database
if (exists("final_exposure_tab_combined") && nrow(final_exposure_tab_combined) > 0) {
  cat("\n========================================\n")
  cat("Creating pivoted exposure time table...\n")
  cat("========================================\n")
  
  # Create formatted column combining mean, min, and max exposure time
  # Format: "mean (min, max)"
  table_3_final <- final_exposure_tab_combined %>%
    mutate(
      exposure_formatted = case_when(
        is.na(mean_exposure_time) & is.na(min_exposure_time) & is.na(max_exposure_time) ~ NA_character_,
        is.na(mean_exposure_time) ~ paste0("NA (", 
                                           ifelse(is.na(min_exposure_time), "NA", round(min_exposure_time, 2)), 
                                           "-", 
                                           ifelse(is.na(max_exposure_time), "NA", round(max_exposure_time, 2)), 
                                           ")"),
        is.na(min_exposure_time) & is.na(max_exposure_time) ~ paste0(round(mean_exposure_time, 2), " (NA, NA)"),
        is.na(min_exposure_time) ~ paste0(round(mean_exposure_time, 2), " (NA, ", round(max_exposure_time, 2), ")"),
        is.na(max_exposure_time) ~ paste0(round(mean_exposure_time, 2), " (", round(min_exposure_time, 2), ", NA)"),
        TRUE ~ paste0(round(mean_exposure_time, 2), " (", round(min_exposure_time, 2), "-", round(max_exposure_time, 2), ")")
      )
    ) %>%
    select(target_cohort_id, cohort_name, database_name, exposure_formatted) %>%
    pivot_wider(
      id_cols = c(target_cohort_id, cohort_name),
      names_from = database_name,
      values_from = exposure_formatted,
      values_fill = NA
    ) %>%
    # Apply ordering based on order_vec
    mutate(
      target_cohort_id = factor(target_cohort_id, levels = order_vec)
    ) %>%
    arrange(target_cohort_id)
  
  cat("Created table_3_final with", nrow(table_3_final), "rows\n")
  cat("Columns:", paste(names(table_3_final), collapse = ", "), "\n")
} 

write.csv(table_3_final, "table_3_final.csv", row.names = F)

