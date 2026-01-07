library(dplyr)
library(readr)
library(stringr)

# 1) Define results directory and get list of databases
results_dir <- "results"
databases <- list.dirs(results_dir, full.names = FALSE, recursive = FALSE)
databases <- databases[!grepl("\\.zip$", databases)]

cat("Found databases:", paste(databases, collapse = ", "), "\n")

# Define covariate_analysis_id values to keep
covariate_analysis_id_to_keep = c(1, 3, 4, 5)

# Read ordering table (needed for all databases)
order_tbl <- read_csv("table_order.csv", show_col_types = FALSE) %>%
  mutate(target_cohort_id = as.character(target_cohort_id))

order_vec <- order_tbl %>%
  mutate(target_cohort_id = as.character(target_cohort_id)) %>%
  pull(target_cohort_id)

# Initialize list to store final tables for each database
final_covariate_tabs <- list()

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
  
  # Load covariates
  covariates_file <- file.path(char_path, "c_covariates.csv")
  if (!file.exists(covariates_file)) {
    warning(paste("c_covariates.csv not found for", db, "- skipping"))
    next
  }
  
  covariates <- tryCatch({
    read.csv(covariates_file)
  }, error = function(e) {
    warning(paste("Error reading c_covariates.csv for", db, ":", e$message))
    return(NULL)
  })
  
  if (is.null(covariates) || nrow(covariates) == 0) {
    warning(paste("No covariates data for", db, "- skipping"))
    next
  }
  
  # Load covariate reference file
  covariates_ref_file <- file.path(char_path, "c_covariate_ref.csv")
  if (!file.exists(covariates_ref_file)) {
    warning(paste("c_covariate_ref.csv not found for", db, "- skipping"))
    next
  }
  
  covariates_ref <- tryCatch({
    read.csv(covariates_ref_file)
  }, error = function(e) {
    warning(paste("Error reading c_covariate_ref.csv for", db, ":", e$message))
    return(NULL)
  })
  
  if (is.null(covariates_ref) || nrow(covariates_ref) == 0) {
    warning(paste("No covariate reference data for", db, "- skipping"))
    next
  }
  
  # Filter covariates_ref to keep only desired covariate_analysis_id values
  covariates_ref_filtered <- covariates_ref %>%
    filter(analysis_id %in% covariate_analysis_id_to_keep) %>%
    distinct(covariate_id, covariate_name, .keep_all = TRUE)
  
  # Get the covariate_id values to keep
  covariate_ids_to_keep <- unique(covariates_ref_filtered$covariate_id)
  
  if (length(covariate_ids_to_keep) == 0) {
    warning(paste("No covariates found with covariate_analysis_id in", 
                  paste(covariate_analysis_id_to_keep, collapse = ", "), 
                  "for", db, "- skipping"))
    next
  }
  
  # Load cohort definitions (target names)
  target_name_file <- file.path(cg_path, "cg_cohort_definition.csv")
  if (!file.exists(target_name_file)) {
    warning(paste("cg_cohort_definition.csv not found for", db, "- skipping"))
    next
  }
  
  target_name <- tryCatch({
    read.csv(target_name_file)
  }, error = function(e) {
    warning(paste("Error reading cg_cohort_definition.csv for", db, ":", e$message))
    return(NULL)
  })
  
  if (is.null(target_name) || nrow(target_name) == 0) {
    warning(paste("No cohort definitions for", db, "- skipping"))
    next
  }
  
  # Get covariate name and target name matches
  covariates_labeled <- covariates %>%
    filter(covariate_id %in% covariate_ids_to_keep & cohort_type == "Target") %>%
    left_join(
      covariates_ref_filtered %>% select(covariate_id, covariate_name),
      by = "covariate_id"
    ) %>%  
    left_join(
      target_name %>% select(target_cohort_id = cohort_definition_id, cohort_name),
      by = "target_cohort_id"
    )
  
  # Filter data
  covariates_filtered <- covariates_labeled %>% 
    filter(target_cohort_id %in% order_vec)
  
  # Check for duplicated target_cohort_id × covariate_id rows (after filtering)
  cat("\n  Checking for duplicated target_cohort_id × covariate_id pairs (after filtering)...\n")
  
  dup_check <- covariates_filtered %>%
    count(target_cohort_id, covariate_id) %>%
    filter(n > 1)
  
  if (nrow(dup_check) > 0) {
    cat("  WARNING: Duplicate target_cohort_id × covariate_id pairs detected in", db, ":\n")
    for (i in 1:nrow(dup_check)) {
      cat("    Target=", dup_check$target_cohort_id[i], 
          ", Covariate=", dup_check$covariate_id[i], 
          ", Count=", dup_check$n[i], "\n", sep = "")
    }
    warning(
      "Duplicate target_cohort_id × covariate_id pairs detected in database: ", db, "\n",
      "See details above for specific target × covariate combinations."
    )
  } else {
    cat("  ✓ No duplicates found\n")
  }
  
  # Create final covariate table for this database
  final_covariate_tab_db <- covariates_filtered %>% 
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
    arrange(target_cohort_id, covariate_id)
  
  # Format average_value as percentage
  final_covariate_tab_db$average_value = paste0(round(final_covariate_tab_db$average_value, 4) * 100, "%")
  
  # Save as separate object with database name
  # Create a valid R object name (replace special characters)
  db_obj_name <- gsub("[^A-Za-z0-9_]", "_", db)
  obj_name <- paste0("final_covariate_tab_", db_obj_name)
  assign(obj_name, final_covariate_tab_db, envir = .GlobalEnv)
  
  # Also store in list
  final_covariate_tabs[[db]] <- final_covariate_tab_db
  
  cat("  Created", obj_name, "with", nrow(final_covariate_tab_db), "rows\n")
}

cat("\n========================================\n")
cat("Processing complete!\n")
cat("Individual tables saved as: final_covariate_tab_<database_name>\n")
cat("All tables also stored in list: final_covariate_tabs\n")
cat("========================================\n")

# Optional: Create combined table across all databases
final_covariate_tab_combined <- bind_rows(final_covariate_tabs)

# Format and pivot wider: Create COUNT (percentage) format and pivot by database
if (exists("final_covariate_tab_combined") && nrow(final_covariate_tab_combined) > 0) {
  cat("\n========================================\n")
  cat("Creating pivoted covariates table...\n")
  cat("========================================\n")
  
  # Create formatted column combining sum_value (count) and average_value (percentage)
  # Note: average_value is already formatted as percentage string, so we need to extract the numeric value
  # or use sum_value for count and keep average_value as is
  table_2_final <- final_covariate_tab_combined %>%
    mutate(
      # Extract numeric value from average_value percentage string if needed
      # Or use sum_value directly for count
      count_value = ifelse(is.na(sum_value), 0, sum_value),
      # average_value is already formatted as percentage string like "25.5%"
      # Create formatted column: COUNT (percentage)
      count_percentage = case_when(
        is.na(count_value) & is.na(average_value) ~ NA_character_,
        is.na(count_value) ~ paste0("NA (", average_value, ")"),
        is.na(average_value) ~ paste0(count_value, " (NA)"),
        TRUE ~ paste0(count_value, " (", average_value, ")")
      )
    ) %>%
    select(target_cohort_id, cohort_name, covariate_id, covariate_name, database_name, count_percentage) %>%
    pivot_wider(
      id_cols = c(target_cohort_id, cohort_name, covariate_id, covariate_name),
      names_from = database_name,
      values_from = count_percentage,
      values_fill = NA
    )
  
  cat("Created table_2_final with", nrow(table_2_final), "rows\n")
  cat("Columns:", paste(names(table_2_final), collapse = ", "), "\n")
}

table_2_T2DM_only = table_2_final %>% filter(target_cohort_id == 1001)
write.csv(table_2_final, "table_2_final.csv", row.names = F)

