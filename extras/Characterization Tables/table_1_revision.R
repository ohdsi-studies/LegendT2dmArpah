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

subgroups = as.character(order_tbl$subgroup_order_last2digits[1:14])
subgroup_names = order_tbl$subgroup_name_1[1:14]

# Create mapping from last 2 digits to subgroup name
subgroup_mapping <- data.frame(
  last_2_digits = as.character(order_tbl$subgroup_order_last2digits[1:14]),
  subgroup_name = order_tbl$subgroup_name_1[1:14],
  stringsAsFactors = FALSE
)

# Initialize list to store final tables for each database
final_cohort_count_tabs <- list()

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
  
  # Label cohort_counts table
  cohort_counts_labeled <- cohort_counts |> 
    filter(cohort_type == "Target") %>%
    left_join(
      cohort_defn %>% select(target_cohort_id = cohort_definition_id, cohort_name),
      by = "target_cohort_id"
    ) %>%
    mutate(database_name = db)
  
  # Get 7-digit IDs from order_vec (for matching 10-character subgroups)
  seven_digit_ids <- order_vec[nchar(order_vec) == 7]
  
  # Filter cohort_counts_labeled
  final_cohort_count_db <- cohort_counts_labeled %>% 
    filter(
      # Keep ALL IDs that are in order_vec (including 1001, 7-digit IDs, etc.)
      target_cohort_id %in% order_vec |
      # OR keep 10-character IDs where:
      # - First 7 characters match a 7-digit ID from order_vec
      # - AND last 2 digits are in subgroups
      (nchar(target_cohort_id) == 10 & 
       substr(target_cohort_id, 1, 7) %in% seven_digit_ids &
       substr(target_cohort_id, 9, 10) %in% subgroups)
    ) %>% 
    mutate( 
      # Extract last 2 digits for ordering 10-character IDs
      last_2_digits = if_else(nchar(target_cohort_id) == 10, 
                             substr(target_cohort_id, 9, 10), 
                             NA_character_),
      # Extract first 7 digits for 10-character IDs to find parent
      parent_7_digit = if_else(nchar(target_cohort_id) == 10,
                              substr(target_cohort_id, 1, 7),
                              NA_character_),
      # Create ordering: 
      # - For 7-digit IDs in order_vec: use their position in order_vec
      # - For other IDs in order_vec (like 1001): use their position in order_vec
      # - For 10-character IDs: use parent's position + small offset (0.1-0.99) based on subgroup order
      #   This ensures 10-digit children appear immediately after their 7-digit parent
      ordering_key = if_else(
        target_cohort_id %in% order_vec,
        match(target_cohort_id, order_vec),
        # For 10-character: parent position + fractional offset for subgroup ordering
        match(parent_7_digit, order_vec) + 
          (match(last_2_digits, subgroups) / 1000)
      ),
      # For 10-digit cohorts, replace cohort_name with subgroup name
      # For non-10-digit cohorts, clean the cohort_name
      cohort_name = if_else(
        nchar(target_cohort_id) == 10,
        # Map last 2 digits to subgroup name
        subgroup_mapping$subgroup_name[match(last_2_digits, subgroup_mapping$last_2_digits)],
        # For non-10-digit IDs: get rid of long names
        cohort_name %>%
          str_remove("\\s*-\\s*in cohorts:.*$") %>%   # drop suffix
          str_squish()                              # normalize spaces
      )
    ) %>% 
    arrange(ordering_key) %>%
    select(target_cohort_id, cohort_name, person_count, database_id, database_name)
  
  # Save as separate object with database name
  # Create a valid R object name (replace special characters)
  db_obj_name <- gsub("[^A-Za-z0-9_]", "_", db)
  obj_name <- paste0("final_cohort_count_tab_", db_obj_name)
  assign(obj_name, final_cohort_count_db, envir = .GlobalEnv)
  
  # Also store in list
  final_cohort_count_tabs[[db]] <- final_cohort_count_db
  
  cat("  Created", obj_name, "with", nrow(final_cohort_count_db), "rows\n")
}

cat("\n========================================\n")
cat("Processing complete!\n")
cat("Individual tables saved as: final_cohort_count_tab_<database_name>\n")
cat("All tables also stored in list: final_cohort_count_tabs\n")
cat("========================================\n")

# Optional: Create combined table across all databases
final_cohort_count_tab_combined <- bind_rows(final_cohort_count_tabs)

# Format and pivot wider: Create person_count columns per database
if (exists("final_cohort_count_tab_combined") && nrow(final_cohort_count_tab_combined) > 0) {
  cat("\n========================================\n")
  cat("Creating pivoted cohort counts table...\n")
  cat("========================================\n")
  
  # Pivot wider by database
  table_1_final <- final_cohort_count_tab_combined %>%
    select(target_cohort_id, cohort_name, database_name, person_count) %>%
    pivot_wider(
      id_cols = c(target_cohort_id, cohort_name),
      names_from = database_name,
      values_from = person_count,
      values_fill = NA
    ) %>%
    # Apply ordering: same logic as individual database processing
    mutate(
      # Extract last 2 digits for ordering 10-character IDs
      last_2_digits = if_else(nchar(target_cohort_id) == 10, 
                             substr(target_cohort_id, 9, 10), 
                             NA_character_),
      # Extract first 7 digits for 10-character IDs to find parent
      parent_7_digit = if_else(nchar(target_cohort_id) == 10,
                              substr(target_cohort_id, 1, 7),
                              NA_character_),
      # Create ordering key (same as individual processing)
      ordering_key = if_else(
        target_cohort_id %in% order_vec,
        match(target_cohort_id, order_vec),
        # For 10-character: parent position + fractional offset for subgroup ordering
        match(parent_7_digit, order_vec) + 
          (match(last_2_digits, subgroups) / 1000)
      ),
      # For 10-digit cohorts, replace cohort_name with subgroup name
      cohort_name = if_else(
        nchar(target_cohort_id) == 10,
        # Map last 2 digits to subgroup name
        subgroup_mapping$subgroup_name[match(last_2_digits, subgroup_mapping$last_2_digits)],
        cohort_name  # Keep original name for non-10-digit IDs
      )
    ) %>%
    arrange(ordering_key) %>%
    select(-last_2_digits, -parent_7_digit, -ordering_key)  # Remove helper columns
  
  cat("Created table_1_final with", nrow(table_1_final), "rows\n")
  cat("Columns:", paste(names(table_1_final), collapse = ", "), "\n")
}

write.csv(table_1_final, "table_1_final.csv", row.names = F)
