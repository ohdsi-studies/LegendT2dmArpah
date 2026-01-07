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
final_tabs <- list()

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
  
  ci_path <- file.path(db_path, "CohortIncidenceModule")
  if (!dir.exists(ci_path)) {
    warning(paste("CohortIncidenceModule not found for", db, "- skipping"))
    next
  }
  
  # Load data for this database
  inc_file <- file.path(ci_path, "ci_incidence_summary.csv")
  tdef_file <- file.path(ci_path, "ci_target_def.csv")
  odef_file <- file.path(ci_path, "ci_outcome_def.csv")
  
  if (!file.exists(inc_file) || !file.exists(tdef_file) || !file.exists(odef_file)) {
    warning(paste("Required files not found for", db, "- skipping"))
    next
  }
  
  # Load incidence summary
  inc <- tryCatch({
    read_csv(inc_file, show_col_types = FALSE)
  }, error = function(e) {
    warning(paste("Error reading ci_incidence_summary.csv for", db, ":", e$message))
    return(NULL)
  })
  
  if (is.null(inc) || nrow(inc) == 0) {
    warning(paste("No incidence data for", db, "- skipping"))
    next
  }
  
  # Load target definition
  tdef <- tryCatch({
    read_csv(tdef_file, show_col_types = FALSE)
  }, error = function(e) {
    warning(paste("Error reading ci_target_def.csv for", db, ":", e$message))
    return(NULL)
  })
  
  if (is.null(tdef) || nrow(tdef) == 0) {
    warning(paste("No target definitions for", db, "- skipping"))
    next
  }
  
  # Load outcome definition
  odef <- tryCatch({
    read_csv(odef_file, show_col_types = FALSE)
  }, error = function(e) {
    warning(paste("Error reading ci_outcome_def.csv for", db, ":", e$message))
    return(NULL)
  })
  
  if (is.null(odef) || nrow(odef) == 0) {
    warning(paste("No outcome definitions for", db, "- skipping"))
    next
  }
  
  # 2) Ensure join keys have matching types (safe choice: character)
  inc <- inc %>% mutate(
    TARGET_COHORT_DEFINITION_ID = as.character(TARGET_COHORT_DEFINITION_ID),
    OUTCOME_ID = as.character(OUTCOME_ID)
  )
  
  tdef <- tdef %>% mutate(
    TARGET_COHORT_DEFINITION_ID = as.character(TARGET_COHORT_DEFINITION_ID)
  )
  
  odef <- odef %>% mutate(
    OUTCOME_ID = as.character(OUTCOME_ID)
  )
  
  # 3) Keep only needed mapping columns (and de-dup just in case)
  tdef_small <- tdef %>%
    select(TARGET_COHORT_DEFINITION_ID, TARGET_NAME) %>%
    distinct()
  
  odef_small <- odef %>%
    select(OUTCOME_ID, OUTCOME_NAME) %>%
    distinct()
  
  # 4) Join
  inc_labeled <- inc %>%
    left_join(tdef_small, by = "TARGET_COHORT_DEFINITION_ID") %>%
    left_join(odef_small, by = "OUTCOME_ID")
  
  # 5) Diagnostics: what didn't map?
  missing_targets <- inc_labeled %>%
    filter(is.na(TARGET_NAME)) %>%
    distinct(TARGET_COHORT_DEFINITION_ID) %>%
    arrange(TARGET_COHORT_DEFINITION_ID)
  
  missing_outcomes <- inc_labeled %>%
    filter(is.na(OUTCOME_NAME)) %>%
    distinct(OUTCOME_ID) %>%
    arrange(OUTCOME_ID)
  
  if (nrow(missing_targets) > 0) {
    cat("  Warning: Unmapped target IDs for", db, ":\n")
    print(missing_targets)
  }
  
  if (nrow(missing_outcomes) > 0) {
    cat("  Warning: Unmapped outcome IDs for", db, ":\n")
    print(missing_outcomes)
  }
  
  # 6) Filter data
  inc_filtered <- inc_labeled %>% 
    filter(SUBGROUP_ID == 0,
           is.na(AGE_GROUP_ID),
           is.na(GENDER_ID),
           is.na(START_YEAR),
           TARGET_COHORT_DEFINITION_ID %in% order_vec)
  
  # 6.5) Check for duplicated target x outcome rows (after filtering)
  cat("\n  Checking for duplicated TARGET_COHORT_DEFINITION_ID × OUTCOME_ID pairs (after filtering)...\n")
  
  dup_check <- inc_filtered %>%
    count(TARGET_COHORT_DEFINITION_ID, OUTCOME_ID) %>%
    filter(n > 1)
  
  if (nrow(dup_check) > 0) {
    cat("  WARNING: Duplicate TARGET_COHORT_DEFINITION_ID × OUTCOME_ID pairs detected in", db, ":\n")
    for (i in 1:nrow(dup_check)) {
      cat("    Target=", dup_check$TARGET_COHORT_DEFINITION_ID[i], 
          ", Outcome=", dup_check$OUTCOME_ID[i], 
          ", Count=", dup_check$n[i], "\n", sep = "")
    }
    warning(
      "Duplicate TARGET_COHORT_DEFINITION_ID × OUTCOME_ID pairs detected in database: ", db, "\n",
      "See details above for specific target × outcome combinations."
    )
  } else {
    cat("  ✓ No duplicates found\n")
  }
  
  # 7) Create final table for this database
  final_tab_db <- inc_filtered %>% 
    mutate( 
      # applying ordering
      TARGET_COHORT_DEFINITION_ID = factor(TARGET_COHORT_DEFINITION_ID, levels = order_vec),
      # get rid of long names
      TARGET_NAME = TARGET_NAME %>%
        str_remove("\\s*-\\s*in cohorts:.*$") %>%   # drop suffix
        str_squish(),                              # normalize spaces
      # add database name
      database_name = db
    ) %>% 
    select(TARGET_COHORT_DEFINITION_ID, TARGET_NAME,
           OUTCOME_ID, OUTCOME_NAME, PERSONS_AT_RISK, PERSON_DAYS, OUTCOMES,
           INCIDENCE_PROPORTION_P100P, INCIDENCE_RATE_P100PY, database_name) %>%
    arrange(TARGET_COHORT_DEFINITION_ID, as.integer(OUTCOME_ID))
  
  # Save as separate object with database name
  # Create a valid R object name (replace special characters)
  db_obj_name <- gsub("[^A-Za-z0-9_]", "_", db)
  obj_name <- paste0("final_tab_", db_obj_name)
  assign(obj_name, final_tab_db, envir = .GlobalEnv)
  
  # Also store in list
  final_tabs[[db]] <- final_tab_db
  
  cat("  Created", obj_name, "with", nrow(final_tab_db), "rows\n")
}

cat("\n========================================\n")
cat("Processing complete!\n")
cat("Individual tables saved as: final_tab_<database_name>\n")
cat("All tables also stored in list: final_tabs\n")
cat("========================================\n")

# 7) Optional: Create combined table across all databases
final_tab_combined <- bind_rows(final_tabs)
# write_csv(final_tab_combined, "ci_incidence_summary_labeled_combined.csv")

final_tab_outcomes_only = final_tab_combined %>% select(TARGET_NAME, OUTCOME_NAME,
                                                        OUTCOMES, INCIDENCE_PROPORTION_P100P,
                                                        database_name)

# 8) Optional: Save individual tables to CSV
# for (db in names(final_tabs)) {
#   db_obj_name <- gsub("[^A-Za-z0-9_]", "_", db)
#   write_csv(final_tabs[[db]], paste0("ci_incidence_summary_labeled_", db_obj_name, ".csv"))
# }

# 9) Pivot outcomes data wider by database
# Create formatted outcome count (incidence proportion) column, then pivot
if (exists("final_tab_combined") && exists("final_tab_outcomes_only")) {
  cat("\n========================================\n")
  cat("Creating pivoted outcomes table...\n")
  cat("========================================\n")
  
  # Create formatted column combining OUTCOMES and INCIDENCE_PROPORTION_P100P
  table_4_final <- final_tab_outcomes_only %>%
    mutate(
      outcome_formatted = case_when(
        is.na(OUTCOMES) & is.na(INCIDENCE_PROPORTION_P100P) ~ NA_character_,
        is.na(OUTCOMES) ~ paste0("NA (", round(INCIDENCE_PROPORTION_P100P, 2), ")"),
        is.na(INCIDENCE_PROPORTION_P100P) ~ paste0(OUTCOMES, " (NA)"),
        TRUE ~ paste0(OUTCOMES, " (", round(INCIDENCE_PROPORTION_P100P, 2), ")")
      )
    ) %>%
    select(TARGET_NAME, OUTCOME_NAME, database_name, outcome_formatted) %>%
    pivot_wider(
      id_cols = c(TARGET_NAME, OUTCOME_NAME),
      names_from = database_name,
      values_from = outcome_formatted,
      values_fill = NA
    )
  
  cat("Created final_tab_outcomes_pivoted with", nrow(final_tab_outcomes_pivoted), "rows\n")
  cat("Columns:", paste(names(final_tab_outcomes_pivoted), collapse = ", "), "\n")
}

write.csv(table_4_final, "table_4_final.csv", row.names = F)
