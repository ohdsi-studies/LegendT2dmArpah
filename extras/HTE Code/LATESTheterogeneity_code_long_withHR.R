# heterogeneity code long
# include subgroup-wide HRs

library(dplyr)
library(tidyr)
library(rlang)
library(purrr)

##################
# load in data and names
##################

site = "jmdc" #cuimc, mdcd, mdcr, washu, jmdc, starr

df = read.csv(paste0("results/cm_result_", site, ".csv")) 
subgroup_names = read.csv("subgroup_names.csv")
cohort_names = read.csv("cohort_names.csv") # includes outcome names
diagnostics = read.csv(paste0("results/cm_diagnostics_summary_", site, ".csv")) # need to make sure we only use rows that pass

##################
# filter rows 
##################

# merge diagnostics column into df
diag_sel <- diagnostics %>% # select relevant columns
  select(
    analysis_id, target_id, comparator_id, outcome_id, database_id,
    balance_diagnostic,
    mdrr_diagnostic,
    equipoise_diagnostic
  )

# left‑join those diagnostics onto your df by the shared key columns
df <- df %>%
  left_join(
    diag_sel,
    by = c("analysis_id", "target_id", "comparator_id", "outcome_id", "database_id")
  )

outcomes_only = df %>% 
  filter(outcome_id >= 1000,       # only “true” outcomes
         !is.na(rr),               # drop any missing IRRs
         mdrr_diagnostic     == "PASS",  # (1) MDRR diagnostic passed
         balance_diagnostic  == "PASS",  # (2) covariate balance passed
         equipoise_diagnostic == "PASS")  # (3) equipoise passed


##################
# helper functions
##################

# find subgroup id
find_subgroup_id = function(id){
  subgroup_id = as.integer(id %% 1000)
  return(subgroup_id)
}

# this finds the cohort IDs (no subgroup) 
find_parent_id = function(id){
  subgroup_id = as.integer(id %% 1000)
  parent_id = (id - subgroup_id) / 1000
  return(parent_id)
}

# some of the cohort IDs have subgroups of subgroups -> use this wrapper 
# (i.e. need to run cohort ID function twice)
get_cohort_id <- function(id){
  # first peel off one layer
  p1 <- find_parent_id(id)
  # if id < 20,000,000 use just one peel; otherwise peel twice
  ifelse(
    id < 2e7,
    p1,
    find_parent_id(p1)
  )
}

# find the combined calibrated SE
comb_SE = function(x, y){
  return(sqrt(x^2 + y^2))
}

# find p value
find_pval = function(est, se){
  pval = 2*pnorm(q=abs(est/se), lower.tail=FALSE)
  return(pval)
}

##################
# process dataframe
##################

outcomes_only$cohortid_target = get_cohort_id(outcomes_only$target_id)
outcomes_only$cohortid_comparator = get_cohort_id(outcomes_only$comparator_id)
outcomes_only$subgroup_id = find_subgroup_id(outcomes_only$target_id)

# add in readable names
outcomes_only <- outcomes_only %>%
  # join in target names
  left_join(
    cohort_names %>% rename(target_name = cohort_name),
    by = c("cohortid_target"    = "cohort_definition_id")
  ) %>%
  # join in comparator names
  left_join(
    cohort_names %>% rename(comparator_name = cohort_name),
    by = c("cohortid_comparator" = "cohort_definition_id")
  )  %>%
  # join outcome names
  left_join(
    cohort_names %>% rename(outcome_name = cohort_name),
    by = c("outcome_id" = "cohort_definition_id")
  )

##################
# find our statistic
##################

# 1) define comparisons in one place:
comparisons <- list(
  diabetes_severe    = c(grp1 = 31, grp2 = 51), #31 = yes
  masld              = c(grp1 = 32, grp2 = 52),
  renal              = c(grp1 = 32, grp2 = 54), # 32= renal no HD, 54 = any renal
  renalHD            = c(grp1 = 35, grp2 = 54), # 32= renal + HD, 54 = any renal
  obesity            = c(grp1 = 36, grp2 = 56), 
  dka                = c(grp1 = 37, grp2 = 57),
  diabetic_retinopathy = c(grp1 = 38, grp2 = 58),
  hld                = c(grp1 = 39, grp2 = 59),
  htn                = c(grp1 = 60, grp2 = 40),
  age_low_high       = c(grp1 = 71, grp2 = 73),
  age_low_middle     = c(grp1 = 71, grp2 = 72),
  age_high_middle    = c(grp1 = 73, grp2 = 72),
  sex                = c(grp1 = 91, grp2 = 92) #91 = male
)


# 2a) build a named list of “_est” expressions
exprs_est <- imap(comparisons, function(ids, nm) {
  expr(
    calibrated_log_rr[subgroup_id == !!ids["grp1"]][1] -
      calibrated_log_rr[subgroup_id == !!ids["grp2"]][1]
  )
}) %>% 
  set_names(paste0(names(comparisons), "_est"))

# 2b) build a named list of “_se” expressions
exprs_se <- imap(comparisons, function(ids, nm) {
  expr(
    comb_SE(
      calibrated_se_log_rr[subgroup_id == !!ids["grp1"]][1],
      calibrated_se_log_rr[subgroup_id == !!ids["grp2"]][1]
    )
  )
}) %>% 
  set_names(paste0(names(comparisons), "_se"))

## <<< changes here: 2c) build the RR_grp1 and RR_grp2 expressions
exprs_RR_grp1 <- imap(comparisons, function(ids, nm) {
  # grab the calibrated_log_rr for subgroup 1
  expr(calibrated_log_rr[subgroup_id == !!ids["grp1"]][1])
}) %>% set_names(paste0(names(comparisons), "_RR_grp1"))

exprs_RR_grp2 <- imap(comparisons, function(ids, nm) {
  # grab the calibrated_log_rr for subgroup 2
  expr(calibrated_log_rr[subgroup_id == !!ids["grp2"]][1])
}) %>% set_names(paste0(names(comparisons), "_RR_grp2"))

# 2e) build the individual SE expressions for grp1 & grp2
exprs_RR_grp1_se <- imap(comparisons, function(ids, nm) {
  expr(calibrated_se_log_rr[subgroup_id == !!ids["grp1"]][1])
}) %>% set_names(paste0(names(comparisons), "_SE_grp1"))

exprs_RR_grp2_se <- imap(comparisons, function(ids, nm) {
  expr(calibrated_se_log_rr[subgroup_id == !!ids["grp2"]][1])
}) %>% set_names(paste0(names(comparisons), "_SE_grp2"))

# 2f) combine everything
exprs_all <- c(
  exprs_est, exprs_se,
  exprs_RR_grp1, exprs_RR_grp2,
  exprs_RR_grp1_se, exprs_RR_grp2_se
)

# 3) Splice them into summarise():
diffs_summary <- outcomes_only %>%
  group_by(database_id, cohortid_target, cohortid_comparator, outcome_id, target_name, comparator_name, outcome_name) %>%
  summarise(
    !!!exprs_all,
    .groups = "drop"
  )

# 4) pivot long
diffs_long <- diffs_summary %>%
  pivot_longer(
    cols = tidyselect::matches("_(est|se|RR_grp1|RR_grp2|SE_grp1|SE_grp2)$"),
    names_to = c("subgroup", ".value"),
    names_pattern = "(.*)_(est|se|RR_grp1|RR_grp2|SE_grp1|SE_grp2)$"
  )


# sanity check
diffs_long %>% glimpse()

# 5) find p value
diffs_long$pval = find_pval(diffs_long$est, diffs_long$se)

# 6) filter out NAs
diffs_long = diffs_long %>% filter(!is.na(se))

# save

# file naming schema

# build your new filename
out_fn <- paste0("hte_results_withHR/", site, "_heterogeneity_result_long_withHR.csv")

# write it
write.csv(diffs_long, out_fn, row.names = FALSE)
