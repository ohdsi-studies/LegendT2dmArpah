# meta analysis

# 1. read in all files from ("hte_results_withHR/")

# 2. from EACH datasource, find a pooled estimate for each target, comparator, outcome, subgroup quad
# the current estimates are differences between log(HR)s (i.e. if the subgroup name is obesity, 
# then it is the DIFFERENCE of log(HR of a target-comparator-outcome triad, with subgroup = NO obesity) 
# and log(HR of a target-comparator-outcome triad, with subgroup = obesity)

# choose the most appropriate RMA 

# 

library(dplyr)
library(purrr)
library(tidyr)
library(metafor)

# 1. Read in all your heterogeneity‐result CSVs
files <- list.files("hte_results_withHR", pattern = "\\.csv$", full.names = TRUE)
diffs_all <- map_df(files, read.csv, stringsAsFactors = FALSE)

# 2. For each (target, comparator, outcome, subgroup) run a random‑effects meta‑analysis
# 2. Run one random‑effects meta‐analysis per (target, comparator, outcome, subgroup)
meta_results <- diffs_all %>%
  group_by(
    cohortid_target, target_name,
    cohortid_comparator, comparator_name,
    outcome_id, outcome_name,
    subgroup
  ) %>%
  nest() %>%
  mutate(
    # count how many distinct databases contributed
    n_databases = map_int(data, ~ n_distinct(.x$database_id)),
    
    # run the meta‐analysis - overall est
    ma = map(data, ~ rma(
      yi     = est,
      sei    = se,
      data   = .x,
      method = "REML"
    )),
    
    # REML meta on grp1 log-RR
    ma_grp1 = map(data, ~ rma(
      yi     = RR_grp1,
      sei    = SE_grp1,
      data   = .x,
      method = "REML"
    )),
    
    # REML meta on grp2 log-RR
    ma_grp2 = map(data, ~ rma(
      yi     = RR_grp2,
      sei    = SE_grp2,
      data   = .x,
      method = "REML"
    ))
    
  ) %>%
  mutate(
    
    # extract overall results
    pooled_est = map_dbl(ma, ~ .x$b),
    pooled_se  = map_dbl(ma, ~ .x$se),
    pooled_z   = map_dbl(ma, ~ .x$zval),
    pooled_p   = map_dbl(ma, ~ .x$pval),
    ci_lb      = map_dbl(ma, ~ .x$ci.lb),
    ci_ub      = map_dbl(ma, ~ .x$ci.ub),
    
    # extract grp1 results
    pooled_est_grp1 = map_dbl(ma_grp1, ~ .x$b),
    pooled_se_grp1  = map_dbl(ma_grp1, ~ .x$se),
    pooled_z_grp1   = map_dbl(ma_grp1, ~ .x$zval),
    pooled_p_grp1   = map_dbl(ma_grp1, ~ .x$pval),
    ci_lb_grp1      = map_dbl(ma_grp1, ~ .x$ci.lb),
    ci_ub_grp1      = map_dbl(ma_grp1, ~ .x$ci.ub),
    
    # extract grp2 results
    pooled_est_grp2 = map_dbl(ma_grp2, ~ .x$b),
    pooled_se_grp2  = map_dbl(ma_grp2, ~ .x$se),
    pooled_z_grp2   = map_dbl(ma_grp2, ~ .x$zval),
    pooled_p_grp2   = map_dbl(ma_grp2, ~ .x$pval),
    ci_lb_grp2      = map_dbl(ma_grp2, ~ .x$ci.lb),
    ci_ub_grp2      = map_dbl(ma_grp2, ~ .x$ci.ub)
    
  ) %>%
  select(
    cohortid_target, target_name,
    cohortid_comparator, comparator_name,
    outcome_id, outcome_name,
    subgroup,
    n_databases,
    pooled_est, pooled_se, pooled_z, pooled_p, ci_lb, ci_ub,
    pooled_est_grp1, pooled_se_grp1, pooled_z_grp1, pooled_p_grp1, ci_lb_grp1, ci_ub_grp1,
    pooled_est_grp2, pooled_se_grp2, pooled_z_grp2, pooled_p_grp2, ci_lb_grp2, ci_ub_grp2
  ) %>%
  ungroup()

meta_results$HRgrp1 = round(exp(meta_results$pooled_est_grp1), 2)
ub1_SE = (exp(meta_results$ci_ub_grp1) - exp(meta_results$pooled_est_grp1)) / 1.96
lb1_SE = (exp(meta_results$ci_lb_grp1) - exp(meta_results$pooled_est_grp1)) / (-1.96)
meta_results$SE_HRgrp1 = round((ub1_SE + lb1_SE) / 2, 2)
meta_results$HRgrp2 = round(exp(meta_results$pooled_est_grp2), 2)
ub1_SE = (exp(meta_results$ci_ub_grp2) - exp(meta_results$pooled_est_grp2)) / 1.96
lb1_SE = (exp(meta_results$ci_lb_grp2) - exp(meta_results$pooled_est_grp2)) / (-1.96)
meta_results$SE_HRgrp2 = round((ub1_SE + lb1_SE) / 2, 2)

meta_results$CI_grp1 = paste0(round(exp(meta_results$pooled_est_grp1), 2), "_", "(", round(exp(meta_results$ci_lb_grp1), 2), ",", round(exp(meta_results$ci_ub_grp1), 2), ")")
meta_results$CI_grp2 = paste0(round(exp(meta_results$pooled_est_grp2), 2), "_", "(", round(exp(meta_results$ci_lb_grp2), 2), ",", round(exp(meta_results$ci_ub_grp2), 2), ")")
meta_results$p_val = round(meta_results$pooled_p, 2)

# 3. rank results

meta_ranked <- meta_results %>%
  group_by(subgroup) %>%
  arrange(subgroup, pooled_p) %>%
  mutate(rank_within_subgroup = row_number()) %>%
  ungroup()

# Write out your pooled‐results table
write.csv(
  meta_ranked,
  file = "heterogeneity_meta_results_withHR.csv",
  row.names = FALSE
)
