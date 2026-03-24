################################################################################
# INSTRUCTIONS: This script assumes you have cohorts you would like to use in an
# ATLAS instance. Please note you will need to update the baseUrl to match
# the settings for your enviroment. You will also want to change the 
# CohortGenerator::saveCohortDefinitionSet() function call arguments to identify
# a folder to store your cohorts. This code will store the cohorts in 
# "inst/sampleStudy" as part of the template for reference. You should store
# your settings in the root of the "inst" folder and consider removing the 
# "inst/sampleStudy" resources when you are ready to release your study.
# 
# See the Download cohorts section
# of the UsingThisTemplate.md for more details.
# ##############################################################################

###the authoring site (CUIMC aka me) needs to complete this script BEFORE executing the createStratspecs script

library(dplyr)
baseUrl <- ""
# Use this if your WebAPI instance has security enables
# ROhdsiWebApi::authorizeWebApi(
#   baseUrl = baseUrl,
#   authMethod = "windows"
# )
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    ###indication
    11708 , #T2DM 
    
    ###drug cohorts
    11713 , #alpha-glucosidase inhibitors with metformin
    11730 , #DPP4s with metformin
    11729 , #GIP-GLP dual agonist with metformin
    11728 , #GLP-1 RA with metformin
    11727 , #meglitinide with metformin
    11726 , #SGLT2is with metformin
    11725 , #Sulfonylureas with metformin 
    11724 , #thiazolidinediones with metformin
    11723 , #thiazolidinediones
    11722 , #Sulfonylureas 
    11721 , #SGLT2is 
    11720 , #meglitinide 
    11719 , #GLP-1 RAs 
    11718 , #GIP/GLP-RA dual agonist 
    11717 , #DPP4s 
    11716 , #bigaunides 
    11711 , #alpha-glucosidase inhibitor
    ###subgroup
    # 11746 , #diabetes severity
    # 11710 , #metabolic dysfunction-associated steatotic liver disease 
    # 11732 , #renal disease not on dialysis 
    # 11745 , #ANY renal disease (for negation)
    # 11733 , #renal disease on dialysis
    11739 , #obesity
    11979, # obesity dx only
    11980, # obesity measurement only
    # 11741 , #diabetic ketoacidosis 
    # 11743 , #diabetic retinopathy
    # 11737 , #hyperlipidemia
    # 11735 , #essential hypertension
    ###outcomes
    11753 , #acute pancreatitis
    11706 , #hepatic failure
    11756 , #hypoglycemia
    11758 , #diabetic ketoacidosis
    11752 , #abnormal weight gain
    11749 , #acute renal failure
    11757 , #vomiting
    11755 , #diarrhea
    11751 , #stroke
    11750 , #hospitalization with heart failure events
    11748  #acute myocardial infarction
  ),
  generateStats = TRUE
)

# Rename cohorts
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11708,]$cohortName <- "T2DM"

cohortDefinitionSet[cohortDefinitionSet$cohortId == 11713,]$cohortName <- "alpha-glucosidase inhibitors with metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11730,]$cohortName <- "DPP4s with metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11729,]$cohortName <- "GIP-GLP-1 RA dual agonists with metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11728,]$cohortName <- "GLP-1 RAs with metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11727,]$cohortName <- "meglitinide with metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11726,]$cohortName <- "SGLT2is with metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11725,]$cohortName <- "Sulfonylureas with metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11724,]$cohortName <- "thiazolidinediones with metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11723,]$cohortName <- "thiazolidinediones without metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11722,]$cohortName <- "Sulfonylureas without metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11721,]$cohortName <- "SGLT2is without metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11720,]$cohortName <- "meglitinides without metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11719,]$cohortName <- "GLP-1 RAs without metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11718,]$cohortName <- "GIP-GLP-1 RA dual agonists without metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11717,]$cohortName <- "DPP4s without metformin"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11716,]$cohortName <- "bigaunides"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11711,]$cohortName <- "alpha-glucosidase inhibitors without metformin"

# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11746,]$cohortName <- "diabetes severity"
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11710,]$cohortName <- "metabolic dysfunction-associated steatotic liver disease "
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11732,]$cohortName <- "renal disease not on dialysis"
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11745,]$cohortName <- "ANY renal disease (for negation)"
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11733,]$cohortName <- "renal disease on dialysis"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11739,]$cohortName <- "obesity"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11779,]$cohortName <- "obesity dx code"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11780,]$cohortName <- "obesity measurement"

cohortDefinitionSet[cohortDefinitionSet$cohortId == 11741,]$cohortName <- "diabetic ketoacidosis"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11743,]$cohortName <- "diabetic retinopathy"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11737,]$cohortName <- "hyperlipidemia"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11735,]$cohortName <- "essential hypertension"

cohortDefinitionSet[cohortDefinitionSet$cohortId == 11753,]$cohortName <- "acute pancreatitis"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11706,]$cohortName <- "hepatic failure"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11756,]$cohortName <- "hypoglycemia"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11758,]$cohortName <- "diabetic ketoacidosis"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11752,]$cohortName <- "abnormal weight gain"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11749,]$cohortName <- "acute renal failure"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11757,]$cohortName <- "vomiting"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11755,]$cohortName <- "diarrhea"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11751,]$cohortName <- "stroke"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11750,]$cohortName <- "hospitalization with heart failure events"
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11748,]$cohortName <- "acute myocardial infarction"


# Re-number cohorts
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11708,]$cohortId <- 11708
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11713,]$cohortId <- 11713
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11730,]$cohortId <- 11730
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11729,]$cohortId <- 11729
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11728,]$cohortId <- 11728
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11727,]$cohortId <- 11727
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11726,]$cohortId <- 11726
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11725,]$cohortId <- 11725
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11724,]$cohortId <- 11724
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11723,]$cohortId <- 11723
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11722,]$cohortId <- 11722
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11721,]$cohortId <- 11721
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11720,]$cohortId <- 11720
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11719,]$cohortId <- 11719
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11718,]$cohortId <- 11718
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11717,]$cohortId <- 11717
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11716,]$cohortId <- 11716
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11711,]$cohortId <- 11711

# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11746,]$cohortId <- 11746
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11710,]$cohortId <- 11710
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11732,]$cohortId <- 11732
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11745,]$cohortId <- 11745
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11733,]$cohortId <- 11733
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11739,]$cohortId <- 11739
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11741,]$cohortId <- 11741
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11743,]$cohortId <- 11743
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11737,]$cohortId <- 11737
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11735,]$cohortId <- 11735

# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11753,]$cohortId <- 11753
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11706,]$cohortId <- 11706
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11756,]$cohortId <- 11756
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11758,]$cohortId <- 11758
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11752,]$cohortId <- 11752
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11749,]$cohortId <- 11749
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11757,]$cohortId <- 11757
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11755,]$cohortId <- 11755
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11751,]$cohortId <- 11751
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11750,]$cohortId <- 11750
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11748,]$cohortId <- 11748

# Save the cohort definition set
# NOTE: Update settingsFileName, jsonFolder and sqlFolder
# for your study.
CohortGenerator::saveCohortDefinitionSet(
  cohortDefinitionSet = cohortDefinitionSet,
  settingsFileName = "inst/cuimc_rev/Cohorts.csv",
  jsonFolder = "inst/cuimc_rev/cohorts",
  sqlFolder = "inst/cuimc_rev/sql/sql_server",
)


# Download and save the negative control outcomes
# negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
#   conceptSetId = 1885090,
#   baseUrl = baseUrl
# ) %>%
#   ROhdsiWebApi::resolveConceptSet(
#     baseUrl = baseUrl
#   ) %>%
#   ROhdsiWebApi::getConcepts(
#     baseUrl = baseUrl
#   ) %>%
#   rename(outcomeConceptId = "conceptId",
#          cohortName = "conceptName") %>%
#   mutate(cohortId = row_number() + 100) %>%
#   select(cohortId, cohortName, outcomeConceptId)
# 
# # NOTE: Update file location for your study.
# CohortGenerator::writeCsv(
#   x = negativeControlOutcomeCohortSet,
#   file = "inst/sampleStudy/negativeControlOutcomes.csv",
#   warnOnFileNameCaseMismatch = F
# )
