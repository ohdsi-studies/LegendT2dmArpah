################################################################################
# INSTRUCTIONS: Make sure you have downloaded your cohorts using 
# DownloadCohorts.R and that those cohorts are stored in the "inst" folder
# of the project. This script is written to use the sample study cohorts
# located in "inst/sampleStudy" so you will need to modify this in the code 
# below. 
# 
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
# ##############################################################################
library(dplyr)
library(Strategus)

########################################################
# Above the line - MODIFY ------------------------------
########################################################

# Get the list of cohorts - NOTE: you should modify this for your
# study to retrieve the cohorts you downloaded as part of
# DownloadCohorts.R

cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/cuimc_rev/Cohorts.csv",
  jsonFolder = "inst/cuimc_rev/cohorts",
  sqlFolder = "inst/cuimc_rev/sql/sql_server"
)

# Re-number cohorts
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11708,]$cohortId <- 1001
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11713,]$cohortId <- 1002
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11730,]$cohortId <- 1003
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11729,]$cohortId <- 1004
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11728,]$cohortId <- 1005
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11727,]$cohortId <- 1006
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11726,]$cohortId <- 1007
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11725,]$cohortId <- 1008
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11724,]$cohortId <- 1009
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11723,]$cohortId <- 1010
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11722,]$cohortId <- 1011
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11721,]$cohortId <- 1012
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11720,]$cohortId <- 1013
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11719,]$cohortId <- 1014
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11718,]$cohortId <- 1015
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11717,]$cohortId <- 1016
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11716,]$cohortId <- 1017
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11711,]$cohortId <- 1018

# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11746,]$cohortId <- 1019
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11710,]$cohortId <- 1020
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11732,]$cohortId <- 1021
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11745,]$cohortId <- 1022
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11733,]$cohortId <- 1023
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11739,]$cohortId <- 1024
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11979,]$cohortId <- 1041 # obesity dx code
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11980,]$cohortId <- 1042 # obesity measurement
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11741,]$cohortId <- 1025
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11743,]$cohortId <- 1026
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11737,]$cohortId <- 1027
# cohortDefinitionSet[cohortDefinitionSet$cohortId == 11735,]$cohortId <- 1028

cohortDefinitionSet[cohortDefinitionSet$cohortId == 11753,]$cohortId <- 1029
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11706,]$cohortId <- 1030
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11756,]$cohortId <- 1031
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11758,]$cohortId <- 1032
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11752,]$cohortId <- 1033
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11749,]$cohortId <- 1034
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11757,]$cohortId <- 1035
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11755,]$cohortId <- 1036
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11751,]$cohortId <- 1037
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11750,]$cohortId <- 1038
cohortDefinitionSet[cohortDefinitionSet$cohortId == 11748,]$cohortId <- 1039


###Metformin as T
# 11716 , #bigaunides 1503297
# 11723 , #thiazolidinediones 1525215
# 11722 , #Sulfonylureas 1502855,1502809,1597756,1559684,1594973,1560171
# 11721 , #SGLT2is 43526465,44785829,45774751,793293
# 11720 , #meglitinides 1516766,1502826
# 11719 , #GLP-1 RAs 45774435,1583722,44506754,40170911,793143
# 11718 , #GIP/GLP-RA dual agonists 779705
# 11717 , #DPP4s 1580747,40166035,40239216,43013884
# 11711 , #alpha-glucosidase inhibitors 1510202

##when metformin is not T
# 11713 , #alpha-glucosidase inhibitors with metformin
# 11730 , #DPP4s with metformin
# 11729 , #GIP-GLP dual agonist with metformin
# 11728 , #GLP-1 RA with metformin
# 11727 , #meglitinide with metformin
# 11726 , #SGLT2is with metformin
# 11725 , #Sulfonylureas with metformin 
# 11724 , #thiazolidinediones with metformin

tcis <- list(
  #########thiazolidinediones with metformin AS T
  list(
    targetId = 1009, ##thiazolidinediones with metformin
    comparatorId = 1008, #Sulfonylureas with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      1502855,1502809,1597756,1559684,1594973,1560171 #Sulfonylureas
    ) 
  ),
  list(
    targetId = 1009, ##thiazolidinediones with metformin
    comparatorId = 1007, #SGLT2is with metformin
    indicationId = 1001, # T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      43526465,44785829,45774751,793293 #SGLT2is
    ) 
  ), 
  list(
    targetId = 1009, ##thiazolidinediones with metformin
    comparatorId = 1006, #meglitinides with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      1516766,1502826 #meglitinides
    )
  ),
  list(
    targetId = 1009, ##thiazolidinediones with metformin
    comparatorId = 1005, #GLP-1 RAs  with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      45774435,1583722,44506754,40170911,793143 #GLP-1 RAs
    )
  ),
  list(
    targetId = 1009, ##thiazolidinediones with metformin
    comparatorId = 1004, #GIP/GLP-RA dual agonists with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      779705 #GIP/GLP-RA dual agonists
    )
  ),
  list(
    targetId = 1009, ##thiazolidinediones with metformin
    comparatorId = 1003, #DPP4s with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 1009, ##thiazolidinediones with metformin
    comparatorId = 1002, #alpha-glucosidase inhibitors with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      1510202 #alpha-glucosidase inhibitors
    )
  ), 
  ########SULFONYLUREAS with metformin AS Target######
  list(
    targetId = 1008, #Sulfonylureas with metformin
    comparatorId = 1007 , #SGLT2is with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      43526465,44785829,45774751,793293 #SGLT2is
    )
  ),
  list(
    targetId = 1008, #Sulfonylureas with metformin
    comparatorId = 1006 , #meglitinides with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      1516766,1502826 #meglitinides
    )
  ),
  list(
    targetId = 1008, #Sulfonylureas with metformin
    comparatorId = 1005 , #GLP-1 RA with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      45774435,1583722,44506754,40170911,793143 #GLP-1 RAs
    )
  ),
  list(
    targetId = 1008, #Sulfonylureas with metformin
    comparatorId = 1004, #GIP/GLP-RA dual agonists with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      779705 #GIP/GLP-RA dual agonists
    )
  ),
  list(
    targetId = 1008, #Sulfonylureas with metformin
    comparatorId = 1003, #DPP4s with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 1008, #Sulfonylureas with metformin
    comparatorId = 1002, #alpha-glucosidase inhibitors with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      1510202 #alpha-glucosidase inhibitors
    )
  ),
  ########SGLT2is with metformin AS Target######
  list(
    targetId = 1007 , #SGLT2is with metformin
    comparatorId = 1006 , #meglitinides with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      43526465,44785829,45774751,793293, #SGLT2is
      1516766,1502826 #meglitinides
    )
  ), 
  list(
    targetId = 1007 , #SGLT2is with metformin
    comparatorId = 1005 , #GLP-1 RA with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      43526465,44785829,45774751,793293, #SGLT2is
      45774435,1583722,44506754,40170911,793143 #GLP-1 RAs
    )
  ),
  list(
    targetId = 1007 , #SGLT2is with metformin
    comparatorId = 1004, #GIP/GLP-RA dual agonists with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      43526465,44785829,45774751,793293, #SGLT2is
      779705 #GIP/GLP-RA dual agonists
    )
  ),
  list(
    targetId = 1007 , #SGLT2is with metformin
    comparatorId = 1003, #DPP4s with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      43526465,44785829,45774751,793293, #SGLT2is
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 1007 , #SGLT2is with metformin
    comparatorId = 1002, #alpha-glucosidase inhibitors with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      43526465,44785829,45774751,793293, #SGLT2is
      1510202 #alpha-glucosidase inhibitors
    )
  ),
  ########GLP-1 RAs with metformin AS Target######
  list(
    targetId = 1005 , #GLP-1 RA with metformin
    comparatorId = 1006 , #meglitinides with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      45774435,1583722,44506754,40170911,793143, #GLP-1 RAs
      1516766,1502826 #meglitinides
    )
  ), 
  list(
    targetId = 1005 , #GLP-1 RA with metformin
    comparatorId = 1004, #GIP/GLP-RA dual agonists with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      45774435,1583722,44506754,40170911,793143, #GLP-1 RAs
      779705 #GIP/GLP-RA dual agonists
    )
  ),
  list(
    targetId = 1005 , #GLP-1 RA with metformin
    comparatorId = 1003, #DPP4s with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      45774435,1583722,44506754,40170911,793143, #GLP-1 RAs
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 1005 , #GLP-1 RA with metformin
    comparatorId = 1002, #alpha-glucosidase inhibitors with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      45774435,1583722,44506754,40170911,793143, #GLP-1 RAs
      1510202 #alpha-glucosidase inhibitors
    )
  ),
  #########GIP/GLP-RA dual agonists with metformin AS Target######
  list(
    targetId = 1004, #GIP/GLP-RA dual agonists with metformin
    comparatorId = 1006 , #meglitinides with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      779705, #GIP/GLP-RA dual agonists
      1516766,1502826 #meglitinides
    )
  ), 
  list(
    targetId = 1004, #GIP/GLP-RA dual agonists with metformin
    comparatorId = 1003, #DPP4s with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      779705, #GIP/GLP-RA dual agonists
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 1004, #GIP/GLP-RA dual agonists with metformin
    comparatorId = 1002, #alpha-glucosidase inhibitors with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      779705, #GIP/GLP-RA dual agonists
      1510202 #alpha-glucosidase inhibitors
    )
  ), 
  #########meglitinides with metformin AS Target######
  list(
    targetId = 1006 , #meglitinides with metformin
    comparatorId = 1003, #DPP4s with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1516766,1502826, #meglitinides
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 1006 , #meglitinides with metformin
    comparatorId = 1002, #alpha-glucosidase inhibitors with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1516766,1502826, #meglitinides
      1510202 #alpha-glucosidase inhibitors
    )
  ),
  ##########alpha-glucosidase inhibitors with metformin AS Target######
  list(
    targetId = 1002, #alpha-glucosidase inhibitors with metformin
    comparatorId = 1003, #DPP4s with metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1510202, #alpha-glucosidase inhibitors
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  ##########Biguanides AS Target (uses second set of drug cohorts)######
  list(
    targetId = 1017 , #bigaunides 
    comparatorId = 1011, #Sulfonylureas w/o metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      1502855,1502809,1597756,1559684,1594973,1560171 #Sulfonylureas
    )
  ),
  list(
    targetId = 1017 , #bigaunides 
    comparatorId = 1010, ##thiazolidinediones w/o metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      1525215 #thiazolidinediones
    )
  ),
  list(
    targetId = 1017 , #bigaunides 
    comparatorId = 1015, #GIP/GLP-RA dual agonists w/o metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      779705 #GIP/GLP-RA dual agonists
    )
  ), 
  list(
    targetId = 1017 , #bigaunides 
    comparatorId = 1012 , #SGLT2is w/o metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      43526465,44785829,45774751,793293 #SGLT2is
    )
  ),
  list(
    targetId = 1017 , #bigaunides 
    comparatorId = 1014 , #GLP-1 RA w/o metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      45774435,1583722,44506754,40170911,793143 #GLP-1 RAs
    )
  ),
  list(
    targetId = 1017 , #bigaunides 
    comparatorId = 1016, #DPP4s w/o metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 1017 , #bigaunides 
    comparatorId = 1013 , #meglitinides w/o metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      1516766,1502826 #meglitinides
    )
  ),
  list(
    targetId = 1017 , #bigaunides 
    comparatorId = 1018 , #alpha-glucosidase inhibitors w/o metformin
    indicationId = 1001, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      1510202 #alpha-glucosidase inhibitors
    )
  )
)##corresponds to opening parens

# These are the cohorts we'd like to use as subgroups for all T/Cs
cohortSubsets <- c(1024,1041,1042)
# cohortSubsets <- c(1019,1020,1021,1022,1023,1024,1025,1026,1027,1028)
#       11746, 11710, 11732, 11745, 11733, 11739, 11741, 11743, 11737, 11735)

ageGroups <- list(
  list(
    minAge = 0,
    maxAge = 20
  ),
  list(
    minAge = 21,
    maxAge = 60
  ),
  list(
    minAge = 61,
    maxAge = 80
  )
)

outcomes <- tibble(
  cohortId = c(1029,1030,1031,1032,1033,1034,1035,1036,1037,1038,1039),
  #11753, 11706, 11756, 11758, 11752, 11749, 11757, 11755, 11751, 11750, 11748),  
  cleanWindow = c(365, 365, 365, 365, 365, 365, 365, 365, 365, 365, 365)
)

# Time-at-risks (TARs) for the outcomes of interest in your study
timeAtRisks <- tibble(
  label = c("On treatment"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)
# # Try to avoid intent-to-treat TARs for SCCS, or then at least disable calendar time spline:
# sccsTimeAtRisks <- tibble(
#   label = c("On treatment", "On treatment"),
#   riskWindowStart  = c(1, 1),
#   startAnchor = c("cohort start", "cohort start"),
#   riskWindowEnd  = c(0, 0),
#   endAnchor = c("cohort end", "cohort end")
# )
# # Try to use fixed-time TARs for patient-level prediction:
# plpTimeAtRisks <- tibble(
#   riskWindowStart  = c(1, 1),
#   startAnchor = c("cohort start", "cohort start"),
#   riskWindowEnd  = c(365, 365),
#   endAnchor = c("cohort start", "cohort start"),
# )
# If you are not restricting your study to a specific time window, 
# please make these strings empty
studyStartDate <- '20150101' #YYYYMMDD
studyEndDate <- '20241231'   #YYYYMMDD
# Some of the settings require study dates with hyphens
studyStartDateWithHyphens <- gsub("(\\d{4})(\\d{2})(\\d{2})", "\\1-\\2-\\3", studyStartDate)
studyEndDateWithHyphens <- gsub("(\\d{4})(\\d{2})(\\d{2})", "\\1-\\2-\\3", studyEndDate)

# Consider these settings for estimation  ----------------------------------------

useCleanWindowForPriorOutcomeLookback <- TRUE # If FALSE, lookback window is all time prior, i.e., including only first events
psMatchMaxRatio <- 1 # If bigger than 1, the outcome model will be conditioned on the matched set
maxCohortSizeForFitting <- 250000 # Downsampled example study to 10000
maxCohortSize <- maxCohortSizeForFitting
maxCasesPerOutcome <- 1000000 # Downsampled example study to 10000

# Consider these settings for patient-level prediction  ----------------------------------------
#plpMaxSampleSize <- 1000000 # Downsampled example study to 20000

########################################################
# Below the line - DO NOT MODIFY -----------------------
########################################################

# Don't change below this line (unless you know what you're doing) -------------

# Shared Resources -------------------------------------------------------------

# Get the unique subset criteria from the tcis
# object to construct the cohortDefintionSet's 
# subset definitions for each target/comparator
# cohort
dfUniqueTcis <- data.frame()
for (i in seq_along(tcis)) {
  dfUniqueTcis <- rbind(dfUniqueTcis, data.frame(cohortId = tcis[[i]]$targetId,
                                                 indicationId = paste(tcis[[i]]$indicationId, collapse = ",")
  ))
  if (!is.null(tcis[[i]]$comparatorId)) {
    dfUniqueTcis <- rbind(dfUniqueTcis, data.frame(cohortId = tcis[[i]]$comparatorId,
                                                   indicationId = paste(tcis[[i]]$indicationId, collapse = ",")
    ))
  }
}

dfUniqueTcis <- unique(dfUniqueTcis)
dfUniqueTcis$subsetDefinitionId <- 0 # Adding as a placeholder for loop below
dfUniqueSubsetCriteria <- unique(dfUniqueTcis[,-1])
for (i in 1:nrow(dfUniqueSubsetCriteria)) {
  uniqueSubsetCriteria <- dfUniqueSubsetCriteria[i,]
  dfCurrentTcis <- dfUniqueTcis[dfUniqueTcis$indicationId == uniqueSubsetCriteria$indicationId,]
  targetCohortIdsForSubsetCriteria <- as.integer(dfCurrentTcis[, "cohortId"])
  dfUniqueTcis[dfUniqueTcis$indicationId == dfCurrentTcis$indicationId,]$subsetDefinitionId <- i
  
  subsetOperators <- list()
  if (uniqueSubsetCriteria$indicationId != "") {
    subsetOperators[[length(subsetOperators) + 1]] <- CohortGenerator::createCohortSubset(
      cohortIds = uniqueSubsetCriteria$indicationId,
      negate = FALSE,
      cohortCombinationOperator = "all",
      startWindow = CohortGenerator::createSubsetCohortWindow(-99999, 0, "cohortStart"),
      endWindow = CohortGenerator::createSubsetCohortWindow(0, 99999, "cohortStart")
    )
  }
  subsetOperators[[length(subsetOperators) + 1]] <- CohortGenerator::createLimitSubset(
    priorTime = 365,
    followUpTime = 1,
    limitTo = "firstEver"
  )
  if (studyStartDate != "" || studyEndDate != "") {
    subsetOperators[[length(subsetOperators) + 1]] <- CohortGenerator::createLimitSubset(
      calendarStartDate = if (studyStartDate == "") NULL else as.Date(studyStartDate, "%Y%m%d"),
      calendarEndDate = if (studyEndDate == "") NULL else as.Date(studyEndDate, "%Y%m%d")
    )
  }
  subsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "",
    definitionId = i,
    subsetOperators = subsetOperators#,
    #operatorNameConcatString = "[T with I]"
  )
  cohortDefinitionSet <- cohortDefinitionSet %>%
    CohortGenerator::addCohortSubsetDefinition(
      cohortSubsetDefintion = subsetDef,
      targetCohortIds = targetCohortIdsForSubsetCriteria
    ) 
  
  if (uniqueSubsetCriteria$indicationId != "") {
    # Also create restricted version of indication cohort:
    subsetDef <- CohortGenerator::createCohortSubsetDefinition(
      name = "",
      definitionId = i + 10,
      subsetOperators = subsetOperators[2:length(subsetOperators)]
    )
    cohortDefinitionSet <- cohortDefinitionSet %>%
      CohortGenerator::addCohortSubsetDefinition(
        cohortSubsetDefintion = subsetDef,
        targetCohortIds = as.integer(uniqueSubsetCriteria$indicationId)
      )
  }  
}

# Making a copy to use when adding CUIMC subgroups and
# later setting up CM T/C combos
dfOriginalUniqueTcis <- dfUniqueTcis

# CUIMC subsets - apply to the T with I subgroup
targetCohortsWithIndicationIds <- cohortDefinitionSet |>
  filter(isSubset == TRUE) |>
  filter(subsetParent %in% dfUniqueTcis$cohortId) |>
  select(cohortId)

for (j in seq_along(cohortSubsets)) {
  # You belong to the subset
  subsetOperators <- list()
  subsetOperators[[length(subsetOperators) + 1]] <- CohortGenerator::createCohortSubset(
    cohortIds = cohortSubsets[j],
    negate = FALSE,
    cohortCombinationOperator = "all",
    startWindow = CohortGenerator::createSubsetCohortWindow(-99999, 0, "cohortStart"),
    endWindow = CohortGenerator::createSubsetCohortWindow(0, 99999, "cohortStart")
  )
  
  # Also create restricted version of indication cohort:
  restrictedSubsetDefinitionId <- j + 30
  subsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "",
    definitionId = restrictedSubsetDefinitionId,
    subsetOperators = subsetOperators,
    identifierExpression = "targetId * 1000 + definitionId"
  )
  
  # Add this restricted version of the indication cohort 
  # to the dfUniqueTcis list for all of the original 
  # TCIS entries
  dfUniqueTcis <- rbind(dfUniqueTcis,
                        data.frame(
                          dfOriginalUniqueTcis[, c("cohortId", "indicationId")],
                          subsetDefinitionId = restrictedSubsetDefinitionId
                        ))
  
  cohortDefinitionSet <- cohortDefinitionSet %>%
    CohortGenerator::addCohortSubsetDefinition(
      cohortSubsetDefintion = subsetDef,
      targetCohortIds = targetCohortsWithIndicationIds$cohortId
    ) 

  # You DON'T belong to the subset
  subsetOperators <- list()
  subsetOperators[[length(subsetOperators) + 1]] <- CohortGenerator::createCohortSubset(
    cohortIds = cohortSubsets[j],
    negate = TRUE,
    cohortCombinationOperator = "all",
    startWindow = CohortGenerator::createSubsetCohortWindow(-99999, 0, "cohortStart"),
    endWindow = CohortGenerator::createSubsetCohortWindow(0, 99999, "cohortStart")
  )

  # Also create restricted version of indication cohort:
  complementSubsetDefinitionId <- j + 50
  subsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "",
    definitionId = complementSubsetDefinitionId,
    subsetOperators = subsetOperators,
    identifierExpression = "targetId * 1000 + definitionId"
  )

  # Add this complement version of the indication cohort 
  # to the dfUniqueTcis list for all of the original 
  # TCIS entries
  dfUniqueTcis <- rbind(dfUniqueTcis,
                        data.frame(
                          dfOriginalUniqueTcis[, c("cohortId", "indicationId")],
                          subsetDefinitionId = complementSubsetDefinitionId
                        ))
  
  cohortDefinitionSet <- cohortDefinitionSet %>%
    CohortGenerator::addCohortSubsetDefinition(
      cohortSubsetDefintion = subsetDef,
      targetCohortIds = targetCohortsWithIndicationIds$cohortId
    )
}

# Age groups
for (j in 1:length(ageGroups)) {
  # Restrict to age
  subsetOperators <- list()
  subsetOperators[[length(subsetOperators) + 1]] <- CohortGenerator::createDemographicSubset(
    ageMin = ageGroups[[j]]$minAge,
    ageMax = ageGroups[[j]]$maxAge
  )

  # Also create age restricted version of indication cohort:
  ageRestrictedSubsetDefinitionId <- j + 70
  subsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "",
    definitionId = ageRestrictedSubsetDefinitionId,
    subsetOperators = subsetOperators,
    identifierExpression = "targetId * 1000 + definitionId"
  )

  # Add this age restricted version of the indication cohort
  # to the dfUniqueTcis list for all of the original
  # TCIS entries
  dfUniqueTcis <- rbind(dfUniqueTcis,
                        data.frame(
                          dfOriginalUniqueTcis[, c("cohortId", "indicationId")],
                          subsetDefinitionId = ageRestrictedSubsetDefinitionId
                        ))

  cohortDefinitionSet <- cohortDefinitionSet %>%
    CohortGenerator::addCohortSubsetDefinition(
      cohortSubsetDefintion = subsetDef,
      targetCohortIds = targetCohortsWithIndicationIds$cohortId
    )
}

# Gender
genders <- c(8507, 8532)
for (j in seq_along(genders)) {
  # Restrict to gender
  subsetOperators <- list()
  subsetOperators[[length(subsetOperators) + 1]] <- CohortGenerator::createDemographicSubset(
    gender = genders[j]
  )

  # Also create restricted version of indication cohort:
  genderRestrictedSubsetDefinitionId <- j + 90
  subsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "",
    definitionId = genderRestrictedSubsetDefinitionId,
    subsetOperators = subsetOperators,
    identifierExpression = "targetId * 1000 + definitionId"
  )

  # Add this gender restricted version of the indication cohort
  # to the dfUniqueTcis list for all of the original
  # TCIS entries
  dfUniqueTcis <- rbind(dfUniqueTcis,
                        data.frame(
                          dfOriginalUniqueTcis[, c("cohortId", "indicationId")],
                          subsetDefinitionId = genderRestrictedSubsetDefinitionId
                        ))

  cohortDefinitionSet <- cohortDefinitionSet %>%
    CohortGenerator::addCohortSubsetDefinition(
      cohortSubsetDefintion = subsetDef,
      targetCohortIds = targetCohortsWithIndicationIds$cohortId
    )
}


negativeControlOutcomeCohortSet <- CohortGenerator::readCsv(
  file = "inst/cuimc/negativeControlOutcomes.csv"
)

if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01
)

# CharacterizationModule Settings ---------------------------------------------
cModuleSettingsCreator <- CharacterizationModule$new()
allCohortIdsExceptOutcomes <- cohortDefinitionSet %>%
  filter(!cohortId %in% outcomes$cohortId) %>%
  pull(cohortId)

characterizationModuleSpecifications <- cModuleSettingsCreator$createModuleSpecifications(
  targetIds = allCohortIdsExceptOutcomes,
  outcomeIds = outcomes$cohortId,
  outcomeWashoutDays = outcomes$cleanWindow,
  minPriorObservation = 365,
  dechallengeStopInterval = 30,
  dechallengeEvaluationWindow = 30,
  riskWindowStart = timeAtRisks$riskWindowStart, 
  startAnchor = timeAtRisks$startAnchor, 
  riskWindowEnd = timeAtRisks$riskWindowEnd, 
  endAnchor = timeAtRisks$endAnchor,
  minCharacterizationMean = .01
)


# CohortIncidenceModule --------------------------------------------------------
ciModuleSettingsCreator <- CohortIncidenceModule$new()
exposureIndicationIds <- cohortDefinitionSet %>%
  filter(!cohortId %in% outcomes$cohortId & isSubset) %>%
  pull(cohortId)
targetList <- lapply(
  exposureIndicationIds,
  function(cohortId) {
    CohortIncidence::createCohortRef(
      id = cohortId, 
      name = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == cohortId]
    )
  }
)
outcomeList <- lapply(
  seq_len(nrow(outcomes)),
  function(i) {
    CohortIncidence::createOutcomeDef(
      id = i, 
      name = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == outcomes$cohortId[i]], 
      cohortId = outcomes$cohortId[i], 
      cleanWindow = outcomes$cleanWindow[i]
    )
  }
)

tars <- list()
for (i in seq_len(nrow(timeAtRisks))) {
  tars[[i]] <- CohortIncidence::createTimeAtRiskDef(
    id = i, 
    startWith = gsub("cohort ", "", timeAtRisks$startAnchor[i]), 
    endWith = gsub("cohort ", "", timeAtRisks$endAnchor[i]), 
    startOffset = timeAtRisks$riskWindowStart[i],
    endOffset = timeAtRisks$riskWindowEnd[i]
  )
}
analysis1 <- CohortIncidence::createIncidenceAnalysis(
  targets = exposureIndicationIds,
  outcomes = seq_len(nrow(outcomes)),
  tars = seq_along(tars)
)
# irStudyWindow <- CohortIncidence::createDateRange(
#   startDate = studyStartDateWithHyphens,
#   endDate = studyEndDateWithHyphens
# )
irDesign <- CohortIncidence::createIncidenceDesign(
  targetDefs = targetList,
  outcomeDefs = outcomeList,
  tars = tars,
  analysisList = list(analysis1),
  #studyWindow = irStudyWindow,
  strataSettings = CohortIncidence::createStrataSettings(
    byYear = TRUE,
    byGender = TRUE,
    byAge = TRUE,
    ageBreaks = seq(0, 110, by = 10)
  )
)
cohortIncidenceModuleSpecifications <- ciModuleSettingsCreator$createModuleSpecifications(
  irDesign = irDesign$toList()
)


# CohortMethodModule -----------------------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE # Keep TRUE because you're excluding concepts
)
outcomeList <- append(
  lapply(seq_len(nrow(outcomes)), function(i) {
    if (useCleanWindowForPriorOutcomeLookback)
      priorOutcomeLookback <- outcomes$cleanWindow[i]
    else
      priorOutcomeLookback <- 99999
    CohortMethod::createOutcome(
      outcomeId = outcomes$cohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = priorOutcomeLookback
    )
  }),
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)
targetComparatorOutcomesList <- list()
for (i in seq_along(tcis)) {
  tci <- tcis[[i]]
  
  # Filter to the T/C with I - Use the original
  # unique Tcis for this since dfUniqueTcis will
  # contain all of the nested subgroups
  currentSubsetDefinitionId <- dfOriginalUniqueTcis %>%
    filter(cohortId == tci$targetId &
             indicationId == paste(tci$indicationId, collapse = ",")) %>%
    pull(subsetDefinitionId)
  
  targetId <- cohortDefinitionSet %>%
    filter(subsetParent == tci$targetId & subsetDefinitionId == currentSubsetDefinitionId) %>%
    pull(cohortId)
  comparatorId <- cohortDefinitionSet %>% 
    filter(subsetParent == tci$comparatorId & subsetDefinitionId == currentSubsetDefinitionId) %>%
    pull(cohortId)  
  targetComparatorOutcomesList[[length(targetComparatorOutcomesList)+1]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = targetId,
    comparatorId = comparatorId,
    outcomes = outcomeList,
    excludedCovariateConceptIds = tci$excludedCovariateConceptIds
  )
  
  # We'll retain these T/C/Subset ids to use in the subsequent loop
  parentSubsetId <- currentSubsetDefinitionId
  parentTargetId <- targetId
  parentComparatorId <- comparatorId

  # Get the subset definition IDs that matches
  # the target ID. The comparator will also use the same subset
  # definition ID. Exclude the parentSubsetId from this list
  subsetDefinitions <- dfUniqueTcis %>%
    filter(cohortId == tci$targetId &
             indicationId == paste(tci$indicationId, collapse = ",") &
             subsetDefinitionId !=  parentSubsetId) %>%
    pull(subsetDefinitionId)
  
  for (j in seq_along(subsetDefinitions)) {
    currentSubsetDefinitionId <- subsetDefinitions[j]
    targetId <- cohortDefinitionSet %>%
      filter(subsetParent == parentTargetId & subsetDefinitionId == currentSubsetDefinitionId) %>%
      pull(cohortId)
    comparatorId <- cohortDefinitionSet %>% 
      filter(subsetParent == parentComparatorId & subsetDefinitionId == currentSubsetDefinitionId) %>%
      pull(cohortId)
    targetComparatorOutcomesList[[length(targetComparatorOutcomesList)+1]] <- CohortMethod::createTargetComparatorOutcomes(
      targetId = targetId,
      comparatorId = comparatorId,
      outcomes = outcomeList,
      excludedCovariateConceptIds = tci$excludedCovariateConceptIds
    )    
  }
}
getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
  restrictToCommonPeriod = TRUE,
  studyStartDate = studyStartDate,
  studyEndDate = studyEndDate,
  maxCohortSize = 0,
  covariateSettings = covariateSettings
)
createPsArgs = CohortMethod::createCreatePsArgs(
  maxCohortSizeForFitting = maxCohortSizeForFitting,
  errorOnHighCorrelation = TRUE,
  stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
  estimator = "att",
  prior = Cyclops::createPrior(
    priorType = "laplace", 
    exclude = c(0), 
    useCrossValidation = TRUE
  ),
  control = Cyclops::createControl(
    noiseLevel = "silent", 
    cvType = "auto", 
    seed = 1, 
    resetCoefficients = TRUE, 
    tolerance = 2e-07, 
    cvRepetitions = 1, 
    startingVariance = 0.01
  )
)
matchOnPsArgs = CohortMethod::createMatchOnPsArgs(
  maxRatio = psMatchMaxRatio,
  caliper = 0.2,
  caliperScale = "standardized logit",
  allowReverseMatch = FALSE,
  stratificationColumns = c()
)
# stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
#   numberOfStrata = 5,
#   stratificationColumns = c(),
#   baseSelection = "all"
# )
computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = maxCohortSize,
  covariateFilter = NULL
)
computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = maxCohortSize,
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)
fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = psMatchMaxRatio != 1,
  useCovariates = FALSE,
  inversePtWeighting = FALSE,
  prior = Cyclops::createPrior(
    priorType = "laplace", 
    useCrossValidation = TRUE
  ),
  control = Cyclops::createControl(
    cvType = "auto", 
    seed = 1, 
    resetCoefficients = TRUE,
    startingVariance = 0.01, 
    tolerance = 2e-07, 
    cvRepetitions = 1, 
    noiseLevel = "quiet"
  )
)
cmAnalysisList <- list()
for (i in seq_len(nrow(timeAtRisks))) {
  createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
    firstExposureOnly = FALSE,
    washoutPeriod = 0,
    removeDuplicateSubjects = "keep first",
    censorAtNewRiskWindow = TRUE,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 99999,
    riskWindowStart = timeAtRisks$riskWindowStart[[i]],
    startAnchor = timeAtRisks$startAnchor[[i]],
    riskWindowEnd = timeAtRisks$riskWindowEnd[[i]],
    endAnchor = timeAtRisks$endAnchor[[i]],
    minDaysAtRisk = 1,
    maxDaysAtRisk = 99999
  )
  cmAnalysisList[[i]] <- CohortMethod::createCmAnalysis(
    analysisId = i,
    description = sprintf(
      "Cohort method, %s",
      timeAtRisks$label[i]
    ),
    getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
    createStudyPopArgs = createStudyPopArgs,
    createPsArgs = createPsArgs,
    matchOnPsArgs = matchOnPsArgs,
    # stratifyByPsArgs = stratifyByPsArgs,
    computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
    computeCovariateBalanceArgs = computeCovariateBalanceArgs,
    fitOutcomeModelArgs = fitOutcomeModelArgs
  )
}
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)


# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  #Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(characterizationModuleSpecifications) 
  #Strategus::addModuleSpecifications(cohortIncidenceModuleSpecifications) |>
  #Strategus::addModuleSpecifications(cohortMethodModuleSpecifications) 

if (!dir.exists(file.path("inst", "cuimc_rev"))) {
  dir.create(file.path("inst", "cuimc_rev"), recursive = T)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "cuimc_rev", "Obesity_Cohort_Analysis_Specification.json")
)