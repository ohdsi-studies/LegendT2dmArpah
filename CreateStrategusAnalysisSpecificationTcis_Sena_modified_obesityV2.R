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
    targetId = 11724, ##thiazolidinediones with metformin
    comparatorId = 11725, #Sulfonylureas with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      1502855,1502809,1597756,1559684,1594973,1560171 #Sulfonylureas
    ) 
  ),
  list(
    targetId = 11724, ##thiazolidinediones with metformin
    comparatorId = 11726, #SGLT2is with metformin
    indicationId = 11708, # T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      43526465,44785829,45774751,793293 #SGLT2is
    ) 
  ), 
  list(
    targetId = 11724, ##thiazolidinediones with metformin
    comparatorId = 11727, #meglitinides with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      1516766,1502826 #meglitinides
    )
  ),
  list(
    targetId = 11724, ##thiazolidinediones with metformin
    comparatorId = 11728, #GLP-1 RAs  with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      45774435,1583722,44506754,40170911,793143 #GLP-1 RAs
    )
  ),
  list(
    targetId = 11724, ##thiazolidinediones with metformin
    comparatorId = 11729, #GIP/GLP-RA dual agonists with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      779705 #GIP/GLP-RA dual agonists
    )
  ),
  list(
    targetId = 11724, ##thiazolidinediones with metformin
    comparatorId = 11730, #DPP4s with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 11724, ##thiazolidinediones with metformin
    comparatorId = 11713, #alpha-glucosidase inhibitors with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1525215, #thiazolidinediones
      1510202 #alpha-glucosidase inhibitors
    )
  ), 
  ########SULFONYLUREAS with metformin AS Target######
  # list(
  #   targetId = 11725, #Sulfonylureas with metformin
  #   comparatorId = 11724, ##thiazolidinediones with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
  #     1525215 #thiazolidinediones
  #   )
  # ),
  list(
    targetId = 11725, #Sulfonylureas with metformin
    comparatorId = 11726 , #SGLT2is with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      43526465,44785829,45774751,793293 #SGLT2is
    )
  ),
  list(
    targetId = 11725, #Sulfonylureas with metformin
    comparatorId = 11727 , #meglitinides with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      1516766,1502826 #meglitinides
    )
  ),
  list(
    targetId = 11725, #Sulfonylureas with metformin
    comparatorId = 11728 , #GLP-1 RA with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      45774435,1583722,44506754,40170911,793143 #GLP-1 RAs
    )
  ),
  list(
    targetId = 11725, #Sulfonylureas with metformin
    comparatorId = 11729, #GIP/GLP-RA dual agonists with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      779705 #GIP/GLP-RA dual agonists
    )
  ),
  list(
    targetId = 11725, #Sulfonylureas with metformin
    comparatorId = 11730, #DPP4s with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 11725, #Sulfonylureas with metformin
    comparatorId = 11713, #alpha-glucosidase inhibitors with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1502855,1502809,1597756,1559684,1594973,1560171, #Sulfonylureas
      1510202 #alpha-glucosidase inhibitors
    )
  ),
  ########SGLT2is with metformin AS Target######
  # list(
  #   targetId = 11726 , #SGLT2is with metformin
  #   comparatorId = 11725, #Sulfonylureas with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     43526465,44785829,45774751,793293, #SGLT2is
  #     1502855,1502809,1597756,1559684,1594973,1560171 #Sulfonylureas
  #   )
  # ),
  # list(
  #   targetId = 11726 , #SGLT2is with metformin
  #   comparatorId = 11724, ##thiazolidinediones with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     43526465,44785829,45774751,793293, #SGLT2is
  #     1525215 #thiazolidinediones
  #   )
  # ),
  list(
    targetId = 11726 , #SGLT2is with metformin
    comparatorId = 11727 , #meglitinides with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      43526465,44785829,45774751,793293, #SGLT2is
      1516766,1502826 #meglitinides
    )
  ), 
  list(
    targetId = 11726 , #SGLT2is with metformin
    comparatorId = 11728 , #GLP-1 RA with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      43526465,44785829,45774751,793293, #SGLT2is
      45774435,1583722,44506754,40170911,793143 #GLP-1 RAs
    )
  ),
  list(
    targetId = 11726 , #SGLT2is with metformin
    comparatorId = 11729, #GIP/GLP-RA dual agonists with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      43526465,44785829,45774751,793293, #SGLT2is
      779705 #GIP/GLP-RA dual agonists
    )
  ),
  list(
    targetId = 11726 , #SGLT2is with metformin
    comparatorId = 11730, #DPP4s with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      43526465,44785829,45774751,793293, #SGLT2is
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 11726 , #SGLT2is with metformin
    comparatorId = 11713, #alpha-glucosidase inhibitors with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      43526465,44785829,45774751,793293, #SGLT2is
      1510202 #alpha-glucosidase inhibitors
    )
  ),
  ########GLP-1 RAs with metformin AS Target######
  # list(
  #   targetId = 11728 , #GLP-1 RA with metformin
  #   comparatorId = 11725, #Sulfonylureas with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     45774435,1583722,44506754,40170911,793143, #GLP-1 RAs
  #     1502855,1502809,1597756,1559684,1594973,1560171 #Sulfonylureas
  #   )
  # ),
  # list(
  #   targetId = 11728 , #GLP-1 RA with metformin
  #   comparatorId = 11724, ##thiazolidinediones with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     45774435,1583722,44506754,40170911,793143, #GLP-1 RAs
  #     1525215 #thiazolidinediones
  #   )
  # ),
  list(
    targetId = 11728 , #GLP-1 RA with metformin
    comparatorId = 11727 , #meglitinides with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      45774435,1583722,44506754,40170911,793143, #GLP-1 RAs
      1516766,1502826 #meglitinides
    )
  ), 
  # list(
  #   targetId = 11728 , #GLP-1 RA with metformin
  #   comparatorId = 11726 , #SGLT2is with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     45774435,1583722,44506754,40170911,793143, #GLP-1 RAs
  #     43526465,44785829,45774751,793293 #SGLT2is
  #   )
  # ),
  list(
    targetId = 11728 , #GLP-1 RA with metformin
    comparatorId = 11729, #GIP/GLP-RA dual agonists with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      45774435,1583722,44506754,40170911,793143, #GLP-1 RAs
      779705 #GIP/GLP-RA dual agonists
    )
  ),
  list(
    targetId = 11728 , #GLP-1 RA with metformin
    comparatorId = 11730, #DPP4s with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      45774435,1583722,44506754,40170911,793143, #GLP-1 RAs
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 11728 , #GLP-1 RA with metformin
    comparatorId = 11713, #alpha-glucosidase inhibitors with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      45774435,1583722,44506754,40170911,793143, #GLP-1 RAs
      1510202 #alpha-glucosidase inhibitors
    )
  ),
  #########GIP/GLP-RA dual agonists with metformin AS Target######
  # list(
  #   targetId = 11729, #GIP/GLP-RA dual agonists with metformin
  #   comparatorId = 11725, #Sulfonylureas with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     779705, #GIP/GLP-RA dual agonists
  #     1502855,1502809,1597756,1559684,1594973,1560171 #Sulfonylureas
  #   )
  # ),
  # list(
  #   targetId = 11729, #GIP/GLP-RA dual agonists with metformin
  #   comparatorId = 11724, ##thiazolidinediones with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     779705, #GIP/GLP-RA dual agonists
  #     1525215 #thiazolidinediones
  #   )
  # ),
  list(
    targetId = 11729, #GIP/GLP-RA dual agonists with metformin
    comparatorId = 11727 , #meglitinides with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      779705, #GIP/GLP-RA dual agonists
      1516766,1502826 #meglitinides
    )
  ), 
  # list(
  #   targetId = 11729, #GIP/GLP-RA dual agonists with metformin
  #   comparatorId = 11726 , #SGLT2is with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     779705, #GIP/GLP-RA dual agonists
  #     43526465,44785829,45774751,793293 #SGLT2is
  #   )
  # ),
  # list(
  #   targetId = 11729, #GIP/GLP-RA dual agonists with metformin
  #   comparatorId = 11728 , #GLP-1 RA with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     779705, #GIP/GLP-RA dual agonists
  #     45774435,1583722,44506754,40170911,793143 #GLP-1 RAs
  #   )
  # ),
  list(
    targetId = 11729, #GIP/GLP-RA dual agonists with metformin
    comparatorId = 11730, #DPP4s with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      779705, #GIP/GLP-RA dual agonists
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 11729, #GIP/GLP-RA dual agonists with metformin
    comparatorId = 11713, #alpha-glucosidase inhibitors with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      779705, #GIP/GLP-RA dual agonists
      1510202 #alpha-glucosidase inhibitors
    )
  ), 
  #########meglitinides with metformin AS Target######
  # list(
  #   targetId = 11727 , #meglitinides with metformin
  #   comparatorId = 11725, #Sulfonylureas with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1516766,1502826, #meglitinides
  #     1502855,1502809,1597756,1559684,1594973,1560171 #Sulfonylureas
  #   )
  # ),
  # list(
  #   targetId = 11727 , #meglitinides with metformin
  #   comparatorId = 11724, ##thiazolidinediones with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1516766,1502826, #meglitinides
  #     1525215 #thiazolidinediones
  #   )
  # ),
  # list(
  #   targetId = 11727 , #meglitinides with metformin
  #   comparatorId = 11729, #GIP/GLP-RA dual agonists with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1516766,1502826, #meglitinides
  #     779705 #GIP/GLP-RA dual agonists
  #   )
  # ), 
  # list(
  #   targetId = 11727 , #meglitinides with metformin
  #   comparatorId = 11726 , #SGLT2is with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1516766,1502826, #meglitinides
  #     43526465,44785829,45774751,793293 #SGLT2is
  #     
  #   )
  # ),
  # list(
  #   targetId = 11727 , #meglitinides with metformin
  #   comparatorId = 11728 , #GLP-1 RA with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1516766,1502826, #meglitinides
  #     45774435,1583722,44506754,40170911,793143 #GLP-1 RAs
  #   )
  # ),
  list(
    targetId = 11727 , #meglitinides with metformin
    comparatorId = 11730, #DPP4s with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1516766,1502826, #meglitinides
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 11727 , #meglitinides with metformin
    comparatorId = 11713, #alpha-glucosidase inhibitors with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1516766,1502826, #meglitinides
      1510202 #alpha-glucosidase inhibitors
    )
  ),
  ##########alpha-glucosidase inhibitors with metformin AS Target######
  # list(
  #   targetId = 11713, #alpha-glucosidase inhibitors with metformin
  #   comparatorId = 11725, #Sulfonylureas with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1510202, #alpha-glucosidase inhibitors
  #     1502855,1502809,1597756,1559684,1594973,1560171 #Sulfonylureas
  #   )
  # ),
  # list(
  #   targetId = 11713, #alpha-glucosidase inhibitors with metformin
  #   comparatorId = 11724, ##thiazolidinediones with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1510202, #alpha-glucosidase inhibitors
  #     1525215 #thiazolidinediones
  #     
  #   )
  # ),
  # list(
  #   targetId = 11713, #alpha-glucosidase inhibitors with metformin
  #   comparatorId = 11729, #GIP/GLP-RA dual agonists with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1510202, #alpha-glucosidase inhibitors
  #     779705 #GIP/GLP-RA dual agonists
  #   )
  # ), 
  # list(
  #   targetId = 11713, #alpha-glucosidase inhibitors with metformin
  #   comparatorId = 11726 , #SGLT2is with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1510202, #alpha-glucosidase inhibitors
  #     43526465,44785829,45774751,793293 #SGLT2is
  #   )
  # ),
  # list(
  #   targetId = 11713, #alpha-glucosidase inhibitors with metformin
  #   comparatorId = 11728 , #GLP-1 RA with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1510202, #alpha-glucosidase inhibitors
  #     45774435,1583722,44506754,40170911,793143 #GLP-1 RAs
  #   )
  # ),
  list(
    targetId = 11713, #alpha-glucosidase inhibitors with metformin
    comparatorId = 11730, #DPP4s with metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1510202, #alpha-glucosidase inhibitors
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  # list(
  #   targetId = 11713, #alpha-glucosidase inhibitors with metformin
  #   comparatorId = 11727 , #meglitinides with metformin
  #   indicationId = 11708, #T2DM
  #   excludedCovariateConceptIds = c(
  #     1510202, #alpha-glucosidase inhibitors
  #     1516766,1502826 #meglitinides
  #   )
  # ),
  ##########Biguanides AS Target (uses second set of drug cohorts)######
  list(
    targetId = 11716 , #bigaunides 
    comparatorId = 11722, #Sulfonylureas w/o metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      1502855,1502809,1597756,1559684,1594973,1560171 #Sulfonylureas
    )
  ),
  list(
    targetId = 11716 , #bigaunides 
    comparatorId = 11723, ##thiazolidinediones w/o metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      1525215 #thiazolidinediones
    )
  ),
  list(
    targetId = 11716 , #bigaunides 
    comparatorId = 11718, #GIP/GLP-RA dual agonists w/o metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      779705 #GIP/GLP-RA dual agonists
    )
  ), 
  list(
    targetId = 11716 , #bigaunides 
    comparatorId = 11721 , #SGLT2is w/o metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      43526465,44785829,45774751,793293 #SGLT2is
    )
  ),
  list(
    targetId = 11716 , #bigaunides 
    comparatorId = 11719 , #GLP-1 RA w/o metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      45774435,1583722,44506754,40170911,793143 #GLP-1 RAs
    )
  ),
  list(
    targetId = 11716 , #bigaunides 
    comparatorId = 11717, #DPP4s w/o metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      1580747,40166035,40239216,43013884 #DPP4s
    )
  ),
  list(
    targetId = 11716 , #bigaunides 
    comparatorId = 11720 , #meglitinides w/o metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      1516766,1502826 #meglitinides
    )
  ),
  list(
    targetId = 11716 , #bigaunides 
    comparatorId = 11711 , #alpha-glucosidase inhibitors w/o metformin
    indicationId = 11708, #T2DM
    excludedCovariateConceptIds = c(
      1503297, #bigaunides
      1510202 #alpha-glucosidase inhibitors
    )
  )
)##corresponds to opening parens

# These are the cohorts we'd like to use as subgroups for all T/Cs
cohortSubsets <- c(# 11746, 11710, 11732, 11745, 11733, 
                   11739, 11979, 11980) #11741, 11743, 11737, 11735) ######this is ideally where we want to reference the cohorts of statifying factors

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
  cohortId = c(11753, 11706, 11756, 11758, 11752, 11749, 11757, 11755, 11751, 11750, 11748),  
  cleanWindow = c(365, 365, 365, 365, 365, 365, 365, 365, 365, 365, 365)
)

# Time-at-risks (TARs) for the outcomes of interest in your study
##TF - do the number of elements in the TAR vectors need to correspon to the number of outcomes listed or the number of analyses in the 'list of lists'?
timeAtRisks <- tibble(
  label = c("On treatment", "On treatment"),
  riskWindowStart  = c(1, 1),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0, 0),
  endAnchor = c("cohort end", "cohort end")
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
  subsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "",
    definitionId = j + 20,
    subsetOperators = subsetOperators,
    identifierExpression = "targetId * 10 + definitionId"
  )
  
  cohortDefinitionSet <- cohortDefinitionSet %>%
    CohortGenerator::addCohortSubsetDefinition(
      cohortSubsetDefintion = subsetDef,
      targetCohortIds = targetCohortsWithIndicationIds$cohortId
    ) 
  
 # ## You DON'T belong to the subset ####propose getting rid of lines 232-254 (keep the bracket)
  subsetOperators <- list()
  subsetOperators[[length(subsetOperators) + 1]] <- CohortGenerator::createCohortSubset(
    cohortIds = cohortSubsets[j],
    negate = TRUE, ###negate = true means 'give me the complement of this cohort'
    cohortCombinationOperator = "all",
    startWindow = CohortGenerator::createSubsetCohortWindow(-99999, 0, "cohortStart"),
    endWindow = CohortGenerator::createSubsetCohortWindow(0, 99999, "cohortStart")
  )

  # Also create restricted version of indication cohort:
  subsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "",
    definitionId = j + 30,
    subsetOperators = subsetOperators,
    identifierExpression = "targetId * 10 + definitionId"
  )

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
  
  # Also create restricted version of indication cohort:
  subsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "",
    definitionId = j + 40,
    subsetOperators = subsetOperators,
    identifierExpression = "targetId * 10 + definitionId"
  )
  
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
  subsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "",
    definitionId = j + 50,
    subsetOperators = subsetOperators,
    identifierExpression = "targetId * 10 + definitionId"
  )
  
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
  # Get the subset definition ID that matches
  # the target ID. The comparator will also use the same subset
  # definition ID
  currentSubsetDefinitionId <- dfUniqueTcis %>%
    filter(cohortId == tci$targetId &
             indicationId == paste(tci$indicationId, collapse = ",")) %>%
    pull(subsetDefinitionId)
  targetId <- cohortDefinitionSet %>%
    filter(subsetParent == tci$targetId & subsetDefinitionId == currentSubsetDefinitionId) %>%
    pull(cohortId)
  comparatorId <- cohortDefinitionSet %>% 
    filter(subsetParent == tci$comparatorId & subsetDefinitionId == currentSubsetDefinitionId) %>%
    pull(cohortId)
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = targetId,
    comparatorId = comparatorId,
    outcomes = outcomeList,
    excludedCovariateConceptIds = tci$excludedCovariateConceptIds
  )
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
  file.path("inst", "cuimc_rev", "Obesity_Cohort_Analysis_Specification_V2.json")
)