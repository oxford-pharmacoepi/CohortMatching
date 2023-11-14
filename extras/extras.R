rm(list = ls())
library("magrittr")
# Variables ====================================================================
cdm <- DrugUtilisation::generateConceptCohortSet(
  cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 100),
  conceptSet = list(asthma = 317009, other = 432526),
  name = "cases",
  end  = "observation_period_end_date",
  requiredObservation = c(180,180),
  overwrite = TRUE
)

name <- "new_cohort"
targetCohortName <- "cases"
targetCohortId <- NULL
matchSex <- TRUE
matchYearOfBirth <- TRUE
ratio <- 1




# Function =====================================================================

