test_that("test match works", {
  cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200)
  followback  <- 180
  conceptSet_code <- list(asthma = 317009)
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = cdm,
    conceptSet = conceptSet_code,
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )

  expect_no_error(exactMatchingCohort(
    cohort = cdm$cases, matchSex = TRUE, matchYearBirth = TRUE, matchPair = 1
  ))

})
