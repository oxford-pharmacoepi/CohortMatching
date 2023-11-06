test_that("generateMatchedCohort", {
  # Run without errors (simple example, with one cohort)
  followback <- 180
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )

  expect_no_error(generateMatchedCohort(cdm,
                                        name = "NewCohort",
                                        targetCohortName = "cases"))

  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009, other = 432526),
    name = "cohort",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )
  expect_no_error(generateMatchedCohort(cdm,
                                        name = "NewCohort1",
                                        targetCohortName = "cohort",
                                        targetCohortId = NULL,
                                        matchSex = TRUE,
                                        matchYear = TRUE,
                                        ratio = 1))

})
