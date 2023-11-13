test_that("generateMatchedCohort", {
  # Run without errors (simple example, with one cohort)
  followback <- 180
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE)

  expect_no_error(generateMatchedCohort(cdm,
                                        name = "newCohort",
                                        targetCohortName = "cases"))

  # Run without errors (simple example, with two cohorts)
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009, other = 432526),
    name = "cohort",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )
  expect_no_error(generateMatchedCohort(cdm,
                                        name = "newCohort",
                                        targetCohortName = "cohort",
                                        targetCohortId = NULL,
                                        matchSex = TRUE,
                                        matchYearOfBirth = TRUE,
                                        ratio = 1))

})

test_that("generateMatchedCohort, no duplicated people within a cohort", {
  followback <- 180

  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009, other = 432526),
    name = "cohort",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )
  a <- generateMatchedCohort(cdm,
                             name = "newCohort",
                             targetCohortName = "cohort",
                             targetCohortId = NULL,
                             matchSex = TRUE,
                             matchYearOfBirth = TRUE,
                             ratio = 1)

  p1 <- a$NewCohort1 %>%
    dplyr::filter(cohort_name == "asthma") %>%
    dplyr::select(subject_id) %>%
    dplyr::pull() %>%
    length()
  expect_true(length(p1) == length(unique(p1)))

})
