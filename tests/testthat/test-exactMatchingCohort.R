test_that("test exactMatchingCohort works if all are true", {

  followback  <- 180
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )

  expect_no_error(
    exactMatchingCohort(
      cdm,
      name = "new_cohort",
      targetCohortName = "cases"
    )
  )

})

test_that("test exactMatchingCohort works if one is true and the other one false", {


  followback  <- 180
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )

  expect_no_error(
    exactMatchingCohort(
      cdm,
      name = "new_cohort",
      targetCohortName = "cases",
      matchSex = TRUE,
      matchYearOfBirth = FALSE
    )
  )

  expect_no_error(
    exactMatchingCohort(
      cdm,
      name = "new_cohort",
      targetCohortName = "cases",
      matchSex = FALSE,
      matchYearOfBirth = TRUE
    )
  )

  expect_no_error(
    exactMatchingCohort(
      cdm,
      name = "new_cohort",
      targetCohortName = "cases",
      matchSex = FALSE,
      matchYearOfBirth = FALSE
    )
  )


})

test_that("test exactMatchingCohort works if there are no subjects", {
  followback  <- 180
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(asthma = 317009),
    name = "cases",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )
  cdm$cases <- cdm$cases %>% dplyr::filter(subject_id == 0)
  expect_no_error(
    exactMatchingCohort(
      cdm,
      name = "new_cohort",
      targetCohortName = "cases",
    )
  )
})
