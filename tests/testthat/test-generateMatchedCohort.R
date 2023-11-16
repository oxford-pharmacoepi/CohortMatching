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
                                        name = "new_cohort",
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
                                        name = "new_cohort",
                                        targetCohortName = "cohort",
                                        targetCohortId = 1,
                                        matchSex = TRUE,
                                        matchYearOfBirth = TRUE,
                                        ratio = 1))

  expect_no_error(generateMatchedCohort(cdm,
                                        name = "new_cohort",
                                        targetCohortName = "cohort",
                                        targetCohortId = NULL,
                                        matchSex = TRUE,
                                        matchYearOfBirth = TRUE,
                                        ratio = 1))

  expect_no_error(generateMatchedCohort(cdm,
                                        name = "new_cohort",
                                        targetCohortName = "cohort",
                                        targetCohortId = c(1,2),
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
                             name = "new_cohort",
                             targetCohortName = "cohort",
                             targetCohortId = NULL,
                             matchSex = TRUE,
                             matchYearOfBirth = TRUE,
                             ratio = 1)


  p1 <- a$new_cohort %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(subject_id) %>%
    dplyr::pull() %>%
    length()
  expect_true(length(p1) == length(unique(p1)))

})

test_that("check that we obtain expected result", {
  followback <- 180
  cdm <- DrugUtilisation::generateConceptCohortSet(
    cdm = DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200),
    conceptSet = list(c1 = 317009, c2 = 432526, c3 = 4141052),
    name = "cohort",
    end  = "observation_period_end_date",
    requiredObservation = c(followback,followback),
    overwrite = TRUE
  )

  matched_cohorts <- generateMatchedCohort(cdm,
                                           name = "new_cohort",
                                           targetCohortName = "cohort",
                                           targetCohortId = NULL,
                                           matchSex = TRUE,
                                           matchYearOfBirth = TRUE,
                                           ratio = 1)
  expect_true(
    length(CDMConnector::cohort_count(matched_cohorts[["new_cohort"]]) %>%
             dplyr::select("number_records") %>%
             dplyr::pull() %>%
             unique()) == 3
  )

  n <- matched_cohorts[["new_cohort"]] %>%
    dplyr::summarise(n = max(.data$cohort_definition_id)/2) %>%
    dplyr::pull()

  cohorts <- matched_cohorts[["new_cohort"]] %>%
    dplyr::select("person_id" = "subject_id", "cohort_definition_id") %>%
    dplyr::inner_join(
      matched_cohorts[["person"]] %>%
        dplyr::select("person_id", "gender_concept_id", "year_of_birth"),
      by = "person_id"
    )

  expect_true(is.na(nrow(cohorts %>%
    dplyr::filter(.data$cohort_definition_id %in% c(1,2,3)) %>%
    dplyr::left_join(
      cohorts %>%
        dplyr::filter(.data$cohort_definition_id %in% c(4,5,6)) %>%
        dplyr::mutate("cohort_definition_id" = .data$cohort_definition_id-n),
      by = c("cohort_definition_id", "gender_concept_id", "year_of_birth")
    ) %>%
    dplyr::filter(
      is.na(person_id.y)
    ))))





})
