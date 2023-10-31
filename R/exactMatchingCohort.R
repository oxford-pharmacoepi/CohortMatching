#' @noRd
exactMatchingCohort <- function(cohort,matchSex = TRUE, matchYearBirth = TRUE, matchPair = 1){

# ERROR MESSAGES ---------------------------------------------------------------
  errorMessage <- checkmate::makeAssertCollection()
  # Check cohort class
  data_check   <- class(cohort) == "cdm_reference"
  checkmate::assertTRUE(data_check, add = errorMessage)
  if(!isTRUE(data_check)){
    errorMessage$push(glue::glue("- {cohort} is not a cdm object"))
  }
  # Check if cohort has person_id, observation_period, and cases tables "cases"
  person_check <- "person" %in% names(cohort)
  checkmate::assertTRUE(person_check, add = errorMessage)
  if(!isTRUE(person_check)){
    errorMessage$push(glue::glue("- {cohort} has not table named 'person'"))
  }
  observation_period_check <- "observation_period" %in% names(cohort)
  checkmate::assertTRUE(observation_period_check , add = errorMessage)
  if(!isTRUE(observation_period_check)){
    errorMessage$push(glue::glue("- {cohort} has not table named 'observation_period'"))
  }
  cases_check <- "cases" %in% names(cohort)
  checkmate::assertTRUE(cases_check, add = errorMessage)
  if(!isTRUE(cases_check)){
    errorMessage$push(glue::glue("- {cohort} has not table named 'cases'"))
  }

  checkmate::reportAssertions(collection = errorMessage)
# ------------------------------------------------------------------------------
  matchCols <- c()
  if(matchSex){
    matchCols <- append(matchCols, "gender_concept_id")
  }
  if(matchYearBirth){
    matchCols <- append(matchCols, "year_of_birth")
  }

  # Cases
  cases <- cohort$person %>%
    dplyr::select(.data$person_id, dplyr::all_of(.env$matchCols)) %>%
    dplyr::right_join(
      cohort$cases %>%
        dplyr::select(person_id = .data$subject_id),
      by = "person_id"
    ) %>%
    dplyr::rename(cases_id = .data$person_id)

  # Controls
  controls <- cohort$person %>%
    dplyr::select(.data$person_id, dplyr::all_of(.env$matchCols)) %>%
    dplyr::anti_join(
      cohort$cases %>%
        dplyr::select(person_id = .data$subject_id),
      by = "person_id"
    ) %>%
    dplyr::rename(controls_id = .data$person_id)

  # Matching - if there is more than one match, remove a row at random
  cases1 <- cases %>%
    dplyr::mutate(id = dbplyr::sql("random()")) %>%
    dplyr::group_by(dplyr::across(.env$matchCols)) %>%
    dbplyr::window_order(.data$id) %>%
    dplyr::mutate(pair_id = dplyr::row_number()) %>%
    dbplyr::window_order() %>%
    dplyr::select(-.data$id)

  controls1 <- controls %>%
    dplyr::mutate(id = dbplyr::sql("random()")) %>%
    dplyr::group_by(dplyr::across(.env$matchCols)) %>%
    dbplyr::window_order(.data$id) %>%
    dplyr::mutate(pair_id = dplyr::row_number()) %>%
    dbplyr::window_order() %>%
    dplyr::select(-.data$id)

  matches <- cases1 %>%
    dplyr::inner_join(controls1,
                      by = c("pair_id",dplyr::all_of(matchCols))
    ) %>%
    # Remove those pairs where the control individual was not in observation at index date
    dplyr::left_join(
      cohort$observation_period %>%
        dplyr::select(controls_id = .data$person_id, .data$observation_period_start_date, .data$observation_period_end_date),
      by = "controls_id"
    ) %>%
    dplyr::left_join(
      cohort$cases %>%
        dplyr::select(cases_id = .data$subject_id, index_date = .data$cohort_start_date),
      by = "cases_id"
    ) %>%
    dplyr::filter(
      .data$index_date >= .data$observation_period_start_date,
      .data$index_date <= .data$observation_period_end_date
    ) %>%
    dplyr::select(.data$cases_id, .data$controls_id, .data$gender_concept_id, .data$year_of_birth)
    return(matches)
}
