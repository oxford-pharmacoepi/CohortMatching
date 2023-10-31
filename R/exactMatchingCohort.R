#' @noRd
exactMatchingCohort <- function(cohort, matchSex = TRUE, matchYearBirth = TRUE, matchPair = 1){

  cdm <- attr(cohort, "cdm_reference")

# ERROR MESSAGES ---------------------------------------------------------------
  # errorMessage <- checkmate::makeAssertCollection()
  # # Check cdm class
  # data_check   <- class(cdm) == "cdm_reference"
  # checkmate::assertTRUE(data_check, add = errorMessage)
  # if(!isTRUE(data_check)){
  #   errorMessage$push(glue::glue("- {cdm} is not a cdm object"))
  # }
  # # Check if cdm has person_id, observation_period, and cohort tables "cases"
  # person_check <- "person" %in% names(cdm)
  # checkmate::assertTRUE(person_check, add = errorMessage)
  # if(!isTRUE(person_check)){
  #   errorMessage$push(glue::glue("- {cdm} has not table named 'person'"))
  # }
  # observation_period_check <- "observation_period" %in% names(cdm)
  # checkmate::assertTRUE(observation_period_check , add = errorMessage)
  # if(!isTRUE(observation_period_check)){
  #   errorMessage$push(glue::glue("- {cdm} has not table named 'observation_period'"))
  # }
  # cases_check <- "cases" %in% names(cdm)
  # checkmate::assertTRUE(cases_check, add = errorMessage)
  # if(!isTRUE(cases_check)){
  #   errorMessage$push(glue::glue("- {cdm} has not table named 'cases'"))
  # }
  #
  # checkmate::reportAssertions(collection = errorMessage)

    matchCols <- c()
  if(matchSex){
    matchCols <- append(matchCols, "gender_concept_id")
  }
  if(matchYearBirth){
    matchCols <- append(matchCols, "year_of_birth")
  }

  # Cases
  cases <- cdm$person %>%
    dplyr::select("person_id", dplyr::all_of(.env$matchCols)) %>%
    dplyr::right_join(
      cohort %>%
        dplyr::select("person_id" = "subject_id"),
      by = "person_id"
    ) %>%
    dplyr::rename("cases_id" = "person_id")

  # Controls
  controls <- cdm$person %>%
    dplyr::select("person_id", dplyr::all_of(.env$matchCols)) %>%
    dplyr::anti_join(
      cohort %>%
        dplyr::select("person_id" = "subject_id"),
      by = "person_id"
    ) %>%
    dplyr::rename("controls_id" = "person_id")

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
      cdm$observation_period %>%
        dplyr::select("controls_id" = "person_id", "observation_period_start_date", "observation_period_end_date"),
      by = "controls_id"
    ) %>%
    dplyr::left_join(
      cohort %>%
        dplyr::select("cases_id" = "subject_id", "index_date" = "cohort_start_date"),
      by = "cases_id"
    ) %>%
    dplyr::filter(
      .data$index_date >= .data$observation_period_start_date,
      .data$index_date <= .data$observation_period_end_date
    ) %>%
    dplyr::select("cases_id", "controls_id", "gender_concept_id", "year_of_birth")
    return(matches)
}

