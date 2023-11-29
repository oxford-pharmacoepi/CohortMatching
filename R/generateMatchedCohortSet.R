#' Generate a new cohort matched cohort from a preexisting target cohort. The
#' new cohort will contain individuals not included in the target cohort with
#' same year of birth (matchYearOfBirth = TRUE) and same sex (matchSex = TRUE).
#'
#' @param cdm A cdm reference object.
#' @param name Name of the new generated cohort set.
#' @param targetCohortName Name of the target cohort to match.
#' @param targetCohortId Cohort definition id to match from the target cohort.
#' If NULL all the cohort definition id present in the target cohort will be
#' matched.
#' @param matchSex Whether to match in sex.
#' @param matchYearOfBirth Whether to match in year of birth.
#' @param ratio Number of allowed matches per individual in the target cohort.
#'
#' @return A cdm reference object that contains the new generated cohort set.
#'
#' @export
#'
generateMatchedCohortSet <- function(cdm,
                                  name,
                                  targetCohortName,
                                  targetCohortId = NULL,
                                  matchSex = TRUE,
                                  matchYearOfBirth = TRUE,
                                  ratio = 1){
  # validate initial input
  validateInput(
    cdm = cdm, name = name, targetCohortName = targetCohortName,
    targetCohortId = targetCohortId, matchSex = matchSex,
    matchYearOfBirth = matchYearOfBirth, ratio = ratio
  )

  # table prefix
  tablePrefix <- randomPrefix()

  cohort <- cdm[[targetCohortName]]

  # get the number of cohorts
  n <- getNumberOfCohorts(cohort)

  # create temporal cases and controls cohorts
  cdm <- createTemporalCohorts(cdm, n, targetCohortName, tablePrefix)

  # get targetCohortId
  var <- getTargetCohortId(cohort, targetCohortId)
  cohort_set_ref <- var$cohort_set_ref
  targetCohortId <- var$targetCohortId

  # cols to match
  matchCols <- getMatchCols(matchSex, matchYearOfBirth)

  # get cases
  cases <- getCases(cdm, matchCols, cohort)

  # get controls
  var <- getControls(cdm, matchCols, cohort, targetCohortId, tablePrefix, n)
  cdm      <- var$cdm
  controls <- var$controls

  # match pairs
  var <- matchPairs(cdm, cases, controls, cohort, matchCols, ratio, n, targetCohortId, tablePrefix)
  cdm      <- var$cdm
  matches1 <- var$matches1

  # output a cohort object
  cdm <- getOutputCohortObject(cdm, matches1, cohort_set_ref, name, matchSex, matchYearOfBirth, tablePrefix, n)

  # Delete permanent tables
  CDMConnector::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  # Return
  return(cdm)
}

#' @noRd
validateInput <- function(cdm,
                          name,
                          targetCohortName,
                          targetCohortId,
                          matchSex,
                          matchYearOfBirth,
                          ratio) {
  errorMessage <- checkmate::makeAssertCollection()
  # Check cdm class
  data_check   <- any("cdm_reference" == class(cdm))
  checkmate::assertTRUE(data_check, add = errorMessage)
  if(!isTRUE(data_check)){
    errorMessage$push(glue::glue("- cdm input must be a cdm object"))
  }
  # Check if targetCohortName is a character
  targetCohortName_format_check <- any(class(targetCohortName) %in% c("character"))
  checkmate::assertTRUE(targetCohortName_format_check, add = errorMessage)
  if(!isTRUE(targetCohortName_format_check)){
    errorMessage$push(glue::glue("- targetCohortName input must be a string"))
  }
  # Check if targetCohortName length
  targetCohortName_length_check <- length(targetCohortName) == 1
  checkmate::assertTRUE(  targetCohortName_length_check, add = errorMessage)
  if(!isTRUE(  targetCohortName_length_check)){
    errorMessage$push(glue::glue("- targetCohortName input must have length equal to 1"))
  }
  # Check if targetCohortName is within the cdm object
  targetCohortName_check <- targetCohortName %in% names(cdm)
  checkmate::assertTRUE(targetCohortName_check, add = errorMessage)
  if(!isTRUE(targetCohortName_check)){
    errorMessage$push(glue::glue("- cdm input has not table named {targetCohortName}"))
  }
  # Check if observation period is within the cdm object
  observation_period_check <- "observation_period" %in% names(cdm)
  checkmate::assertTRUE(observation_period_check , add = errorMessage)
  if(!isTRUE(observation_period_check)){
    errorMessage$push(glue::glue("- cdm input has not table named 'observation_period'"))
  }
  # Check if targetCohortId is a numeric value
  if(!is.null(targetCohortId)){
    targetCohortId_format_check <- any(class(targetCohortId) %in% c("numeric","double","integer"))
    checkmate::assertTRUE(targetCohortId_format_check, add = errorMessage)
    if(!isTRUE(targetCohortId_format_check)){
      errorMessage$push(glue::glue("- targetCohortId input must be numeric"))
    }
  }
  # Check if targetCohortId is in the cohort_definition_id
  if(!is.null(targetCohortId)){
    rows <- cdm[[targetCohortName]] %>% dplyr::filter(.data$cohort_definition_id %in% targetCohortId) %>% dplyr::tally() %>% dplyr::pull()
    targetCohortId_check <- rows != 0
    checkmate::assertTRUE(targetCohortId_check, add = errorMessage)
    if(!isTRUE(targetCohortId_check)){
      errorMessage$push(glue::glue("- {name} table does not containg '{targetCohortId}' as a cohort_definition_id"))
    }
  }
  checkmate::reportAssertions(collection = errorMessage)
  return(invisible(TRUE))
}


randomPrefix <- function(n = 5) {
  paste0(
    "temp_", paste0(sample(letters, 5, TRUE), collapse = ""), "_", collapse = ""
  )
}

getNumberOfCohorts <- function(cohort){
  # Read number of cohorts
  n <- cohort %>%
    dplyr::summarise(v = max(.data$cohort_definition_id)) %>%
    dplyr::pull(.data$v) # number of different cohorts

  if(is.na(n)){# Empty table, number of cohorts is 0
    n <- 0
  }
  return(n)
}

createTemporalCohorts <- function(cdm, n, targetCohortName, tablePrefix){
  if(n == 0){
    cdm[[paste0(tablePrefix,"controls")]] <- cdm[[targetCohortName]]
    cdm[[paste0(tablePrefix,"cases")]]    <- cdm[[targetCohortName]]

  }else{
    # Create controls cohort
    controls_table <- lapply(n+(1:n), function(x) {
      cdm$person %>%
        dplyr::select("subject_id" = "person_id") %>%
        dplyr::mutate("cohort_definition_id" = .env$x)
    })

    controls_table <- Reduce(dplyr::union_all, controls_table) %>%
      dplyr::mutate("cohort_start_date" = NA,
                    "cohort_end_date"   = NA) %>%
      CDMConnector::computeQuery()

    cdm[[paste0(tablePrefix,"controls")]] <- CDMConnector::newGeneratedCohortSet(controls_table)

    newAttrition <- CDMConnector::cohort_attrition(cdm[[paste0(tablePrefix,"controls")]]) %>%
      dplyr::mutate(reason = "Subjects in the database")

    cdm[[paste0(tablePrefix,"controls")]] <- CDMConnector::newGeneratedCohortSet(
      cohortRef          = controls_table,
      cohortAttritionRef = newAttrition,
      overwrite          = TRUE
    )

    # Create cases cohort
    cdm[[paste0(tablePrefix,"cases")]]    <- cdm[[targetCohortName]]
  }

  return(cdm)
}


getTargetCohortId <- function(cohort, targetCohortId){
  # Participants with the targetCohortId
  if(!is.null(targetCohortId)){
    # Select participants with the targetCohortId of interest
    cohort <- cohort %>%
      dplyr::filter(.data$cohort_definition_id %in% targetCohortId)
    cohort_set_ref    <- CDMConnector::cohortSet(cohort) %>%
      dplyr::arrange(.data$cohort_definition_id)
  }else{
    # If "targetCohortId" is null, we will do it for all the existing targetCohortId
    targetCohortId    <- CDMConnector::cohortSet(cohort) %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::pull()

    cohort_set_ref    <- CDMConnector::cohortSet(cohort) %>%
      dplyr::arrange(.data$cohort_definition_id)
    targetCohortId    <- cohort_set_ref$cohort_definition_id
  }
  return(list("cohort_set_ref" = cohort_set_ref, "targetCohortId" = targetCohortId))
}


getMatchCols <- function(matchSex, matchYearOfBirth){
  # Obtain matched columns
  matchCols <- c()
  if(matchSex){
    matchCols <- append(matchCols, "gender_concept_id")
  }
  if(matchYearOfBirth){
    matchCols <- append(matchCols, "year_of_birth")
  }
  return(matchCols)
}


getCases <- function(cdm, matchCols, cohort){
  cases <- cdm$person %>%
    dplyr::select("person_id", dplyr::any_of(.env$matchCols)) %>%
    dplyr::right_join(
      cohort %>%
        dplyr::select("person_id" = "subject_id", "cohort_definition_id"),
      by = "person_id"
    ) %>%
    dplyr::rename("cases_id" = "person_id")

  return(cases)
}


getControls <- function(cdm, matchCols, cohort, targetCohortId, tablePrefix, n){

  for(targetCohortId_i in targetCohortId){
    controls_i <- cdm$person %>%
      dplyr::select("person_id", dplyr::any_of(.env$matchCols)) %>%
      dplyr::anti_join(
        cohort %>%
          dplyr::filter(.data$cohort_definition_id == targetCohortId_i) %>%
          dplyr::select("person_id" = "subject_id"),
        by = "person_id"
      ) %>%
      dplyr::mutate("cohort_definition_id" = targetCohortId_i) %>%
      dplyr::rename("controls_id" = "person_id")

    if(targetCohortId_i == targetCohortId[[1]]){
      controls <- controls_i
    }else{
      controls <- controls %>%
        dplyr::union_all(controls_i)
    }
  }
  # Attrition set-up - Controls
  cdm[[paste0(tablePrefix,"controls")]] <- cdm[[paste0(tablePrefix,"controls")]] %>%
    dplyr::inner_join(
      controls %>%
        dplyr::select("subject_id" = "controls_id", "cohort_definition_id") %>%
        dplyr::mutate("cohort_definition_id" = .data$cohort_definition_id+.env$n),
      by = c("subject_id", "cohort_definition_id")
    ) %>%
    CDMConnector::record_cohort_attrition("Exclude cases",
                                          cohortId = c(targetCohortId,targetCohortId+n))

  return(list("cdm" = cdm, "controls" = controls))
}



matchPairs <- function(cdm, cases, controls, cohort, matchCols, ratio, n, targetCohortId, tablePrefix){

  # Matching - if there is more than one match, choose one pair at random
  cases1 <- cases %>%
    dplyr::mutate(id = dbplyr::sql("random()")) %>%
    dplyr::group_by(.data$cohort_definition_id, dplyr::across(.env$matchCols)) %>%
    dbplyr::window_order(.data$id) %>%
    dplyr::mutate(pair_id = dplyr::row_number()) %>%
    dbplyr::window_order() %>%
    dplyr::select(-"id") %>%
    CDMConnector::computeQuery()

  controls1 <- controls %>%
    dplyr::mutate(id = dbplyr::sql("random()")) %>%
    dplyr::group_by(.data$cohort_definition_id, dplyr::across(.env$matchCols)) %>%
    dbplyr::window_order(.data$id) %>%
    dplyr::mutate(pair_id = dplyr::row_number()) %>%
    dbplyr::window_order() %>%
    dplyr::select(-"id")%>%
    CDMConnector::computeQuery()

  check_nums <- controls1 %>% dplyr::ungroup() %>% dplyr::group_by(.data$cohort_definition_id) %>% dplyr::summarise(n_controls = dplyr::n()) %>%
    dplyr::left_join(
      cases1 %>% dplyr::ungroup() %>% dplyr::group_by(.data$cohort_definition_id) %>% dplyr::summarise(n_cases = dplyr::n()),
      by = "cohort_definition_id"
    ) %>%
    dplyr::mutate(state = .data$n_controls > .data$n_cases) %>%
    dplyr::filter(.data$state == FALSE)

  if(!is.na(nrow(check_nums))){
    warning("Number of cases in one of the cohorts is higher than number of controls. Ratio is set to 1.")
    ratio <- 1
  }

  matches_table <- cases1 %>%
    dplyr::inner_join(
      controls1,
      by = c("pair_id","cohort_definition_id", matchCols)
    )

  # If ratio is not one, it works, but is not necessary do it as matches_1 = matches_table
  if(ratio != 1){
    matches_table_1 <- matches_table %>%
      dplyr::mutate(max_cases = max(.data$pair_id)) %>%
      dplyr::select(-"cases_id") %>%
      dplyr::right_join(
        controls1,
        by = c("pair_id", "cohort_definition_id", "gender_concept_id", "year_of_birth", "controls_id")
      ) %>%
      dplyr::mutate(max_cases =
                      dplyr::if_else(is.na(.data$max_cases), max(.data$max_cases, na.rm = TRUE), .data$max_cases)
      ) %>%
      dplyr::filter(!is.na(.data$max_cases)) # No matching
    if(ratio == "Inf"){
      matches_table_1 <- matches_table_1 %>%
        dplyr::mutate(ratio = max(.data$pair_id)/.data$max_cases)
    }else{
      matches_table_1 <- matches_table_1 %>%
        dplyr::mutate(ratio = .env$ratio)
    }

    matches_table_1 <- matches_table_1 %>%
      dplyr::mutate(pair_id1 =
                      dplyr::if_else(.data$max_cases < .data$pair_id,
                                     .data$pair_id - .data$max_cases*(.data$ratio-1),
                                     .data$pair_id)) %>%
      dplyr::mutate(pair_id = .data$pair_id1) %>%
      dplyr::select(-"pair_id1", -"max_cases", -"ratio") %>%
      dplyr::inner_join(
        cases1,
        by = c("pair_id", "cohort_definition_id", matchCols)
      ) %>%
      dplyr::union_all(
        matches_table
      ) %>%
      dplyr::distinct()

    matches_table <-  matches_table_1 %>%
      CDMConnector::compute_query()
  }else{
    matches_table <-  matches_table %>%
      CDMConnector::compute_query()
  }


  #  Attrition set-up - Controls
  cdm[[paste0(tablePrefix,"controls")]] <- cdm[[paste0(tablePrefix,"controls")]] %>%
    dplyr::inner_join(
      matches_table %>%
        dplyr::ungroup() %>%
        dplyr::select("subject_id" = "controls_id", "cohort_definition_id") %>%
        dplyr::mutate("cohort_definition_id" = .data$cohort_definition_id+n),
      by = c("subject_id", "cohort_definition_id")
    ) %>%
    CDMConnector::record_cohort_attrition("Subjects with matched pair",
                                          cohortId = c(targetCohortId+n))

  #  Attrition set-up - Cases
  cdm[[paste0(tablePrefix,"cases")]] <- cdm[[paste0(tablePrefix,"cases")]] %>%
    dplyr::inner_join(
      matches_table %>%
        dplyr::ungroup() %>%
        dplyr::select("subject_id" = "cases_id", "cohort_definition_id"),
      by = c("subject_id", "cohort_definition_id")
    ) %>%
    CDMConnector::record_cohort_attrition("Subjects with matched pair",
                                          cohortId = c(targetCohortId))

  matches1 <-  matches_table %>%
    # Remove those pairs where the control individual was not in observation at index date
    dplyr::left_join(
      cdm$observation_period %>%
        dplyr::select("controls_id" = "person_id", "observation_period_start_date", "observation_period_end_date"),
      by = "controls_id"
    ) %>%
    dplyr::left_join(
      cohort %>%
        dplyr::select("cases_id" = "subject_id", "cohort_start_date","cohort_end_date","cohort_definition_id"),
      by = c("cases_id","cohort_definition_id")
    ) %>%
    dplyr::filter(
      .data$cohort_start_date >= .data$observation_period_start_date,
      .data$cohort_start_date <= .data$observation_period_end_date
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("cases_id", "controls_id", "cohort_definition_id", "cohort_start_date","cohort_end_date")

  #  Attrition set-up - Controls
  cdm[[paste0(tablePrefix,"controls")]] <- cdm[[paste0(tablePrefix,"controls")]] %>%
    dplyr::inner_join(
      matches1 %>%
        dplyr::select("subject_id" = "controls_id", "cohort_definition_id") %>%
        dplyr::mutate("cohort_definition_id" = .data$cohort_definition_id+n),
      by = c("subject_id", "cohort_definition_id")
    ) %>%
    CDMConnector::record_cohort_attrition("Pair in observation during the index date",
                                          cohortId = c(targetCohortId+n))

  #  Attrition set-up - Cases
  cdm[[paste0(tablePrefix,"cases")]] <- cdm[[paste0(tablePrefix,"cases")]] %>%
    dplyr::inner_join(
      matches1 %>%
        dplyr::select("subject_id" = "cases_id", "cohort_definition_id"),
      by = c("subject_id", "cohort_definition_id")
    ) %>%
    CDMConnector::record_cohort_attrition("Pair in observation during the index date",
                                          cohortId = c(targetCohortId))

  return(list("cdm" = cdm, "matches1" = matches1))
}


getOutputCohortObject <- function(cdm, matches1, cohort_set_ref, name, matchSex, matchYearOfBirth, tablePrefix, n){

  cohort_ref <- matches1 %>%
    dplyr::select(-"controls_id") %>%
    dplyr::rename("subject_id" = "cases_id") %>%
    dplyr::union_all(
      matches1 %>%
        dplyr::select(-"cases_id") %>%
        dplyr::rename("subject_id" = "controls_id") %>%
        dplyr::mutate(cohort_definition_id = .data$cohort_definition_id + 0.5)
    )

  if(n != 0){
    cohort_set_ref <- cohort_set_ref %>%
      dplyr::select("cohort_definition_id", "cohort_name") %>%
      dplyr::union_all(
        cohort_set_ref %>%
          dplyr::select("cohort_definition_id", "cohort_name") %>%
          dplyr::mutate(cohort_definition_id = .data$cohort_definition_id + 0.5) %>%
          dplyr::mutate(cohort_name = paste0(.data$cohort_name,"_matched"))
      ) %>%
      dplyr::mutate(target_cohort_name = .env$name) %>%
      dplyr::mutate(match_sex          = .env$matchSex) %>%
      dplyr::mutate(math_year_of_birth = .env$matchYearOfBirth) %>%
      dplyr::mutate(match_status       = dplyr::if_else(grepl(".5", .data$cohort_definition_id),
                                                        "matched",
                                                        "target")) %>%
      dplyr::mutate("target_cohort_id" = floor(.data$cohort_definition_id)) %>%
      dplyr::mutate("cohort_definition_id" = dplyr::if_else(grepl(".5", .data$cohort_definition_id),
                                                            .data$cohort_definition_id+.env$n-0.5,
                                                            .data$cohort_definition_id))
    cohort_ref <- cohort_ref %>%
      dplyr::mutate(cohort_definition_id = dplyr::if_else(grepl(".5", .data$cohort_definition_id),
                                                          .data$cohort_definition_id+.env$n-0.5,
                                                          .data$cohort_definition_id)) %>%
      CDMConnector::compute_query(temporary = FALSE,
                                  schema    = attr(cdm, "write_schema"),
                                  name      = name,
                                  overwrite = TRUE)

  }else{
    cohort_ref <- cohort_ref %>%
      CDMConnector::compute_query(temporary = FALSE,
                                  schema    = attr(cdm, "write_schema"),
                                  name      = name,
                                  overwrite = TRUE)

  }

  # Set-up attrition
  attrition <- CDMConnector::cohort_attrition(cdm[[paste0(tablePrefix,"controls")]]) %>%
    dplyr::union_all(
      CDMConnector::cohort_attrition(cdm[[paste0(tablePrefix,"cases")]])
    ) %>%
    dplyr::arrange(.data$cohort_definition_id)

  new_cohort <- CDMConnector::newGeneratedCohortSet(
    cohortRef    = cohort_ref,
    cohortSetRef = cohort_set_ref,
    cohortAttritionRef = attrition,
    overwrite    = TRUE
  )

  cdm[[name]] <- new_cohort

  return(cdm)
}
