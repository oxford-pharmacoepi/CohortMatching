#' @noRd
exactMatchingCohort <- function(cdm,
                                name,
                                targetCohortName,
                                targetCohortId = NULL,
                                matchSex  = TRUE,
                                matchYearOfBirth = TRUE,
                                ratio = 1){

  cohort <- cdm[[targetCohortName]]

  #  Attrition set-up
  n <- cohort %>%
    dplyr::summarise(v = max(.data$cohort_definition_id)) %>%
    dplyr::pull()

  if(is.na(n)){
    # Empty table
    n <- 0
    cdm[["controls_temporal_table"]] <- cdm[[targetCohortName]]
    cdm[["cases_temporal_table"]]    <- cdm[[targetCohortName]]
  }else{
    controls_table <- lapply(n+(1:n), function(x) {
      cdm$person %>%
        dplyr::select("subject_id" = "person_id") %>%
        dplyr::mutate("cohort_definition_id" = .env$x)
    })

    controls_table <- Reduce(dplyr::union_all, controls_table) %>%
      dplyr::mutate("cohort_start_date" = NA,
                    "cohort_end_date"   = NA) %>%
      CDMConnector::computeQuery()

    cdm[["controls_temporal_table"]] <- CDMConnector::newGeneratedCohortSet(controls_table)
    newAttrition <- CDMConnector::cohort_attrition(cdm[["controls_temporal_table"]]) %>%
      dplyr::mutate(reason = "Subjects in the database")
    cdm[["controls_temporal_table"]] <- CDMConnector::newGeneratedCohortSet(
      cohortRef          = controls_table,
      cohortAttritionRef = newAttrition,
      overwrite          = TRUE
    )
    cdm[["cases_temporal_table"]]    <- cdm[[targetCohortName]]
  }

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



  # Obtain matched columns
  matchCols <- c()
  if(matchSex){
    matchCols <- append(matchCols, "gender_concept_id")
  }
  if(matchYearOfBirth){
    matchCols <- append(matchCols, "year_of_birth")
  }

  # Cases
  cases <- cdm$person %>%
    dplyr::select("person_id", dplyr::any_of(.env$matchCols)) %>%
    dplyr::right_join(
      cohort %>%
        dplyr::select("person_id" = "subject_id", "cohort_definition_id"),
      by = "person_id"
    ) %>%
    dplyr::rename("cases_id" = "person_id")

  # Controls
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
  cdm[["controls_temporal_table"]] <- cdm[["controls_temporal_table"]] %>%
    dplyr::inner_join(
      controls %>%
        dplyr::select("subject_id" = "controls_id", "cohort_definition_id") %>%
        dplyr::mutate("cohort_definition_id" = .data$cohort_definition_id+n),
      by = c("subject_id", "cohort_definition_id")
    ) %>%
    CDMConnector::record_cohort_attrition("Exclude cases",
                                          cohortId = c(targetCohortId,targetCohortId+n))


  # Matching - if there is more than one match, choose one pair at random
  cases1 <- cases %>%
    dplyr::mutate(id = dbplyr::sql("random()")) %>%
    dplyr::group_by(.data$cohort_definition_id, dplyr::across(.env$matchCols)) %>%
    dbplyr::window_order(.data$id) %>%
    dplyr::mutate(pair_id = dplyr::row_number()) %>%
    dbplyr::window_order() %>%
    dplyr::select(-.data$id)

  controls1 <- controls %>%
    dplyr::mutate(id = dbplyr::sql("random()")) %>%
    dplyr::group_by(.data$cohort_definition_id, dplyr::across(.env$matchCols)) %>%
    dbplyr::window_order(.data$id) %>%
    dplyr::mutate(pair_id = dplyr::row_number()) %>%
    dbplyr::window_order() %>%
    dplyr::select(-.data$id)

  matches <- cases1 %>%
    dplyr::inner_join(
      controls1,
      by = c("pair_id","cohort_definition_id", matchCols)
    ) %>%
    CDMConnector::computeQuery()

  #  Attrition set-up - Controls
  cdm[["controls_temporal_table"]] <- cdm[["controls_temporal_table"]] %>%
    dplyr::inner_join(
      matches %>%
        dplyr::ungroup() %>%
        dplyr::select("subject_id" = "controls_id", "cohort_definition_id") %>%
        dplyr::mutate("cohort_definition_id" = .data$cohort_definition_id+n),
      by = c("subject_id", "cohort_definition_id")
    ) %>%
    CDMConnector::record_cohort_attrition("Subjects with matched pair",
                                          cohortId = c(targetCohortId+n))

  #  Attrition set-up - Cases
  cdm[["cases_temporal_table"]] <- cdm[["cases_temporal_table"]] %>%
    dplyr::inner_join(
      matches %>%
        dplyr::ungroup() %>%
        dplyr::select("subject_id" = "cases_id", "cohort_definition_id"),
      by = c("subject_id", "cohort_definition_id")
    ) %>%
    CDMConnector::record_cohort_attrition("Subjects with matched pair",
                                          cohortId = c(targetCohortId))

  matches1 <- matches %>%
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
  cdm[["controls_temporal_table"]] <- cdm[["controls_temporal_table"]] %>%
    dplyr::inner_join(
      matches1 %>%
        dplyr::select("subject_id" = "controls_id", "cohort_definition_id") %>%
        dplyr::mutate("cohort_definition_id" = .data$cohort_definition_id+n),
      by = c("subject_id", "cohort_definition_id")
    ) %>%
    CDMConnector::record_cohort_attrition("Pair in observation during the index date",
                                          cohortId = c(targetCohortId+n))

  #  Attrition set-up - Cases
  cdm[["cases_temporal_table"]] <- cdm[["cases_temporal_table"]] %>%
    dplyr::inner_join(
      matches1 %>%
        dplyr::select("subject_id" = "cases_id", "cohort_definition_id"),
      by = c("subject_id", "cohort_definition_id")
    ) %>%
    CDMConnector::record_cohort_attrition("Pair in observation during the index date",
                                          cohortId = c(targetCohortId))

  # Cohort object --------------------------------------------------------------
  cohort_ref <- matches1 %>%
    dplyr::select(-.data$controls_id) %>%
    dplyr::rename("subject_id" = "cases_id") %>%
    dplyr::union_all(
      matches1 %>%
        dplyr::select(-.data$cases_id) %>%
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
                                                            .data$cohort_definition_id+n-0.5,
                                                            .data$cohort_definition_id))
    cohort_ref <- cohort_ref %>%
      dplyr::mutate(cohort_definition_id = dplyr::if_else(grepl(".5", .data$cohort_definition_id),
                                                          .data$cohort_definition_id+n-0.5,
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
  attrition <- CDMConnector::cohort_attrition(cdm[["controls_temporal_table"]]) %>%
    dplyr::union_all(
      CDMConnector::cohort_attrition(cdm[["cases_temporal_table"]])
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

