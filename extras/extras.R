library(DrugUtilisation)

cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividuals = 200)

followback  <- 180

conceptSet_code <- list(asthma = 317009)
cdm <- DrugUtilisation::generateConceptCohortSet(cdm,
                                                 conceptSet = conceptSet_code,
                                                 name = "cases",
                                                 end  = "observation_period_end_date",
                                                 requiredObservation = c(followback,followback),
                                                 overwrite = TRUE)

matchSex <- TRUE
matchYearBirth <- TRUE
matchPair <- 1


# 1. trobar a person table els matches -----------------------------------------
matchCols <- c()
if(matchSex){
  matchCols <- append(matchCols, "gender_concept_id")
}
if(matchYearBirth){
  matchCols <- append(matchCols, "year_of_birth")
}

# Cases (People with asthma)
matches <- cdm$person %>%
  dplyr::select(person_id, all_of(matchCols)) %>%
  dplyr::right_join(
    cdm$cases %>%
      dplyr::select(person_id = subject_id),
    by = 'person_id') %>%
  dplyr::rename(cases_id = person_id) %>%
  dplyr::left_join( # to eliminate directly those ones that did not have a match
    # Controls (People without asthma)
    cdm$person %>%
      dplyr::select(person_id, all_of(matchCols)) %>%
      dplyr::anti_join(
        cdm$cases %>%
          dplyr::select(person_id = subject_id),
        by = 'person_id') %>%
      dplyr::rename(controls_id = person_id),
    # Match with year_of_birth and gender_concept_id
    by = matchCols)

# 2. assignar a cada parella el cohort start date de la cohort de athma (pensar com jugar amb duplicats)
# Duplicates: select one at random
duplicates <- matches %>%
  # Subset of controls duplicates:
  dplyr::filter(!is.na(controls_id)) %>%
  dplyr::group_by(controls_id) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::filter(n == 2)
if((duplicates %>% dplyr::ungroup() %>% dplyr::tally() %>% dplyr::pull()) != 0){
  duplicates <- duplicates %>%
    dplyr::mutate(val = 1) %>%
    dplyr::mutate(val = cumsum(val)) %>%
    dplyr::mutate(val1 = dplyr::if_else(val == 1,
                                        dbplyr::sql("random()"),
                                        0)) %>%
    dplyr::mutate(val1 = dplyr::if_else(val == 2,
                                        1-sum(val1),
                                        val1)) %>%
    dplyr::mutate(val1 = round(val1,digits = 0)) %>%
    dplyr::filter(val1 == 1) %>%
    dplyr::select(-n,-val,-val1) %>%
    dplyr::ungroup()

  matchingTable <- matches %>%
    dplyr::anti_join(duplicates, by = 'controls_id') %>%
    dplyr::union_all(duplicates)
}else{
  matchingTable <- matches
}

controlsTable <- matchingTable %>%
  dplyr::left_join(
    cdm$observation_period %>%
      dplyr::select(controls_id = person_id, observation_period_start_date, observation_period_end_date),
    by = 'controls_id'
  ) %>%
  dplyr::left_join(
    cdm$cases %>%
      dplyr::select(cases_id = subject_id,
                    index_date = cohort_start_date),
    by = 'cases_id'
  )

# 3. eliminar aquells que no estiguin en observation al index date
# index_date: cdm$cases, cohort start date
# observation period: controls

controlsTable <- controlsTable %>%
  dplyr::filter(
    index_date >= observation_period_start_date,
    index_date <= observation_period_end_date
  )


# 4. ens quedem amb un match 1:1 (matchPair)
matchingTable <- matchingTable %>%
  dplyr::inner_join(
    controlsTable %>%
      dplyr::select('cases_id','controls_id',all_of(matchCols)),
    by = c('cases_id','controls_id',matchCols),
  )






