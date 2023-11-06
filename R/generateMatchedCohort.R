#' generateMatchedCohort summary
#'
#' @param cdm cdm object
#' @param name name of the new cohort
#' @param targetCohortName Name of the target cohort to match
#' @param targetCohortId cohort_definition_id of interest. If it is NULL, all the cohort_definition_id present in the cohort will be matched
#' @param matchSex can be TRUE or FALSE
#' @param matchYear can be TRUE or FALSE
#' @param ratio at the moment, it is not used this argument
#'
#' @return plot
#' @export
#'
#' @examples

generateMatchedCohort <- function(cdm,
                                  name = "NewCohort",
                                  targetCohortName,
                                  targetCohortId = NULL,
                                  matchSex = TRUE,
                                  matchYear = TRUE,
                                  ratio = 1){
  # Check inputs ---------------------------------------------------------------
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

  # Check if tag
  cdm <- exactMatchingCohort(cdm  = cdm,
                             name = name,
                             targetCohortName = targetCohortName,
                             targetCohortId   = targetCohortId,
                             matchSex  = matchSex,
                             matchYear = matchYear,
                             ratio = ratio)

  # Return
  return(cdm)
}
