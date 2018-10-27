#' Creates data frame from validation attribute.
#' @description Creates data frame from validation attribute.
#' @param data Validated data frame.
#' @param attribute Attribute name.
#' @return Data frame with validation results.
get_validations_attribute <- function(data, attribute) {
  attr(data, attribute) %>% purrr::map_df(~ dplyr::tibble(
    validation_id = .$validation_id,
    message = .$message,
    num.violations = .$num.violations)
  )
}

#' Creates data frame from validation results.
#' @description Creates data frame from validation results.
#' @param data Validated data frame.
#' @param errors Data frame with validation errors. See \code{get_validations_attribute} function.
#' @param warnings Data frame with validation warnings. See \code{get_validations_attribute} function.
#' @param file_path Github repo file path to redirect.
#' @param object_name Title to display in the report.
#' @return Data frame with validation results.
create_validation_results <- function(data, errors, warnings, object_name) {
  results <- attr(data, "assertr_results") %>% dplyr::bind_rows() %>%
    dplyr::mutate(object = object_name,
                  result = dplyr::case_when(
                    result == TRUE ~ "Passed",
                    TRUE ~ as.character(result)))
  if (nrow(errors) > 0) {
    results <- results %>% dplyr::mutate(result = dplyr::case_when(
      validation_id %in% errors$validation_id ~ "Failed",
      TRUE ~ as.character(result)))
  }
  if (nrow(warnings) > 0) {
    results <- results %>% dplyr::mutate(result = dplyr::case_when(
      validation_id %in% warnings$validation_id ~ "Warning",
      TRUE ~ as.character(result)))
  }
  errors_and_warnings <- dplyr::bind_rows(errors, warnings)
  if (nrow(errors_and_warnings) > 0) {
    dplyr::left_join(results, errors_and_warnings, by = "validation_id")
  } else {
    results
  }
}

#' Class providing object with methods for simple data validation reports.
#' @docType class
#' @return Object of \code{\link{R6Class}} with methods for simple data validation reports.
#' @export
#' @keywords data
#' @format \code{\link{R6Class}} object.
#' @examples
#' validator <- Validator$new()
#' @section Methods:
#' \describe{
#' \item{\code{add_validations(data, name)}}{This method adds \code{assertr} validation results to the report.}
#' \item{\code{get_validations()}}{This method returns list of current validations.}
Validator <- R6::R6Class(
  classname = "Validator",
  public = list(
    print = function(...) {
      cat("Validation summary: \n")
      cat("\n")
      if (nrow(private$validation_results) > 0) {
        print(private$validation_results %>%
                select(object, title, result, validation_id) %>%
                knitr::kable())
      } else {
        cat("Validator is empty")
      }
    },
    add_validations = function(data, name = deparse(substitute(data))) {
      errors <- get_validations_attribute(data, "assertr_errors")
      warnings <- get_validations_attribute(data, "assertr_warnings")
      object_name <- name
      results <- create_validation_results(data, errors, warnings, object_name)
      private$validation_results <- bind_rows(private$validation_results, results)
      invisible(self)
    },
    get_validations = function(type = "data.frame") {
      if (type == "data.frame") {
        private$validation_results
      } else if (type == "json") {
        rjson::toJSON(private$validation_results)
      } else {
        NULL
      }
    }
  ),
  private = list(
    validation_results = dplyr::tibble()
  ))
