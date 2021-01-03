#' @export
validate <- function(data, name, description = NULL) {
  if (missing(name)) {
    name <- deparse(substitute(data))
    if (name == ".") {
      name <- get_first_name(data)
    }
  }
  attr(data, "data-description") <- description
  attr(data, "data-name") <- name
  attr(data, "assertr_in_chain_success_fun_override") <- success_append
  attr(data, "assertr_in_chain_error_fun_override") <- error_append
  data
}

#' @export
assert_if <- function(data, expr, description = NA, obligatory = FALSE) {
  verify(data = data,
                  expr = !!rlang::enexpr(expr),
                  description = format_description(description),
                  skip_chain_opts = FALSE,
                  obligatory = obligatory,
                  success_fun = success_append,
                  error_fun = error_append,
                  defect_fun = defect_append)
}

#' @export
assert_cols <- function(data, predicate, ..., predicate_calculated_from_column = FALSE, obligatory = FALSE, description = NA) {
  if (predicate_calculated_from_column) {
    data <- insist(
      data = data, predicate_generator = predicate, ...,
      skip_chain_opts = FALSE,
      obligatory = obligatory,
      description = format_description(description),
      success_fun = success_append,
      error_fun = error_append,
      defect_fun = defect_append
    )
  } else {
    data <- assert(
      data = data, predicate = predicate, ...,
      skip_chain_opts = FALSE,
      obligatory = obligatory,
      description = format_description(description),
      success_fun = success_append,
      error_fun = error_append,
      defect_fun = defect_append
    )
  }
}

#' @export
assert_rows <- function(data, row_reduction_fn, predicate, ..., predicate_calculated_from_reduced_row = FALSE, obligatory = FALSE, description = NA) {
  if (predicate_calculated_from_reduced_row) {
    data <- insist_rows(
      data = data,
      row_reduction_fn = row_reduction_fn,
      predicate_generator = predicate,
      ...,
      skip_chain_opts = FALSE,
      obligatory = obligatory,
      description = format_description(description),
      success_fun = success_append,
      error_fun = error_append,
      defect_fun = defect_append
    )
  } else {
    data <- assert_rows(
      data = data,
      row_reduction_fn = row_reduction_fn,
      predicate = predicate,
      ...,
      skip_chain_opts = FALSE,
      obligatory = obligatory,
      description = format_description(description),
      success_fun = success_append,
      error_fun = error_append,
      defect_fun = defect_append
    )
  }
}
