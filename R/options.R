# Get openFDA package-level options (internal) ---------------------------------

#' Retrieve and check an openFDA package-level option (internal)
#' @param option_name A single string denoting an openFDA package-level option
#' @param call A function call to use if error message required.
#' @noRd
get_openFDA_option <- function(option_name, call = rlang::caller_env()) {
  value <- rlang::peek_option(option_name)
  valid_opts <- valid_openFDA_options[[option_name]]
  if (!value %in% valid_opts) {
    cli::cli_abort(
      message = c(
        "!" = "The stored value for option {.var {option_name}} is invalid.",
        "i" = "The option {.var {option_name}} must be one of {.val
               {valid_opts}}",
        "x" = "The value for option {.var {option_name}} was {.val {value}}."
      ),
      call = call,
      class = "openfda_invalid_option_value"
    )
  }
  value
}

