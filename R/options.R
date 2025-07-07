# Get openFDA package-level options (internal) ---------------------------------

#' Retrieve and check an openFDA package-level option (internal)
#' @param option_name A single string denoting an openFDA package-level option
#' @param call A function call to use if error message required.
#' @noRd
get_openFDA_option <- function(option_name, call = rlang::caller_env()) {
  if (!option_name %in% names(valid_openFDA_options)) {
    cli_valid_openFDA_option_names <- cli::cli_vec(
      names(valid_openFDA_options), style = list("vec-last" = " or ")
    )
    cli::cli_abort(
      c("!" = "The provided `option_name` ({.val {option_name}}) is invalid.",
        "i" = "`option_name` must be one of {.val
               {cli_valid_openFDA_option_names}}.",
        "x" = "`option_name` was {.val {option_name}}."
      ),
      call = call,
      class = "openfda_invalid_option_name",
      .internal = TRUE
    )
  }
  value <- rlang::peek_option(option_name)
  valid_opts <- valid_openFDA_options[[option_name]]
  if (!value %in% valid_opts) {
    cli::cli_abort(
      c("!" = "The stored value for option {.var {option_name}} is invalid.",
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

