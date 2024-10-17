#' Get and set your openFDA API keys
#' @param api_key A single-length character vector with your openFDA API key.
#'   You can generate an API key on the
#'   [FDA website](https://open.fda.gov/apis/authentication/).
#' @note To permanently set the API key for a given project, set `OPENFDA_TOKEN`
#'   in `.Renviron`.
#' @examples
#' # Set your openFDA API key with `set_api_key()`
#' api_key <- "example_api_key"
#' set_api_key(api_key)
#'
#' # Retrieve it with `get_api_key()`
#' get_api_key()
#'
#' # An error will be thrown if your API key is an empty string.
#' set_api_key("")
#'
#' try(get_api_key())
#' @return A single length character vector with your API key. For
#'   `set_api_key()`, this is returned invisibly.
#' @rdname api_key
#' @export
set_api_key <- function(api_key) {
  check_openFDA_string_arg(api_key, vname = "api_key")
  Sys.setenv("OPENFDA_TOKEN" = api_key)
  invisible(api_key)
}

#' @return For `get_api_key()`, an error will be thrown if no key has been set.
#' @rdname api_key
#' @export
get_api_key <- function() {
  api_key <- Sys.getenv("OPENFDA_TOKEN")
  if (api_key == "") throw_api_key_error()
  api_key
}

#' Throw an error if a user attempts to use the openFDA package without an API
#' key
#' @param call Used to add information to the error given when
#' @noRd
throw_api_key_error <- function(call = rlang::caller_env()) {
  cli::cli_abort(
    c("To use {.pkg openFDA}, you must set an openFDA API key.",
      "i" = paste0("Go to <https://open.fda.gov/apis/authentication/> ",
                   "to get an openFDA API key, then supply it to ",
                   "{.fun set_api_key} to cache it for use in this session.")),
    class = "openFDA_no_api_key",
    call = call
  )
}
