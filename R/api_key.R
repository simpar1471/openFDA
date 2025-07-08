#' Get and set your openFDA API keys
#' @param api_key A single-length character vector with your openFDA API key. Do
#'   not pass the key in as a parameter in your scripts, though: see the
#'   'Setting an openFDA API key' section below for more information. You can
#'   generate an API key on the
#'   [FDA website](https://open.fda.gov/apis/authentication/).
#' @param user A single-length character vector, used as the `username` argument
#'   in `keyring::key_set()` and `keyring::key_get()`, respectively. The default
#'   is `"openFDA"`. Supply a custom value to this argument if for some reason
#'   you want to store multiple API  keys (though note that the API restricts
#'   the number of requests on a per-IP address basis, as well as on a per-key
#'   basis).
#' @examples
#' \dontshow{
#'   try(prior_api_key <- get_api_key(), silent = TRUE)
#' }
#' # Set your openFDA API key with `set_api_key()`
#' api_key <- "example_api_key"
#' set_api_key(api_key)
#'
#' # Retrieve it with `get_api_key()`
#' get_api_key()
#'
#' # An error will be thrown you try to set your API key to an empty string.
#' try(set_api_key(""))
#' \dontshow{
#'   try(set_api_key(api_key = prior_api_key), silent = TRUE)
#' }
#' @section Setting an openFDA API key:
#' API keys are the sort of thing that should be stored securely. For that
#' reason, `set_api_key()` and `get_api_key()` utilise the
#' [keyring](https://keyring.r-lib.org/) package for storing/retrieving the API
#' key. It is best practice not to set your API key by passing it directly to
#' `set_api_key()`, or you risk exposing it either in your scripts or in
#' `.Rhistory`.
#'
#' Instead, start an interactive R session and run `set_api_key()`. R will
#' prompt you to enter your API key, which will then be stored for future use
#' without logging the key somewhere unsecure.
#'
#' As for the key itself? It will be stored securely using `keyring::key_set()`.
#' Once set on a machine, you shouldn't need to run `set_api_key()` again.
#'
#' ## Creating a new keyring
#' If your current [keyring backend][keyring::backends] supports named keyrings,
#' then when using `set_api_key()` for the first time you will be prompted to
#' create a new keyring called `"openFDA"` using `keyring::keyring_create()`.
#'
#' When asked to do this, you will need to enter a password which can be used to
#' 'lock' and 'unlock' this new keyring (see `keyring::keyring_lock()` for
#' details). However, you shouldn't need to use this password in the future, as
#' the keyring will remain unlocked unless you use `keyring::keyring_lock()` to
#' lock the `"openFDA"` keyring.
#' @returns For `set_api_key()`, returns `NULL` invisibly. The function is
#'   called for its side effect: to store the openFDA API key securely using
#'   `keyring::key_set()`.
#' @rdname api_key
#' @export
set_api_key <- function(api_key, user = "openFDA") {
  user <- check_openFDA_string_arg(user, vname = "user", null_ok = FALSE)
  api_key_not_provided <- rlang::is_missing(api_key)
  if (api_key_not_provided && !interactive()) {
    cli::cli_abort(
      c("!" = "You must be running R in interactive mode to use
               {.fn set_api_key} without providing a value to the `api_key`
               argument.",
        "i" = "If you can run {.fn set_api_key} in interactive mode, please do
               so. Check out the {.fn set_api_key} docs for more information."),
      class = "openfda_set_api_key_needs_interactive_mode"
    )
  }
  if (!api_key_not_provided) {
    check_openFDA_string_arg(api_key, vname = "api_key")
    if (api_key == "") {
      cli::cli_abort(
        c("!" = "If providing `api_key`, it must not be an empty string
                 ({.val \"\"})"),
        class = "openfda_set_api_key_used_empty_string"
      )
    }
  }

  # hardcoded
  service <- "OPENFDA_KEY"

  # Create keyring if it doesn't exist; if it's locked, unlock it
  if (keyring::has_keyring_support() && interactive()) {
    openFDA_keyring <- "openFDA"
    openFDA_keyring_exists <-
      openFDA_keyring %in% keyring::keyring_list()$keyring

    if (openFDA_keyring_exists && keyring::keyring_is_locked(openFDA_keyring)) {
      cli::cli_inform(
        message = c("i" = "The {.val {openFDA_keyring}} keyring is locked.
                           Please unlock with your {.val {openFDA_keyring}}
                           keyring password with {.fn
                           keyring::keyring_unlock}."),
        class = "openfda_keyring_locked"
      )
      keyring::keyring_unlock(keyring = "openFDA")
    } else if (!openFDA_keyring_exists) {
      cli::cli_inform(
        message = c("!" = "No keyring named {.val {openFDA_keyring}} exists.",
                    "i" = "You will be prompted to create one with {.fn
                           keyring::keyring_create}. This requires a
                           password, so please pick one you're likely to
                           remember."),
        class = "openfda_keyring_doesnt_exist"
      )
      keyring::keyring_create(keyring = "openFDA")
    }

    # Add openFDA API key to keyring
    if (api_key_not_provided) {
      keyring::key_set(service = service,
                       user = user,
                       keyring = openFDA_keyring,
                       prompt = "openFDA API key:")
    } else {
      keyring::key_set_with_value(service = service,
                                  user = user,
                                  keyring = openFDA_keyring,
                                  password = api_key)
    }
    cli::cli_inform(
      message = c(
        "!" = "OpenFDA API key set.",
        "i" = "You can retrieve the key with the following code:",
        " " = "{.run keyring::key_get(service = {.val {service}},
                                      user = {.val {user}},
                                      keyring = {.val {openFDA_keyring}})}"),
      class = "openfda_api_key_set"
    )
  } else {
    # For backends without keyring support or running in non-interactive mode
    if (api_key_not_provided) {
      keyring::key_set(service = service,
                       user = user,
                       prompt = "openFDA API key:")
    } else {
      keyring::key_set_with_value(service = service,
                                  user = user,
                                  password = api_key)
    }
    cli::cli_inform(
      message = c(
        "!" = "OpenFDA API key set.",
        "i" = "You can retrieve the key with the following code:",
        " " = "{.run keyring::key_get(service = {.val {service}},
                                      user = {.val {user}})}"),
      class = "openfda_api_key_set"
    )
  }

  invisible(NULL)
}

#' @return For `get_api_key()`, returns the API key as a single string. An error
#'   will be thrown if no key is found or if the returned key is an empty string
#'   (`""`).
#' @rdname api_key
#' @export
get_api_key <- function(user = "openFDA") {
  user <- check_openFDA_string_arg(user, vname = "user", null_ok = FALSE)
  # hardcoded
  service <- "OPENFDA_KEY"
  api_key <- rlang::try_fetch(
    if (keyring::has_keyring_support() && interactive()) {
      keyring::key_get(service = service,
                       username = user,
                       keyring = "openFDA")
    } else {
      keyring::key_get(service = service, username = user)
    },
    error = function(cnd) {
      throw_api_key_error(err = "No openFDA API key was found by
                                 {.fn keyring::key_get}.",
                          class = "openFDA_api_key_missing")
    }
  )
  if (!nzchar(api_key)) {
    throw_api_key_error(
      err = "The openFDA API key retrieved by {.pkg keyring} was empty
             ({.val {\"\"}}).",
      class = "openFDA_api_key_empty"
    )
  }
  api_key
}

#' Throw an error if a user attempts to use the openFDA package without an API
#' key
#' @param err Single string used as main error message.
#' @param class Single string used as error class.
#' @param call Used to add information about the calling environment to the
#'   error when this function is required.
#' @noRd
throw_api_key_error <- function(err,
                                class,
                                call = rlang::caller_env(4)) {
  cli::cli_abort(
    c("!" = err,
      "i" = "To use {.pkg openFDA}, you must set an openFDA API key. Go to
             {.url https://open.fda.gov/apis/authentication/} to get an openFDA
             API key, then use {.run set_api_key()} to cache it for use in this
             and future sessions."),
    class = class,
    call = call
  )
}
