# Get/set openFDA package-level options ----------------------------------------


#' Get and set openFDA package-level options
#' @description User-configurable options used by openFDA, which provide a
#' mechanism for setting default behaviours.
#' @param paging A single string used to set the `openFDA.paging` option. The
#'   default, `NULL`, indicates that you don't want to change its value. The
#'   `openFDA.paging` option controls how `openFDA()` behaves when there are
#'   enough results that [paging is
#'   required](https://open.fda.gov/apis/paging/), and has the default value
#'   `"ask"`. New values must be one of:
#'
#'   * `"ask"` - `openFDA()` will warn you that pagination is required and ask
#'     if you want this to be done. Depending on user input, either a single
#'     [httr2](https://httr2.r-lib.org/) response object or a list of
#'     [httr2](https://httr2.r-lib.org/) response objects will be returned. This
#'     will throw an error if your R session is not in interactive mode. When s
#'     etting this option, the value must be one of:
#'   * `"always"` - `openFDA()` will always perform pagination. A list of
#'     [httr2](https://httr2.r-lib.org/) responses will be returned when
#'     pagination occurs.
#'   * `"never"` - `openFDA()` will never perform pagination. Only the first
#'     [httr2](https://httr2.r-lib.org/) response will be returned.
#' @param paging_verbosity A single string used to set the
#'   `openFDA.paging_verbosity` option. The default, `NULL`, indicates that you
#'   don't want to change its value. The `openFDA.paging_verbosity` option
#'   controls whether `openFDA()` prints messages when paging is required, and
#'   has the default value `"warn"`. New values must be one of:
#'
#'   * `"verbose"` - If paging can be performed, print a message to the console,
#'     stating how many requests are required with a minimum estimate for the
#'     time this will take.
#'   * `"quiet"` - Print no messages to the console regarding paging.
#' @param handle_http_errors A single string used to set the
#'   `openFDA.handle_http_errors` option. The default, `NULL`, indicates that
#'   you don't want to change its value. The `openFDA.handle_http_errors` option
#'   controls how `openFDA()` reacts when responses from the openFDA API have
#'   HTTP codes other than `200 OK`, and has the default value `"warn"`. New
#'   values must be one of:
#'
#'   * `"warn"`: If the returned HTTP code is not 200, issue a warning to the
#'   console and return the underlying [httr2](https://httr2.r-lib.org/)
#'   response object.
#'   * `"error"`: If the returned HTTP code is not 200, throw an (informative)
#'   error.
#'   * `"silent"`: If the returned HTTP code is not 200, return the underlying
#'   [httr2](https://httr2.r-lib.org/) response object without printing a
#'   warning.
#' @examples
#' # Print current options by leaving all input args as `NULL`:
#' openFDA_options()
#'
#' # Or specify some options to set these:
#' openFDA_options(paging = "never", handle_http_errors = "silent")
#'
#' # Make sure to give valid option values
#' try(openFDA_options(paging = "bad-value"))
#'
#' # If you set an option using `options()` and it is not valid, you will
#' # invoke errors
#' try(
#'   rlang::with_options(openFDA.paging = "no", current_openFDA_options())
#' )
#' @returns For `openFDA_options()` returns a named character vector with
#'   current option values. This will include the updated option values, if any
#'   have been set.
#' @rdname openFDA_options
#' @export
openFDA_options <- function(paging = NULL,
                            paging_verbosity = NULL,
                            handle_http_errors = NULL) {
  new_opts <- list(paging = paging,
                   paging_verbosity = paging_verbosity,
                   handle_http_errors = handle_http_errors) |>
    vctrs::list_drop_empty()
  opts_empty <- rlang::is_empty(new_opts)
  if (!opts_empty) {
    names(new_opts) <- paste0("openFDA.", names(new_opts))
    check_for_bad_opts(new_opts)
    msg_body <- purrr::imap_chr(
      .x = new_opts,
      .f = function(value, name) {
        cli::format_inline(
          "{.val {name}} option set to {.val {value}} (was previously {.val
          {rlang::peek_option(name)}}) .")
      }) |>
      purrr::set_names("*")
    options(new_opts)
    cli::cli_inform(
      message = c("!" = "New option values set:", msg_body,
                  "i" = "See {.help openFDA_options} for more information."),
      class = "openFDA_options_set"
    )
  } else {
    report_current_options()
  }
  invisible(current_openFDA_options())
}

#' Get the current openFDA options as a named character vector
#' @returns For `current_openFDA_options()`, returns a named character
#'   vector with the current values for each openFDA package-level option.
#' @rdname openFDA_options
#' @export
current_openFDA_options <- function() {
  opt_names <- names(valid_openFDA_options)
  purrr::map_chr(opt_names, rlang::peek_option) |>
    purrr::set_names(opt_names) |>
    check_current_opts()
}

# Helpers for openFDA package-level option setting/getting (internal) ----------

#' Check for bad option values when setting new options (internal)
#' @param new_opts A list with options and their new values
#' @param call Used to format error message (if required)
#' @returns Invisibily returns `NULL`.
#' @noRd
check_for_bad_opts <- function(new_opts, call = rlang::caller_env()) {
  bad_values <- purrr::imap_lgl(new_opts, function(opt_new_val, opt_name) {
    !opt_new_val %in% valid_openFDA_options[[opt_name]]
  })
  if (any(bad_values)) {
    bad_opts_names <- names(new_opts)[bad_values]
    bad_opts_values <- new_opts[bad_values] |> as.character()
    allowed_opts_values <- purrr::map(
      bad_opts_names, function(opt) {
        cli::cli_vec(valid_openFDA_options[[opt]],
                     style = list("vec-last" = " or "))
      })
    msg_body <- purrr::pmap_chr(
      .l = list(bad_opts_names, bad_opts_values, allowed_opts_values),
      .f = function(name, bad_value, allowed_values) {
        cli::format_inline(
          "Bad {.val {name}} value. You tried to set it to
          {.val {bad_value}}, but it must be one of {.val {allowed_values}}."
        )
      }) |>
      purrr::set_names("*")
    cli::cli_abort(
      message = c("!" = "You attempted to set bad option values:", msg_body,
                  "i" = "See {.help openFDA_options} for more information."),
      call = call,
      class = "openFDA_options_setting_bad_value"
    )
  }
  invisible()
}

#' Check that current options are valid (internal)
#' @param current_options Named character vector with current package options.
#' @param call Call used when formatting error message, if required.
#' @returns Returns `current_options`.
#' @noRd
check_current_opts <- function(current_options, call = rlang::caller_env()) {
  bad_values <- purrr::imap_lgl(
    current_options, \(value, name) !value %in% valid_openFDA_options[[name]]
  )
  if (any(bad_values)) {
    bad_options <- current_options[bad_values]
    n_bad_opts <- length(bad_options)
    valid_choices <- valid_openFDA_options[bad_values] |>
      purrr::map(.f = \(x) cli::cli_vec(x, style = list("vec-last" = " or ")))
    msg_body <- purrr::pmap_chr(
      .l = list(bad_options, names(bad_options), valid_choices),
      .f = function(value, name, valid_opts) {
        cli::format_inline(
          "{.code {name}}: is {.val {value}}, but must be one of {.val
          {valid_opts}}."
        )
      }) |>
      purrr::set_names("*")
    cli::cli_abort(
      c("!" = "Invalid {.pkg openFDA} package-level {cli::qty(n_bad_opts)}
               option value{?s} detected.",
        msg_body,
        "i" = "Set {cli::qty(n_bad_opts)} {?a/} valid value{?s} for
              th{?is/ese} option{?s} using {.fn openFDA_options}."),
      class = "openFDA_bad_option_found",
      call = call
    )
  }
  current_options
}

#' Report the current openFDA options with a formatted message (internal)
#' @param curr_opts Current openFDA package-level options.
#'   Default = `current_openFDA_options()`.
#' @returns Invisibly returns a named character vector with the current values
#'   for each openFDA package-level option.
#' @noRd
report_current_options <- function(curr_opts = current_openFDA_options()) {
  curr_opts <- current_openFDA_options()
  msg_body <- purrr::imap_chr(
    curr_opts, function(value, name) {
      cli::format_inline("{.code {name}} = {.val {value}}")
    }) |>
    purrr::set_names("*")
  cli::cli_inform(
    c("Current {.pkg openFDA} options:",
      msg_body,
      "i" = "See {.help openFDA_options} for more information."),
    class = "openFDA_report_current_options"
  )
  invisible(curr_opts)
}
