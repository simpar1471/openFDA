#' Check that the `warn_on_http_error` argument in `format_search_term()` is
#' appropriate
#' @inheritParams format_search_term
#' @noRd
check_warn_on_http_error_arg <- function(warn_on_http_error,
                                         call = rlang::caller_env()) {
  is_valid <- checkmate::test_logical(warn_on_http_error, len = 1,
                                      any.missing = FALSE)
  if (!is_valid) {
    if (!inherits(warn_on_http_error, "logical")) {
      cli::cli_abort(
        c("{.var warn_on_http_error} must be of class {.cls logical}.",
          "x" = paste0("{.var warn_on_http_error} was class ",
                       "{.cls {class(warn_on_http_error)}}.")),
        call = call,
        class = "openFDA_wohe_invalid_class")
    }
    if (length(warn_on_http_error) != 1) {
      cli::cli_abort(
        c("{.var warn_on_http_error} must be of length 1.",
          "x" = "{.var warn_on_http_error} has {length(warn_on_http_error)} element{?s}."),
        call = call,
        class = "openFDA_wohe_invalid_length"
      )
    }
    if (is.na(warn_on_http_error)) {
      cli::cli_abort(
        message = paste0("{.var warn_on_http_error} must not be missing ",
                         "({.val {warn_on_http_error}})."),
        call = call,
        class = "openFDA_wohe_is_NA"
      )
    }
  }
  warn_on_http_error
}

#' Check that the `search` argument in `format_search_term()` is appropriate
#' @inheritParams format_search_term
#' @noRd
check_search_arg <- function(search, call = rlang::caller_env()) {
  is_valid <- checkmate::test_character(search, min.len = 1,
                                        any.missing = FALSE)
  if (!is_valid) {
    if (length(search) == 0) {
      cli::cli_abort(
        c("{.var search} must be of length >= 1.",
          "x" = "{.var search} was zero-length."),
        call = call,
        class = "openFDA_search_invalid_length"
      )
    }
    if (!inherits(search, "character")) {
      cli::cli_abort(
        c("{.var search} must be of class {.cls character}.",
          "x" = "{.var search} was class {.cls {class(search)}}."),
        call = call,
        class = "openFDA_search_invalid_class"
      )
    }
    if (anyNA(search)) {
      missing_elems <- is.na(search)
      qty_missing_elems <- cli::qty(sum(missing_elems))
      cli::cli_abort(
        c("{.var search} must have no missing ({.val {NA}}) elements.",
          "x" = paste0("{qty_missing_elems}Element{?s} ",
                       "{.val {which(missing_elems)}} {qty_missing_elems}",
                       "{?was/were} missing ({.val {NA}}).")),
        call = call,
        class = "openFDA_search_has_NAs"
      )
    }
  }
  search
}

#' Check that the `sort` argument in `format_sort_term()` is appropriate
#' @inheritParams format_sort_term
#' @noRd
check_sort_arg <- function(sort, call = rlang::caller_env()) {
  is_valid <- checkmate::test_character(sort, len = 1,
                                        any.missing = FALSE)
  if (!is_valid) {
    if (length(sort) == 0) {
      cli::cli_abort(
        c("{.var sort} must be either {.val {NULL}} or length 1.",
          "x" = "{.var sort} had {.val {length(sort)}} element{?s}."),
        call = call,
        class = "openFDA_sort_invalid_length"
      )
    }
    if (!inherits(sort, "character")) {
      cli::cli_abort(
        c("{.var sort} must be of class {.cls character}.",
          "x" = "{.var sort} was class {.cls {class(sort)}}."),
        call = call,
        class = "openFDA_sort_invalid_class"
      )
    }
    if (is.na(sort)) {
      cli::cli_abort(
        message = "{.var sort} must not be missing ({.val {sort}}).",
        call = call,
        class = "openFDA_sort_is_NA"
      )
    }
  }
  if (rlang::is_named(sort)) {
    if (!grepl(x = sort, pattern = "^(asc|desc)$")) {
      cli::cli_abort(
          c("{.var sort} must be either {.val asc} or {.val desc}.",
            "!" = "{.var sort} was {.val {sort}}."),
          call = call,
          class = "openFDA_sort_invalid_ascdesc"
        )
    }
  } else {
    if (!grepl(x = sort, pattern = "(<?\\:)(asc|desc)$", perl = TRUE)) {
      cli::cli_abort(
          c("{.var sort} must end with either {.val asc} or {.val desc}.",
            "!" = "{.var sort} was {.val {sort}}."),
          call = call,
          class = "openFDA_sort_invalid_ascdesc"
        )
    }
  }
  sort
}

#' Assert that users have passed integerish data to the right openFDA parameters
#' @param param The `count` or `api_key` arguments from [openFDA()].
#' @return Invisibly returns `param` if checks are satisfied, or throws an
#'   error.
#' @noRd
check_openFDA_string_arg <- function(param, vname, call = rlang::caller_env()) {
  is_valid <- checkmate::test_string(param, null.ok = TRUE)
  if (!is_valid) {
    if (!(inherits(param, "character"))) {
      cli::cli_abort(
        c("{.var {vname}} must be be of class {.cls character}.",
          "x" = "{.var {vname}} was class {.cls {class(param)}}."),
        call = call,
        class = "openFDA_invalid_string_param_class"
      )
    }
    if (length(param) != 1) {
      cli::cli_abort(
        c("{.var {vname}} must be of length 1.",
          "x" = "{.var {vname}} has {length(param)} element{?s}."),
        call = call,
        class = "openFDA_invalid_string_param_length"
      )
    }
  }
  param
}

#' Assert that users have passed integerish data to the right openFDA parameters
#' @param param One of `count`, `limit`, or `skip` from [openFDA()].
#' @return Invisibly returns `param` if checks are satisfied, or throws an
#'   error.
#' @noRd
check_openFDA_int_arg <- function(param, vname, call = rlang::caller_env()) {
  is_valid <- checkmate::test_integerish(param, len = 1, null.ok = TRUE,
                                         lower = 0)
  if (!is_valid) {
    if (!inherits(param, "numeric")) {
      cli::cli_abort(
        c("{.var {vname}} must be either {.cls numeric} or {.cls integer}.",
          "x" = "{.var {vname}} was class {.cls {class(param)}}."),
        call = call,
        class = "openFDA_invalid_int_param_class"
      )
    }
    if (length(param) != 1) {
      cli::cli_abort(
        c("{.var {vname}} must be of length 1.",
          "x" = "{.var {vname}} has {length(param)} element{?s}."),
        call = call,
        class = "openFDA_invalid_int_param_length"
      )
    }
    if (param < 1 & vname != "limit") {
      cli::cli_abort(
        c("{.var {vname}} must be more than {.val {0L}}.",
          "x" = "{.var {vname}} was {.val {param}}."),
        call = call,
        class = "openFDA_int_param_below_zero"
      )
    }
  }
  if (vname == "limit") {
    if (param < 0 || param > 1000) {
      cli::cli_abort(
        c("{.var limit} must be between {.val {0L}} and {.val {1000L}}.",
          "x" = "{.var limit} was {.val {param}}."),
        call = call,
        class = "openFDA_limit_oob"
      )
    }
  }
  if (vname == "skip") {
    if (!is.null(param) && param > 25000) {
      cli::cli_abort(
        c("{.var skip} must be less than {.val {25000L}}.",
          "x" = "{.var skip} was {.val {param}}."),
        call = call,
        class = "openFDA_skip_too_large"
      )
    }
  }
  param
}

#' Check that the `mode` argument in `format_search_term()` is appropriate
#' @inheritParams format_search_term
#' @noRd
check_mode_arg <- function(mode) {
  opts <- c("and", "or")
  if (!checkmate::test_string(mode)) {
    if (length(mode) != 1) {
        cli::cli_abort(
          c("{.var mode} must be of length 1.",
            "x" = "{.var mode} has {length(mode)} element{?s}."),
          call = rlang::caller_env(),
          class = "openFDA_mode_invalid_length"
        )
      }
      if (!inherits(mode, "character")) {
        cli::cli_abort(
          c("{.var mode} must be of class {.cls character}.",
            "x" = "{.var mode} was class {.cls {class(mode)}}."),
          call = rlang::caller_env(),
          class = "openFDA_mode_invalid_class"
        )
      }
  }
  if (!mode %in% opts) {
    cli::cli_abort(
      c("{.var mode} must be one of either {.val and} or {.val or}.",
        "x" = "{.var mode} was {.val {mode}}."),
      call = rlang::caller_env(),
      class = "openFDA_mode_invalid_value"
    )
  }
}
