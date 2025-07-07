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
    if (!is.character(search)) {
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
          "x" = "{qty_missing_elems}Element{?s} {.val {which(missing_elems)}}
                 {qty_missing_elems} {?was/were} missing ({.val {NA}})."),
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
    if (!is.character(sort)) {
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

#' Assert that users have passed string data to the right openFDA parameters
#' @param param The `count`, `api_key`, or `paging` arguments from [openFDA()].
#' @param vname A single string with the variable's name in `openFDA()`.
#' @param values A character vector defining acceptable values for `param`.
#'   The default, NULL, indicates that `param` can be any string.
#' @return Invisibly returns `param` if checks are satisfied, or throws an
#'   error.
#' @noRd
check_openFDA_string_arg <- function(param,
                                     vname,
                                     values = NULL,
                                     null_ok = TRUE,
                                     call = rlang::caller_env()) {
  is_valid <- checkmate::test_string(param, null.ok = null_ok, na.ok = FALSE)
  if (!is_valid) {
    if (length(param) != 1) {
      cli::cli_abort(
        c("!" = "{.var {vname}} must be of length 1.",
          "x" = "{.var {vname}} has {length(param)} element{?s}."),
        call = call,
        class = "openFDA_invalid_string_param_length"
      )
    }
    if (is.na(param)) {
      cli::cli_abort(
        c("!" = "{.var {vname}} must not be missing ({.val {param}})."),
        call = call,
        class = "openFDA_invalid_string_param_missing"
      )
    }
    if (!is.character(param)) {
      cli::cli_abort(
        c("!" = "{.var {vname}} must be be of class {.cls character}.",
          "x" = "{.var {vname}} was class {.cls {class(param)}}."),
        call = call,
        class = "openFDA_invalid_string_param_class"
      )
    }
  }
  if (!is.null(values) && !param %in% values) {
    cli_vals <-  cli::cli_vec(values, style = list("vec-last" = " or "))
    cli::cli_abort(
      c("!" = "{.var {vname}} is invalid.",
        "i" = "{.var {vname}} must be one of {.val {cli_vals}}.",
        "x" = "{.var {vname}} was {.val {param}}."),
      call = call,
      class = "openFDA_invalid_string_param_value"
    )
  }
  invisible(param)
}

#' Check that arguments in `openFDA()` with a related package-level option are
#' appropriate
#' @inheritParams format_search_term
#' @noRd
check_arg_with_option <- function(param, vname, call = rlang::caller_env()) {
  retrieve <- paste0("openFDA.", vname)
  param <- check_openFDA_string_arg(param = param,
                                    vname = vname,
                                    values = valid_openFDA_options[[retrieve]],
                                    null_ok = FALSE,
                                    call = call)
  if (vname == "paging") {
    if (param == "ask" && !interactive()) {
      cli::cli_abort(
        c("!" = "The `paging` argument must not be `\"ask\"` if R is not running
                 interactively."),
        class = "openfda_paging_is_ask_when_uninteractive",
        call = call
      )
    }
  }
  invisible(param)
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
    if (!is.numeric(param)) {
      cli::cli_abort(
        c("!" = "{.var {vname}} must be either {.cls numeric} or
                 {.cls integer}.",
          "x" = "{.var {vname}} was class {.cls {class(param)}}."),
        call = call,
        class = "openFDA_invalid_int_param_class"
      )
    }
    if (length(param) != 1) {
      cli::cli_abort(
        c("!" = "{.var {vname}} must be of length 1.",
          "x" = "{.var {vname}} has {length(param)} element{?s}."),
        call = call,
        class = "openFDA_invalid_int_param_length"
      )
    }
    if (param < 1 && vname != "limit") {
      cli::cli_abort(
        c("!" = "{.var {vname}} must be more than {.val {0L}}.",
          "x" = "{.var {vname}} was {.val {param}}."),
        call = call,
        class = "openFDA_int_param_below_zero"
      )
    }
  }
  if (vname == "limit") {
    if (param < 0 || param > 1000) {
      cli::cli_abort(
        c("!" = "{.var limit} must be between {.val {0L}} and {.val {1000L}}.",
          "x" = "{.var limit} was {.val {param}}."),
        call = call,
        class = "openFDA_limit_oob"
      )
    }
  }
  if (vname == "skip") {
    if (!is.null(param) && param > 25000) {
      cli::cli_abort(
        c("!" = "{.var skip} must be less than {.val {25000L}}.",
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
          c("!" = "{.var mode} must be of length 1.",
            "x" = "{.var mode} has {length(mode)} element{?s}."),
          call = rlang::caller_env(),
          class = "openFDA_mode_invalid_length"
        )
      }
      if (!is.character(mode)) {
        cli::cli_abort(
          c("!" = "{.var mode} must be of class {.cls character}.",
            "x" = "{.var mode} was class {.cls {class(mode)}}."),
          call = rlang::caller_env(),
          class = "openFDA_mode_invalid_class"
        )
      }
  }
  if (!mode %in% opts) {
    cli::cli_abort(
      c("!" = "{.var mode} must be one of either {.val and} or {.val or}.",
        "x" = "{.var mode} was {.val {mode}}."),
      call = rlang::caller_env(),
      class = "openFDA_mode_invalid_value"
    )
  }
}
