#' Format character vectors into `search` terms for openFDA API queries
#' @description
#' This function is a helper for constructing search queries. Whilst it  handles
#' some of the available formatting for openFDA APIs, it does not recapture all
#' of the search term syntax available to you. To get a full appreciation of the
#' openFDA search syntax, see <https://open.fda.gov/apis/advanced-syntax/>.
#' @param search A character vector of length 1 or more. If scalar and unnamed,
#'   it will be assumed that you have already formatted your search string to
#'   work with the API. If named, the vector will be collapsed to include your
#'   various search terms, separated by AND or OR terms based on the value of
#'   `operator`.
#' @param exact A single-length logical vector. When `TRUE` (the default),
#'   individual search terms will be surrounded with double quotes (`""`). Set
#'   `exact` to `FALSE` if your search term contains multiple words to be
#'   searched on, e.g. `c("openfda.generic_name" = "losartan+candesartan")`.
#'
#'   This parameter only applies if `search` is a named character vector.
#' @param mode A single-length character vector, which defines how searches in
#'   multiple fields should be combined. By default (`"or"`) they will be
#'   combined with an 'OR' operator, but you can make an 'AND' operator be used
#'   instead (`"and"`). This argument is case-sensitive and will throw an error
#'   if `mode` is not one of either `"and"` or `"or"`.
#'
#'   This parameter only applies if `search` is a named character vector.
#' @examples
#' # Provide a formatted search string and the function will do no formatting
#' format_search_term("openfda.generic_name:verapamil")
#'
#' # Provide a named vector and the function will format it for you
#' format_search_term(c("openfda.generic_name" = "verapamil"))
#'
#' # If providing multiple elements in your search term, use `exact = FALSE`
#' # to prevent the function from surrounding the term with double quotes.
#' format_search_term(c("openfda.generic_name" = "verapamil+amlodipine"),
#'                    exact = FALSE)
#'
#' # Provide a longer named vector and function will merge these with an OR
#' # operator
#' format_search_term(c("openfda.generic_name" = "verapamil",
#'                      "openfda.manufacturer_name" = "glaxo*"))
#'
#' # Or you can set the `mode` argument to merge your search terms with an AND
#' # operator
#' format_search_term(c("openfda.generic_name" = "verapamil",
#'                      "openfda.manufacturer_name" = "glaxo*"),
#'                    mode = "and")
#' @return A character vector of the S3 class `<AsIS>`, with a formatted search
#'   term which can be supplied to `openFDA()`.
#' @seealso
#' * [format_sort_term()] performs similar formatting for the `sort` component
#'   of an openFDA query.
#' * [I()] generates vectors with the `<AsIs>` S3 class.
#' * [httr2::req_url()] documents why [I()] is applied to the output of this
#'   function.
#' @note
#' This function does not check that you're providing accurate field names or
#' search terms. It is up to you to make sure you've provided correctly spelt
#' fields and search terms.
#' @export
format_search_term <- function(search, exact = TRUE, mode = "or") {
  check_search_arg(search)
  check_mode_arg(mode)
  names_search <- names(search)
  length_search <- length(search)
  if (length_search == 1) {
    if (is.null(names_search)) {
      return(format_url_component(search))
    }
  }
  if (length_search > 1) {
    if (is.null(names_search)) {
      cli::cli_abort(
        c("Non-scalar {.var search} inputs must be named.",
          "x" = "{.var names(search)} was {.var NULL}."),
        call = rlang::caller_env(),
        class = "openFDA_nonscalar_search_vector_is_unnamed"
      )
    }
  }
  if ("" %in% names_search) {
    is_empty <- names_search == ""
    cli::cli_abort(
      c(paste0("Named {.var search} inputs must have no empty ",
               "({.val {\"\"}}) names."),
        "x" = paste0("{cli::qty(sum(is_empty))}Element{?s} ",
                     "{which(is_empty)} in {.var names(search)} ",
                     "{cli::qty(sum(is_empty))} {?was/were} {.val {\"\"}}.")),
      call = rlang::caller_env(),
      class = "openFDA_nonscalar_search_vector_with_empty_names"
    )
  }
  purrr::imap_chr(.x = search,
                  .f = \(item, name) {
                    if (exact) {
                      item <- paste0("\"", item, "\"")
                    }
                    paste0(name, ":", item, "")
                  }) |>
    paste0(collapse = switch(mode, or = "+", and = "+AND+")) |>
    format_url_component()
}

#' Format important URL components before sending to openFDA.
#' @param component A component of the search query in an `openFDA()` call. This
#'   function is used to prevent `httr2`'s inbuild URL parsing from messing up
#'   formatted search terms.
#' @seealso [format_search_term()]
#' @noRd
format_url_component <- function(component) {
  component |>
    #stringr::str_replace_all(pattern =  "\\:", replacement = "%3A") |>
    gsub(pattern = "\"", replacement = "%22") |>
    #stringr::str_replace_all(pattern = "\\+", replacement = "%2B") |>
    gsub(pattern = " ", replacement = "+") |>
    gsub(pattern = "\\*", replacement = "%2A") |>
    I()
}