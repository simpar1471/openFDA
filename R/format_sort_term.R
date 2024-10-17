#' Format character vectors into `sort` terms for openFDA API queries
#' @description
#' This function acts as a helper for constructing a sort term in the openFDA
#' API.
#' @param sort A single-length character vector of length 1. If unnamed,
#'   it will be assumed that you have already formatted your search string to
#'   work with the API. If named, the vector will be collapsed to include your
#'   field and sorting choice.
#' @examples
#' # Provide a formatted search string and the function will do no formatting
#' format_sort_term("openfda.generic_name:asc")
#'
#' # Provide a named vector and the function will format it for you
#' format_sort_term(c("openfda.generic_name" = "asc"))
#'
#' # Errors will be thrown if you supply a bad input
#' try(format_sort_term("receivedate:no_order"))
#' try(format_sort_term(c("receivedate" = "ascending")))
#' @return A character vector of the S3 class `<AsIS>`, with a formatted search
#'   term which can be supplied to `openFDA()`.
#' @seealso
#' * [format_search_term()] performs similar formatting for the `search`
#'   component of an openFDA query.
#' * [I()] generates vectors with the `<AsIs>` S3 class.
#' * [httr2::req_url()] documents why [I()] is applied to the output of this
#'   function.
#' @note
#' This function does not check that you're providing accurate field names or
#' search terms. It is up to you to make sure you've provided correctly spelt
#' fields and search terms.
#' @export
format_sort_term <- function(sort) {
  if (is.null(sort)) {
    return(NULL)
  }
  check_sort_arg(sort)
  if (rlang::is_named(sort)) {
    sort <- paste0(names(sort), ":", sort)
  }
  format_url_component(sort)
}