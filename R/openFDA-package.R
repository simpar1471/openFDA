#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' Options consulted by openFDA
#'
#' @description
#' User-configurable options used by openFDA, which provide a mechanism
#' for setting default behaviour.
#'
#' If the default doesn't suit you, change the option using or specify the value
#' of `paging` in `openFDA()`.
#'
#' @section Options for the openFDA package:
#'
#' * `openFDA.paging`: customise the default behaviour of `openFDA()` when there
#'   are enough results that [paging is
#'   required](https://open.fda.gov/apis/paging/). The new value of this option
#'   should be one of `c("ask", "yes", "no", "yes-quiet", "no-quiet")`.
#' * `openFDA.verbosity_paging`: customise openFDA's verbosity when paging. This
#'   should have the value
#'
#' @examples
#' # Access with base R...
#' options("openFDA.paging")
#'
#' # ... or use the rlang package
#' rlang::peek_options("openFDA.paging")
#'
#' # Set using base R...
#' options(openFDA.paging = "yes")
#'
#' # ... or use the rlang package
#' rlang::push_options(openFDA.paging = "no")
#'
#' # (Just to check it's changed)
#' rlang::peek_options("openFDA.paging")
#' @name openFDA_options
NULL