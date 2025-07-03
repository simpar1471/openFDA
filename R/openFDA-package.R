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
#' * `openFDA.paging`: customize the default behaviour of `openFDA()` when there
#'   are enough results that [paging is
#'   required](https://open.fda.gov/apis/paging/). The new value of this option
#'   should be one of `c("ask", "yes", "no", "yes-quiet", "no-quiet")`.
#'
#' @name openFDA_options
NULL