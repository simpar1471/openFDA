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
#' @section Options for the openFDA package:
#'
#' * `openFDA.paging`: customise the default behaviour of `openFDA()` when there
#'   are enough results that [paging is
#'   required](https://open.fda.gov/apis/paging/). The default is `"ask"`. When
#'   setting this option, the value must be one of:
#'
#'   - `"ask"` - `openFDA()` will warn you that pagination is required and ask
#'     if you want this to be done. Depending on user input, either a single
#'     {httr2} response object or a list of {httr2} response objects will be
#'     returned. This will throw an error if your R session is not in
#'     interactive mode. When setting this option, the value must be one of:
#'   - `"always"` - `openFDA()` will always perform pagination. A list of
#'     {httr2} responses will be returned when pagination occurs.
#'   - `"never"` - `openFDA()` will never perform pagination. Only the first
#'     {httr2} response will be returned.
#' * `openFDA.paging_verbosity`: customise openFDA's verbosity when paging. The
#'   default is `"verbose"`. When setting this option, the value must be one of:
#'
#'   - `"verbose"` - If paging can be performed, print a message to the console,
#'     stating how many requests are required with a minimum estimate for the
#'     time this will take.
#'   - `"quiet"` - Print no messages to the console regarding paging.
#' * `openFDA.handle_http_errors`: customise whether `openFDA()` prints warnings
#'   to the console when requests to the openFDA server were not `200 OK`. The
#'   default is `"warn"`. When setting this option, the value must be one of:
#'
#'   - `"warn"`: If the returned HTTP code is not 200, issue a warning to the
#'     console and return the underlying {httr2} response object.
#'   - `"error"`: If the returned HTTP code is not 200, throw an (informative)
#'     error.
#'   - `"silent"`: If the returned HTTP code is not 200, return the underlying
#'     {httr2} response object without printing a warning.
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
#' rlang::peek_option("openFDA.paging")
#' @return No return value - they're package-level options, which can be used to
#'   change the behaviour of `openFDA()`.
#' @name openFDA_options
NULL