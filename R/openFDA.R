#' Send requests to the openFDA API
#' @param search A character vector which will be passed to
#'   `format_search_term()`. If search is `""` (the default), openFDA will
#'   retrieve all records with no filtering. An error will be thrown if any
#'   elements of `search` are missing (`NA`).
#' @param sort A single string or scalar named character vector describing how
#'   to sort the results. The `sort` term should either be formatted as
#'   `"[FIELD]:[asc/desc]"`, or `c("[FIELD]" = "[asc/desc]")`. For example, to
#'   sort results in the Drugs@FDA endpoint by ascending submission status
#'   dates. If values other than `"asc"`, or `"desc"` are supplied, the function
#'   will throw an error.
#' @param count A single string denoting a field on which to count results. If
#'   `NULL` (the default), results will be returned in full to the user. Specify
#'   this parameter if you want to count occurrences of results within your
#'   search term. The [openFDA website](https://open.fda.gov/apis/query-syntax/)
#'   has more information on how to `count` by a specific field.
#' @param limit A single integerish value describing the limit on the number of
#'   records to retrieve. An error will be thrown if `limit` is more than `1000`
#'   (the default).
#' @param skip A single integer describing how many records should be skipped.
#'   If more records are skipped than are found in your search, the openFDA
#'   API will return a 404 error.
#' @param api_key A single-length character vector with your openFDA API key. By
#'   default this is the result of `get_api_key()`. You must set this with
#'   `set_api_key()`, or `openFDA()` will not work.
#' @param endpoint A single-length character vector describing which openFDA
#'   endpoint to target.
#'
#'   * `"animalandveterinary-event"`: Adverse event reports for
#'     animal/veterinary drugs.
#'   * `"drug-event"`: Adverse event reports from [FDA Adverse Event Reporting
#'     System](https://open.fda.gov/data/faers/)
#'   * `"drug-label"`: Drug documentation in the [Structured Product
#'     Labeling](https://www.fda.gov/industry/fda-data-standards-advisory-board/structured-product-labeling-resources)
#'     (SPL) format.
#'   * `"drug-ndc"`: Data from the [National Drug Code
#'     Directory](https://open.fda.gov/data/ndc/) (NDC).
#'   * `"drug-enforcement"`: Data from the [FDA Recall Enterprise
#'     System](https://open.fda.gov/data/res/) about drug products.
#'   * `"drug-drugsfda"`: Data on products approved for human use by the FDA
#'     since 1939, with mostly complete data after 1998.
#'   * `"drug-shortages"`: Data on
#'     [drug shortages](https://open.fda.gov/data/drugshortages/) as compiled by
#'     the FDA.
#'   * `"device-510k"`: Data from 510(k) submissions to the FDA regarding
#'     medical devices.
#'   * `"device-classification"`: Data from the FDA [Product Classification
#'     Database](https://open.fda.gov/data/product-classification/) for medical
#'     devices.
#'   * `"device-enforcement"`: Data from the [FDA Recall Enterprise
#'     System](https://open.fda.gov/data/res/) (RES) for medical devices.
#'   * `"device-event"`: Data on adverse events associated with FDA-approved
#'     medical devices.
#'   * `"device-pma"`: Pre-market approval data for medical devices submitted to
#'     the FDA.
#'   * `"device-recall"`: Data on product recalls for medical devices which
#'     violate FDA law.
#'   * `"device-registrationlisting"`: Data on FDA [Device Registrations and
#'     Listings](https://www.fda.gov/medical-devices/how-study-and-market-your-device/device-registration-and-listing).
#'   * `"device-covid19serology"`: Data from [independent evaluations of
#'     COVID-19 serological
#'     tests](https://open.fda.gov/apis/device/covid19serology/).
#'   * `"device-udi"`: Data from the FDA's Global Unique Device Identification
#'     Database (GUDID).
#'   * `"food-enforcement"`: Data from the [FDA Recall Enterprise
#'     System](https://open.fda.gov/data/res/) for food products.
#'   * `"food-event"`: Data from the [Center for Food Safety and Applied
#'     Nutrition Adverse Event Reporting
#'     System](https://open.fda.gov/data/caers/).
#'   * `"other-historicaldocument"`: The majority of FDA Press Releases, from
#'     1913 to 2014.
#'   * `"other-nsde"`: Data from the [National Drug Code Structured Product
#'     Labeling Data
#'     Elements](https://www.fda.gov/industry/structured-product-labeling-resources/nsde).
#'   * `"other-substance"`: Data regarding substances - individual molecules,
#'     proteins, nucleic acids, and more.
#'   * `"other-unii"`: Data from the [FDA's Global Substance Registration
#'     System](https://www.fda.gov/industry/fda-data-standards-advisory-board/fdas-global-substance-registration-system).
#'   * `"tobacco-problem"`: Data on problems (e.g. damage, defects,
#'     contamination, bad smell) with tobacco products.
#'
#'   This argument is case-sensitive. By default, the package will target the
#'   Drugs@FDA endpoint (`"drugs-drugsfda"`).
#' @param paging A single string describing whether results should be paged. You
#'   can set this on a per-function call level with this parameter, or edit
#'   the [package-level option][openFDA_options] `openFDA.paging` to set paging
#'   behaviour across multiple `openFDA()` calls.
#'
#'   By default, `openFDA()` uses the string stored in
#'   `options("openFDA.paging")`, which has the default value `"ask"`.
#'   Permissible values for `paging` include:
#'
#'   * `"ask"` - `openFDA()` will warn you that pagination is required and ask
#'     if you want this to be done. Depending on user input, either a single
#'     [httr2](https://httr2.r-lib.org/) response object or a list of
#'     [httr2](https://httr2.r-lib.org/) response objects will be returned. This
#'     will throw an error if your R session is not in interactive mode.
#'   * `"always"` - `openFDA()` will always perform pagination. A list of
#'     [httr2](https://httr2.r-lib.org/) responses will be returned when
#'     pagination occurs.
#'   * `"never"` - `openFDA()` will never perform pagination. Only the first
#'     [httr2](https://httr2.r-lib.org/) response will be returned.
#' @param paging_verbosity A single string describing whether messages about
#'   pagination should be printed to the console. You can set this on a
#'   per-function call level with this parameter, or edit the [package-level
#'   option][openFDA_options] `openFDA.paging_verbosity` to set this behaviour
#'   across multiple `openFDA()` calls.
#'
#'   By default, `openFDA()` uses the string stored in
#'   `options("openFDA.paging_verbosity")`, which has the default value
#'   `"verbose"`. Permissible values for `paging_verbosity` include:
#'
#'   * `"verbose"` - If paging can be performed, print a message to the console,
#'     stating how many requests are required with a minimum estimate for the
#'     time this will take.
#'   * `"quiet"` - Even if paging can be performed, do not print messages about
#'     it  to the console.
#' @param handle_http_errors A single string defining how to handle HTTP codes
#'   other than `200 OK`. You can set this on a per-function call level with
#'   this parameter, or edit the [package-level option][openFDA_options]
#'   `openFDA.handle_http_errors` to set this behaviour across multiple
#'   `openFDA()` calls.
#'
#'   By default, `openFDA()` uses the string stored in
#'   `options("openFDA.handle_http_errors")`, which has the default value
#'   `"warn"`. Permissible values for `handle_http_errors` include:
#'
#'   * `"warn"`: If the returned HTTP code is not 200, issue a warning to the
#'     console and return the underlying [httr2](https://httr2.r-lib.org/)
#'     response object.
#'   * `"error"`: If the returned HTTP code is not 200, throw an (informative)
#'     error.
#'   * `"silent"`: If the returned HTTP code is not 200, return the underlying
#'     [httr2](https://httr2.r-lib.org/) response object without printing a
#'     warning.
#' @examples
#' if (httr2::secret_has_key("OPENFDA_KEY")) {
#'   set_api_key(httr2::secret_decrypt(
#'     "TEaDtqdFMq9_Montij5p9IY6T57IyqkbF8IYFVOpk-ttxotFUNdJSxgccAnkq4nQhplaf-r3deQ",
#'     "OPENFDA_KEY"
#'   ))
#'
#'   resp <- openFDA(search = "openfda.manufacturer_name:gilead*",
#'                   limit = 2,
#'                   skip = 10,
#'                   paging = "never",
#'                   paging_verbosity = "quiet")
#'
#'   # The function returns an `httr2` object
#'   print(resp)
#' }
#'
#' # Bad inputs will cause informative errors - here, a bad API key is supplied
#' try(
#'   openFDA(search = "openfda.manufacturer_name:gilead*",
#'           api_key = "BAD_API_KEY",
#'           limit = 1)
#' )
#' @references
#' Kass-Hout TA, Xu Z, Mohebbi M, Nelsen H, Baker A, LEvine J, Johansen E,
#' Bright RA. **OpenFDA: an innovative platform providing access to a wealth of
#' FDA's publicly available data** *J Am Med Inform Assoc* 2016,
#' **23(3):596-600.** \doi{10.1093/jamia/ocv153}
#' @seealso Read the docs for [set_api_key()] to learn how to set your openFDA
#'   API key. Check out [format_search_term()] to see how input `search` vectors
#'   are converted to openFDA API searches.
#' @return Either an `httr2` response object from [httr2::req_perform()] or list
#'   of these objects, depending on whether pagination was required. You can use
#'   [httr2::resp_body_json()] to extract JSON data from these responses.
#' @rdname openFDA
#' @export
openFDA <- function(
  search = "",
  sort = NULL,
  count = NULL,
  limit = 1000,
  skip = NULL,
  endpoint = "drug-drugsfda",
  api_key = get_api_key(),
  paging = get_openFDA_option("openFDA.paging"),
  paging_verbosity = get_openFDA_option("openFDA.paging_verbosity"),
  handle_http_errors = get_openFDA_option("openFDA.handle_http_errors")
) {
  # Check params + drop NULL args from request query
  api_key <- check_openFDA_string_arg(api_key, "api_key")
  check_arg_with_option(paging, "paging")
  check_arg_with_option(paging_verbosity, "paging_verbosity")
  check_arg_with_option(handle_http_errors, "handle_http_errors")
  req_params <- list(search = format_search_term(search),
                     sort = format_sort_term(sort),
                     count = check_openFDA_string_arg(count, "count"),
                     limit = check_openFDA_int_arg(limit, "limit"),
                     skip = check_openFDA_int_arg(skip, "skip")) |>
    vctrs::list_drop_empty()

  # Build request then perform
  url <- endpoint_url(endpoint)
  req_openFDA <- httr2::request(base_url = url) |>
    httr2::req_url_query(!!!req_params) |>
    httr2::req_user_agent(
      string = "openFDA for R (https://www.github.com/simpar1471/openFDA)"
    ) |>
    httr2::req_auth_basic(username = api_key, password = "") |>
    httr2::req_throttle(rate = 240 / 60) |>
    httr2::req_error(
      is_error = \(resp) openFDA_error_handling(resp, handle_http_errors)
    )
  resp_openFDA <- httr2::req_perform(req_openFDA)

  # Paging logic
  if (resp_openFDA$status_code == 200) {
    if (paging_as_lgl(resp_openFDA, paging, paging_verbosity)) {
      resp_openFDA <- req_openFDA |>
        httr2::req_perform_iterative(
          next_req = httr2::iterate_with_link_url(rel = "next"),
          max_reqs = Inf,
          progress = TRUE
        )
    }
  }

  resp_openFDA
}

# openFDA() paging logic (internal) --------------------------------------------

#' User interaction with respect to paging
#' @param total,limit Single-length integerish variables denoting the total
#'   results found in openFDA for a given query, and the limit on the number of
#'   responses as imposed by the package user.
#' @param resp_openFDA An httr2 response object from `openFDA()`
#' @inheritParams openFDA
#' @returns For paging_as_lgl(), returns a single boolean value denoting whether
#'   or not paging will be performed. For `paging_inform()`, returns `NULL`
#'   invisibly and may emit a message about paging. For `paging_ask()`, returns
#'   user input about whether or not they want paging to be performed.
#' @rdname paging
#' @noRd
paging_as_lgl <- function(resp_openFDA, paging, paging_verbosity) {
  resp_has_link_header <- httr2::resp_header_exists(resp_openFDA,
                                                    header = "Link")
  json <- httr2::resp_body_json(resp_openFDA)
  if (!resp_has_link_header | !("results" %in% names(json$meta))) {
    return(FALSE)
  }
  n_resps_mthan_limit <-  json$meta$results$total > json$meta$results$limit
  if (!n_resps_mthan_limit) {
    return(FALSE)
  }
  results <- httr2::resp_body_json(resp_openFDA)$meta$results
  if (paging_verbosity == "verbose") paging_inform(results$total, results$limit)
  if (paging == "ask") {
    if (paging_ask(results$total, results$limit) == 1) {
      return(TRUE)
    }
  }
  paging == "always"
}

#' User interaction with respect to paging
#' @param total,limit Single-length integerish variables denoting the total
#'   results found in openFDA for a given query, and the limit on the number of
#'   responses as imposed by the package user.
#' @returns For `paging_inform()`, returns NULL invisibly. For `paging_ask()`,
#'   returns user input about whether or not they want paging to be performed.
#' @rdname paging
#' @noRd
paging_inform <- function(total, limit) {
  n_queries <- ceiling(total / limit)
  cli_n_queries <- prettyNum(n_queries, big.mark = ",")
  min_time <- prettyunits::pretty_sec(ceiling(n_queries / 240))
  cli::cli_inform(c(
    "!" = cli::format_inline("openFDA returned more results ({total}) than your
                             `limit` ({limit})."),
    "i" = "With {limit} results per page, pagination will require
           {cli_n_queries} queries to retrieve all of these results. This will
           take at least {min_time}."),
    class = "openfda_message_paging"
  )
  cat("\n")
  invisible(NULL)
}

#' Ask an end-user if they want `openFDA()` to page through all results
#' @inheritParams paging_inform
#' @rdname paging
#' @noRd
paging_ask <- function(total, limit) {
  input <- utils::menu(
    title = "Do you want openFDA to retrieve all results?",
    choices = c(paste0("Yes (will perform ", ceiling(total / limit),
                       " requests)"),
                "No (will return first response only)"))
  input
}

# Generate an openFDA endpoint URL (internal) ----------------------------------

#' Format an openFDA endpoint URL
#' @inheritParams openFDA
#' @noRd
endpoint_url <- function(endpoint) {
  valid_endpoints <- c("animalandveterinary-event", "drug-event", "drug-label",
                       "drug-ndc", "drug-enforcement", "drug-drugsfda",
                       "drug-shortages", "device-510k", "device-classification",
                       "device-enforcement", "device-event", "device-pma",
                       "device-recall", "device-registrationlisting",
                       "device-covid19serology", "device-udi",
                       "food-enforcement", "food-event",
                       "other-historicaldocument", "other-nsde",
                       "other-substance", "other-unii", "tobacco-problem")
  if (!endpoint %in% valid_endpoints) {
    cli::cli_abort(
      c("You must select an appropriate {.var endpoint} value.",
        "i" = "Valid endpoints can be seen in the {.fun openFDA}
               documentation.",
        "x" = "You supplied {.val {endpoint}}."),
      call = rlang::caller_env(),
      class = "openFDA_invalid_endpoint"
    )
  }
  paste0("https://api.fda.gov/",
         gsub(x = endpoint, pattern = '-', replacement = '/'),
         ".json")
}

# OpenFDA HTTP error handlers (internal) ---------------------------------------

#' Catch common HTTP errors in `openFDA()` calls
#' @param req_openFDA A httr2 request object, which will be used to query the
#'   openFDA API.
#' @param handle_http_errors Scalar logical value. If `TRUE`, the function will
#'   issue a warning based on the HTTP error encountered
#' @noRd
openFDA_error_handling <- function(resp, handle_http_errors) {
  status <- httr2::resp_status(resp)
  if (status != 200 && handle_http_errors != "silent") {
    call <- rlang::caller_env(n = 6)
    msg <- switch(as.character(status),
                  `400` = openFDA_err_400_msg(resp),
                  `403` = openFDA_err_403_msg(resp),
                  `404` = openFDA_err_404_msg(resp),
                  `500` = openFDA_err_500_msg(resp),
                  openFDA_err_generic_msg(resp))
    cli_fn <- switch(handle_http_errors,
                     "warn" = cli::cli_warn,
                     "error" = cli::cli_abort)
    class <- paste0("openFDA_http_error_", status)
    cli_fn(message = msg, call = call, class = class)
  }
  FALSE
}

#' HTTP error-handling functions for openFDA
#' @param resp An httr2 response object made when calling `openFDA()`.
#' @rdname openFDA_http_errors
#' @noRd
openFDA_err_generic_msg <- function(resp) {
   status <- httr2::resp_status(resp)
   desc <- httr2::resp_status_desc(resp)
   c(paste("The openFDA API returned a", status, "error."),
     "i" = "The status description was:",
     " " = cli::format_inline("{.val {desc}}"))
}

#' @rdname openFDA_http_errors
#' @noRd
openFDA_err_400_msg <- function(resp) {
   query <- retrieve_openFDA_query(resp)
   message <- httr2::resp_body_json(resp)$error$message
   c("The openFDA API returned a 400 error.",
     "!" = "This was due to a malformed query.",
     "i" = "Your query string was:",
     " " = cli::format_inline("{.val {query}}"),
     "i" = "The error message from openFDA was:",
     " " = cli::format_inline("{.val {message}}"))
}

#' @rdname openFDA_http_errors
#' @noRd
openFDA_err_403_msg <- function(resp) {
  api_key <- httr2::url_parse(resp$url)$query$api_key
  c("The openFDA API returned a 403 error.",
    "!" = "This usually means that an invalid {.var api_key} was used.",
    "i" = cli::format_inline("Your API key was {.val {api_key}}."))
}

#' @rdname openFDA_http_errors
#' @noRd
openFDA_err_404_msg <- function(resp) {
  parsed_url <- httr2::url_parse(resp$url)
  search <- parsed_url$query$search
  skip <- parsed_url$query$skip
  if (!is.null(search)) {
    search_str <- cli::format_inline(
      paste0("A restrictive search term. Yours was {.val {search}}.")
    )
  } else {
    search_str <- NULL
  }
  if (!is.null(skip)) {
    skip_str <- cli::format_inline(
      "Skipping more results than were found. Your {.var skip} value was {.val
      {as.integer(skip)}}."
    )
  } else {
    skip_str <- NULL
  }
  c("The openFDA API returned a 404 error.",
    "!" = "This indicates that openFDA had no results to return.",
    "This can be due to:",
    "*" = search_str,
    "*" = skip_str)
}

#' @rdname openFDA_http_errors
#' @noRd
openFDA_err_500_msg <- function(resp) {
  query <- retrieve_openFDA_query(resp)
  details <- httr2::resp_body_json(resp)$error$details
  c("The openFDA API returned a 500 error.",
    "!" = "This can indicate a malformed query string.",
    "i" = "Your query string was:",
    " " = cli::format_inline("{.val {query}}"),
    "i" = "The error message from openFDA was:",
    " " = cli::format_inline("{.val {details}}"))
}

#' Reconstitute an openFDA query from an httr2 response object.
#' @param resp An httr2 response object made when calling `openFDA()`.
#' @noRd
retrieve_openFDA_query <- function(resp) {
  purrr::imap_chr(.x = httr2::url_parse(resp$url)$query,
                  .f = \(value, name) paste0(name, "=", value)) |>
    paste0(collapse = "&")
}

