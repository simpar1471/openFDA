# openFDA (development version)

* **BREAKING CHANGES:**
  * Changed name of `warn_on_http_error` argument in `openFDA()` to `handle_http_errors`, to reflect the new package-level option.
* Added `paging` parameter to `openFDA()` function. This parameter controls whether paging (returning multiple sets of results for one query) occurs where necessary. Check out the documentation for `openFDA()` for more information. 
* Added package-level options to openFDA
  * `openFDA.paging`; the default value of which is `"ask"`. A user can change this option to alter the default paging behaviour of the `openFDA()` function (to `"always"` page results or `"never"` page results).
  * `openFDA.paging_verbosity`; the default value of which is `"verbose"`. A user can change this option to control whether messages about potential paging are printed to the console.
  * `openFDA.handle_http_errors`; the default value of which is `"warn"`. A user can change this option to alter how non-`200 OK` HTTP codes are handled (they can be silenced or trigger errors).
* Added `openFDA_options()` and `current_openFDA_options()`, two functions which can be used to get/set and get openFDA package-level option values, respectively.
* API keys are now stored/retrieved using the `keyring` package. This is a more secure alternative to simply using environment variables, as users are prompted to supply the API key interactively (instead of storing them in a script). 
* The `openFDA()` function now supports caching with `memoise::memoise()`.
* Added extra (hopefully clearer) content to package vignettes/articles. 
* Added minimum R version (4.1.0)

# openFDA 0.1.0

* Basic openFDA functions:
  * `openFDA()` to run queries against the openFDA API.
  * `format_search_term()` and `format_sort_term()` to help construct
    `openFDA()` query components.
  * `set_api_key()` and `get_api_key()` for working with openFDA API keys.
* Initial CRAN submission.
