# openFDA (development version)

* Added `paging` parameter to `openFDA()` function. This parameter controls whether paging (returning multiple sets of results for one query) occurs where necessary. Check out the documentation for `openFDA()` for more information. 
* Added `openFDA.paging` option to openFDA, the default value of which is `"ask"`. A user can change this option to alter the default paging behaviour of the `openFDA()` function.
* Added extra (hopefully clearer) content to package vignettes/articles. 
* Added minimum R version (4.1.0)

# openFDA 0.1.0

* Basic openFDA functions:
  * `openFDA()` to run queries against the openFDA API.
  * `format_search_term()` and `format_sort_term()` to help construct
    `openFDA()` query components.
  * `set_api_key()` and `get_api_key()` for working with openFDA API keys.
* Initial CRAN submission.
