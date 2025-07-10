.onLoad <- function(libname, pkgname) {
  # Memoise openFDA HTTP activity
  openFDA <<- memoise::memoise(openFDA)

  # Set default options
  rlang::push_options(openFDA.paging = "ask",
                      openFDA.paging_verbosity = "verbose",
                      openFDA.handle_http_errors = "warn")
}

.onUnload <- function(libname, pkgname) {
  rlang::push_options(openFDA.paging = NULL,
                      openFDA.paging_verbosity = NULL,
                      openFDA.handle_http_errors = NULL)
}

# To allow mocking of interactive() in tests, see:
# https://testthat.r-lib.org/reference/local_mocked_bindings.html
interactive <- NULL