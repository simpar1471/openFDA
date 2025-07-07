.onLoad <- function(libname, pkgname) {
  # openFDA <<- memoise::memoise(openFDA)
  rlang::push_options(openFDA.paging = "ask",
                      openFDA.paging_verbosity = "verbose",
                      openFDA.handle_http_errors = "warn")
}

.onUnload <- function(libname, pkgname) {
  rlang::push_options(openFDA.paging = NULL,
                      openFDA.paging_verbosity = "verbose",
                      openFDA.handle_http_errors = NULL)
}