.onLoad <- function(libname, pkgname) {
  rlang::push_options(openFDA.paging = "ask")
}

.onUnload <- function(libname, pkgname) {
  rlang::push_options(openFDA.paging = NULL)
}