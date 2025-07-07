## Set internal data for options
valid_openFDA_options <- list(
  openFDA.paging = c("always", "ask", "never"),
  openFDA.paging_verbosity = c("verbose", "quiet"),
  openFDA.handle_http_errors = c("warn", "error", "silent")
)
usethis::use_data(valid_openFDA_options, internal = TRUE, overwrite = TRUE)
