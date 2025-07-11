rlang::push_options(openFDA.paging = "ask",
                    openFDA.paging_verbosity = "verbose",
                    openFDA.handle_http_errors = "warn")

test_that("`current_openFDA_options()` functions as desired", {
  ## Successful if all options are present
  checkmate::expect_character(current_openFDA_options(),
                              len = length(valid_openFDA_options),
                              any.missing = FALSE)

  ## Single bad option
  rlang::with_options(
    openFDA.paging = "TEST-BAD-VALUE",
    expect_error(current_openFDA_options(),
                 class = "openFDA_bad_option_found",
                 regexp = "Set a valid value for this option"),
  )

  ## Multiple bad options
  rlang::with_options(
    openFDA.paging = "no",
    openFDA.paging_verbosity = "LOUD!",
    expect_error(current_openFDA_options(),
                 class = "openFDA_bad_option_found",
                 regexp = "Set valid values for these options")
  )
})

test_that("`openFDA_options()` reports options if none supplied", {
  expect_message(openFDA_options(), class = "openFDA_report_current_options")
})

test_that("`openFDA_options()` can set new options", {
  expect_message(openFDA_options(paging = "always"),
                 class = "openFDA_options_set")
})

test_that("`openFDA_options()` errors with bad option values", {
  expect_error(
    openFDA_options(paging = "always", paging_verbosity = "bad",
                    handle_http_errors = "silent"),
    class = "openFDA_options_setting_bad_value"
  )
})

# Set options back to default values before other tests
rlang::push_options(openFDA.paging = "ask",
                    openFDA.paging_verbosity = "verbose",
                    openFDA.handle_http_errors = "warn")