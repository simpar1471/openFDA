rlang::push_options(openFDA.paging = "ask",
                      openFDA.paging_verbosity = "verbose",
                      openFDA.handle_http_errors = "warn")

test_that("You can get/set options", {
  expect_equal(rlang::peek_option("openFDA.paging"),
               "ask")
  expect_equal(rlang::peek_option("openFDA.handle_http_errors"),
               "warn")

  rlang::push_options(openFDA.paging = "always")
  expect_equal(rlang::peek_option("openFDA.paging"), "always")
})

test_that(desc = "Internal code for checking options throws the right errors", {
  rlang::push_options(openFDA.paging = "no")
  expect_error(
    get_openFDA_option("openFDA.paging"),
    class = "openfda_invalid_option_value"
  )
})

# TODO: Unit tests to check that `openFDA()` behaviour responds correctly to
#       each option change.

# Set options back to default values before other tests
rlang::push_options(openFDA.paging = "ask",
                    openFDA.paging_verbosity = "verbose",
                    openFDA.handle_http_errors = "warn")