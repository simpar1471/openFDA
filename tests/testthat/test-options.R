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
  expect_error(
    get_openFDA_option(option_name = "bad-option-name"),
    class = "openfda_invalid_option_name"
  )

  for (option in names(valid_openFDA_options)) {
    valid_option <- sample(valid_openFDA_options[[option]], size = 1)

    list(valid_option) |>
      setNames(option) |>
      options()
    expect_equal(get_openFDA_option(option), valid_option)

    list(paste0(valid_option, "-not_valid_anymore")) |>
      setNames(option) |>
      options()
    expect_error(
      get_openFDA_option(option),
      class = "openfda_invalid_option_value"
    )
  }
})

# Set options back to default values before other tests
rlang::push_options(openFDA.paging = "ask",
                    openFDA.paging_verbosity = "verbose",
                    openFDA.handle_http_errors = "warn")