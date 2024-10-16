test_that("You can set an API key", {
  skip_on_cran()
  skip_if_not(httr2::secret_has_key("OPENFDA_KEY"))
  encrypted_api_key <-
    "TEaDtqdFMq9_Montij5p9IY6T57IyqkbF8IYFVOpk-ttxotFUNdJSxgccAnkq4nQhplaf-r3deQ"

  # The function should return a character vector
  checkmate::expect_string(
    set_api_key(httr2::secret_decrypt(encrypted_api_key, "OPENFDA_KEY"))
  )

  # Now get the API key
  checkmate::expect_string(
    get_api_key(),
    pattern = httr2::secret_decrypt(encrypted_api_key, "OPENFDA_KEY")
  )
})

test_that("API key functions throw expected errors", {
  expect_error(
    set_api_key(239179024359703452790),
    class = "openFDA_invalid_string_param_class"
  )

  set_api_key("")

  expect_error(get_api_key(), class = "openFDA_no_api_key")
  expect_error(openFDA(), class = "openFDA_no_api_key")
})
