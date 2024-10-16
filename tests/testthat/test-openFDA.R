encrypted_api_key <-
  "TEaDtqdFMq9_Montij5p9IY6T57IyqkbF8IYFVOpk-ttxotFUNdJSxgccAnkq4nQhplaf-r3deQ"

test_that("openFDA can call its API", {
  set_api_key(httr2::secret_decrypt(encrypted_api_key, "OPENFDA_KEY"))

  # Simple query
  resp <- openFDA(
    search = c("openfda.brand_name" = "ozempic"),
    limit = 1
  )
  expect_s3_class(resp, "httr2_response")
})


test_that("openFDA throws formatted HTTP errors", {
  set_api_key(httr2::secret_decrypt(encrypted_api_key, "OPENFDA_KEY"))

  # Error 400
  expect_warning(
    resp <- openFDA(
      search = "openfda.brand_name=\"verapamil\"",
      sort = c("invalid_field" = "asc"),
      limit = 1
    ),
    class ="openFDA_http_error_400"
  )
  expect_s3_class(resp, "httr2_response")

  # Error 403
  expect_warning(
    resp <- openFDA(
      search = "openfda.brand_name:\"verapamil\"\"",
      limit = 1,
      api_key = "really_bad_api_key"
    ),
    class ="openFDA_http_error_403"
  )
  expect_s3_class(resp, "httr2_response")

  # Error 404 - because search returns nothing
  expect_warning(
    resp <- openFDA(
      search = c("openfda.brand_name" = "3129084"),
      limit = 1
    ),
    class ="openFDA_http_error_404"
  )
  expect_s3_class(resp, "httr2_response")

  # Error 404 - because `skip` is so high
  expect_warning(
    resp <- openFDA(
      search = c("openfda.brand_name" = "naproxen"),
      skip = 500
    ),
    class ="openFDA_http_error_404"
  )
  expect_s3_class(resp, "httr2_response")

  # Error 500
  expect_warning(
    resp <- openFDA(
      search = "openfda.brand_name:\"verapamil\"\"",
      limit = 1
    ),
    class ="openFDA_http_error_500"
  )
  expect_s3_class(resp, "httr2_response")
})

test_that("openFDA errors on certain bad inputs", {
  set_api_key("api_key_string")

  expect_error(
    openFDA(
      search = c("openfda.brand_name" = "3129084"),
      limit = 1,
      endpoint = "INCORRECT_STRING"
    ),
    class = "openFDA_invalid_endpoint"
  )

  # Issues with the search vector - also tested in test-format_search_term.R
  expect_error(
    openFDA(search = c("test1", "test2")),
    class = "openFDA_nonscalar_search_vector_is_unnamed"
  )

  expect_error(
    openFDA(search = c("test1", "openfda.manufacturer_name" = "test2")),
    class = "openFDA_nonscalar_search_vector_with_empty_names"
  )

  # Issues with bad...
  ## api_key
  expect_error(
    openFDA(search = "", api_key = 1111111),
    class = "openFDA_invalid_string_param_class"
  )
  expect_error(
    openFDA(search = "", api_key = c("egrio", "oirge")),
    class = "openFDA_invalid_string_param_length"
  )

  ## count
  expect_error(
    openFDA(search = "", count = FALSE),
    class = "openFDA_invalid_string_param_class"
  )
  expect_error(
    openFDA(search = "", count = c("field1", "field2")),
    class = "openFDA_invalid_string_param_length"
  )

  ## limit
  expect_error(
    openFDA(search = "", limit = c("field1", "field2")),
    class = "openFDA_invalid_int_param_class"
  )
  expect_error(
    openFDA(search = "", limit = 80000),
    class = "openFDA_limit_oob"
  )
  expect_error(
    openFDA(search = "", limit = 80000),
    class = "openFDA_limit_oob"
  )

  ## skip
  expect_error(
    openFDA(search = "", skip = "100"),
    class = "openFDA_invalid_int_param_class"
  )
  expect_error(
    openFDA(search = "", skip = c(5, 5)),
    class = "openFDA_invalid_int_param_length"
  )
  expect_error(
    openFDA(search = "", skip = -200),
    class = "openFDA_int_param_below_zero"
  )
  expect_error(
    openFDA(search = "", skip = 26000),
    class = "openFDA_skip_too_large"
  )

  ## warn_on_http_error
  expect_error(
    openFDA(search = "", warn_on_http_error = 1),
    class = "openFDA_wohe_invalid_class"
  )
  expect_error(
    openFDA(search = "", warn_on_http_error = c(TRUE, FALSE)),
    class = "openFDA_wohe_invalid_length"
  )
  expect_error(
    openFDA(search = "", warn_on_http_error = NA),
    class = "openFDA_wohe_is_NA"
  )
})

