encrypted_api_key <-
  "TEaDtqdFMq9_Montij5p9IY6T57IyqkbF8IYFVOpk-ttxotFUNdJSxgccAnkq4nQhplaf-r3deQ"
rlang::push_options(openFDA.paging = "never",
                    openFDA.paging_verbosity = "quiet")

# test_that("openFDA is memoised", {
#   expect_true(memoise::is.memoised(openFDA))
# })

# This test we skip on CRAN, but keep in CI --> tests that the openFDA API is
# actually available with a very simple query
test_that("openFDA can call its API and reach correct endpoints", {
  skip_if_offline()
  suppressMessages(
   set_api_key(httr2::secret_decrypt(encrypted_api_key, "OPENFDA_KEY"))
  )

  # Simple query
  resp <- openFDA(
    search = c("openfda.brand_name" = "ozempic"),
    limit = 2
  )
  expect_s3_class(resp, "httr2_response")
  expect_equal(httr2::resp_body_json(resp)$meta$results$limit, expected = 2)

  endpoints <- c("animalandveterinary-event", "drug-event",
                 "drug-label", "drug-ndc", "drug-enforcement",
                 "drug-drugsfda", "drug-shortages", "device-510k",
                 "device-classification", "device-enforcement",
                 "device-event", "device-pma", "device-recall",
                 "device-registrationlisting", "device-covid19serology",
                 "device-udi", "food-enforcement", "food-event",
                 "other-historicaldocument", "other-nsde",
                 "other-substance", "other-unii", "tobacco-problem")
  purrr::map(endpoints, function(endpoint) {
    resp <- expect_no_warning(suppressMessages(
      openFDA(search = "",
              endpoint = endpoint,
              limit = 1,
              paging = "never",
              paging_verbosity = "quiet")
    ))
    expect_s3_class(resp, class = "httr2_response")
  })
})

test_that(
  desc = "openFDA paging is possible with correct printing behaviour", {
  vcr::use_cassette("test-paging", {
    # Query which requires paging
    resps <- openFDA(search = "openfda.generic_name:\"semaglutide\"",
                     limit = 2,
                     paging = "always",
                     paging_verbosity = "quiet")
    purrr::map(resps, \(resp) expect_s3_class(resp, "httr2_response"))

    # Query which requires paging
    expect_message(
      openFDA(search = "openfda.generic_name:\"semaglutide\"",
              limit = 2,
              paging = "never",
              paging_verbosity = "verbose"),
      class = "openfda_message_paging"
    )
    # Query which requires paging
    expect_no_message(
      openFDA(search = "openfda.generic_name:\"semaglutide\"",
              limit = 2,
              paging = "never",
              paging_verbosity = "quiet"),
      class = "openfda_message_paging"
    )
  })
})

# Does not need vcr as error is thrown *before* any HTTP requests sent
test_that(
  desc = "openFDA paging fails if `paging == \"ask\" but not interactive", {
  skip_if(interactive())
  # Query which requires paging
  expect_error(
    openFDA(search = "openfda.brand_name:o*",
            limit = 300,
            api_key = "placeholder_key",
            paging = "ask"),
    class = "openfda_paging_is_ask_when_uninteractive"
  )
})

test_that("openFDA throws formatted HTTP errors", {
  vcr::use_cassette("test-handle_http_errors", {
    for (handle_http_errors in c("error", "warn", "silent")) {
      expect_fn <- switch(handle_http_errors,
                          "warn" = expect_warning,
                          "error" = expect_error,
                          "silent" = expect_success)
      rlang::push_options(openFDA.handle_http_errors = handle_http_errors)

      expect_condition(
        resp <- openFDA(
          search = "openfda.brand_name=\"verapamil\"",
          sort = c("invalid_field" = "asc"),
          limit = 1
        ),
        class ="openFDA_http_error_400"
      )
      if (handle_http_errors != "error") {
        expect_s3_class(resp, "httr2_response")
        rm(resp)
      } else {
        expect_warning(rm(resp))
      }

      # Error 403 - usually an invalid API key
      expect_condition(
        resp <- openFDA(
          search = "openfda.brand_name:\"verapamil\"\"",
          limit = 1,
          api_key = "really_bad_api_key"
        ),
        class = "openFDA_http_error_403"
      )
      if (handle_http_errors != "error") {
        expect_s3_class(resp, "httr2_response")
        rm(resp)
      } else {
        expect_warning(rm(resp))
      }

      # Error 404 - because search returns nothing
      expect_condition(
        resp <- openFDA(
          search = c("openfda.brand_name" = "3129084"),
          limit = 1
        ),
        class ="openFDA_http_error_404"
      )
      if (handle_http_errors != "error") {
        expect_s3_class(resp, "httr2_response")
        rm(resp)
      } else {
        expect_warning(rm(resp))
      }

      # Error 404 - because `skip` is too high
      expect_condition(
        resp <- openFDA(
          search = c("openfda.brand_name" = "naproxen"),
          skip = 500
        ),
        class ="openFDA_http_error_404"
      )
      if (handle_http_errors != "error") {
        expect_s3_class(resp, "httr2_response")
        rm(resp)
      } else {
        expect_warning(rm(resp))
      }

      # Error 500
      expect_condition(
        resp <- openFDA(
          search = "openfda.brand_name:\"verapamil\"\"",
          limit = 1
        ),
        class ="openFDA_http_error_500"
      )
      if (handle_http_errors != "error") {
        expect_s3_class(resp, "httr2_response")
        rm(resp)
      } else {
        expect_warning(rm(resp))
      }

      # Mock up responses with non-customised error messages. In this case, no
      # special warning will be performed, but the condition class will be
      # specific to the HTTP error
      for (http_status_code in c(408, 503, 504)) {
        temporary_mock <- function(req) {
          httr2::response_json(status_code = http_status_code)
        }
        expect_condition(
          httr2::with_mocked_responses(temporary_mock, {
            openFDA(
              search = "openfda.brand_name:\"verapamil\"",
              limit = 1
            )
          }),
          class = paste0("openFDA_http_error_", http_status_code)
        )
      }
    }
  })
})

# Also doesn't require vcr --> errors occur before `httr2::req_perform()`
test_that("openFDA errors on certain bad inputs", {
  suppressMessages(
    set_api_key(api_key = "api_key_string")
  )

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

  ## paging
  expect_error(
    openFDA(search = c("openfda.brand_name" = "ozempic"),
            paging = 1111111),
    class = "openFDA_invalid_string_param_class"
  )
  expect_error(
    openFDA(search = c("openfda.brand_name" = "ozempic"),
            paging = "bad-value"),
    class = "openFDA_invalid_string_param_value"
  )
  expect_error(
    openFDA(search = c("openfda.brand_name" = "ozempic"),
            paging = c("test", "this")),
    class = "openFDA_invalid_string_param_length"
  )

  ## warn_on_http_error
  expect_error(
    openFDA(search = "", handle_http_errors = 1),
    class = "openFDA_invalid_string_param_class"
  )
  expect_error(
    openFDA(search = "", handle_http_errors = NULL),
    class = "openFDA_invalid_string_param_length"
  )
  expect_error(
    openFDA(search = "", handle_http_errors = c(TRUE, FALSE)),
    class = "openFDA_invalid_string_param_length"
  )
  expect_error(
    openFDA(search = "", handle_http_errors = NA),
    class = "openFDA_invalid_string_param_missing"
  )
})
