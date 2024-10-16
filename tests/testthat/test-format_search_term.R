test_that("format_search_term generates expected strings", {
  expect_equal(ignore_attr = TRUE,
    format_search_term("openfda.brand_name:ozempic"),
    "openfda.brand_name:ozempic"
  )

  expect_equal(ignore_attr = TRUE,
    format_search_term(c("openfda.brand_name" = "ozempic")),
    "openfda.brand_name:%22ozempic%22"
  )

  expect_equal(ignore_attr = TRUE,
    format_search_term(c("openfda.brand_name" = "ozempic",
                         "openfda.generic_name" = "semaglutide")),
    "openfda.brand_name:%22ozempic%22+openfda.generic_name:%22semaglutide%22"
  )

  expect_equal(ignore_attr = TRUE,
    format_search_term(c("openfda.brand_name" = "ozempic",
                    "openfda.generic_name" = "semaglutide"),
                  mode = "and"),
    "openfda.brand_name:%22ozempic%22+AND+openfda.generic_name:%22semaglutide%22"
  )
})

test_that("format_search_term throws expected errors", {
  # Search param
  ## Zero length
  expect_error(
    format_search_term(search = character()),
    class = "openFDA_search_invalid_length"
  )

  ## Bad class
  expect_error(
    format_search_term(search = c(NA_integer_)),
    class = "openFDA_search_invalid_class"
  )
  expect_error(
    format_search_term(search = c(as.complex(1))),
    class = "openFDA_search_invalid_class"
  )

  ## Has missing values
  expect_error(
    format_search_term(search = c(NA_character_)),
    class = "openFDA_search_has_NAs"
  )
  expect_error(
    format_search_term(search = c(NA_character_, "b", NA_character_, "d")),
    class = "openFDA_search_has_NAs"
  )

  # Mode param
  ## Bad length
  expect_error(
    format_search_term(search = "openfda.brand_name=oze*", mode = character()),
    class = "openFDA_mode_invalid_length"
  )
  expect_error(
    format_search_term(search = "openfda.brand_name=oze*", mode = letters[1:5]),
    class = "openFDA_mode_invalid_length"
  )


  ## Bad class
  expect_error(
    format_search_term(search = "openfda.brand_name=oze*", mode = 1),
    class = "openFDA_mode_invalid_class"
  )

  ## Bad value
  expect_error(
    format_search_term(search = "openfda.brand_name=oze*", mode = "andor"),
    class = "openFDA_mode_invalid_value"
  )
  expect_error(
    format_search_term(search = "openfda.brand_name=oz*", mode = NA_character_),
    class = "openFDA_mode_invalid_value"
  )
})
