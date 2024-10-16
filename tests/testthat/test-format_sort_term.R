test_that("format_sort_term generates expected strings", {
  expect_equal(ignore_attr = TRUE,
    format_sort_term("tranmissiondate:asc"),
    "tranmissiondate:asc"
  )

  expect_equal(ignore_attr = TRUE,
    format_sort_term(c("receivedate" = "desc")),
    "receivedate:desc"
  )
})

test_that("format_sort_term throws expected errors", {
  # Search param
  ## Zero length
  expect_error(
    format_sort_term(sort = character()),
    class = "openFDA_sort_invalid_length"
  )

  ## Bad class
  expect_error(
    format_sort_term(sort = c(NA_integer_)),
    class = "openFDA_sort_invalid_class"
  )
  expect_error(
    format_sort_term(sort = c(as.complex(1))),
    class = "openFDA_sort_invalid_class"
  )

  ## Has missing values
  expect_error(
    format_sort_term(sort = c(NA_character_)),
    class = "openFDA_sort_is_NA"
  )
  expect_error(
    format_sort_term(sort = c("receivedate" = NA_character_)),
    class = "openFDA_sort_is_NA"
  )

  ## Invalid asc/desc term
  expect_error(
    format_sort_term(sort = "receivedate:ascc"),
    class = "openFDA_sort_invalid_ascdesc"
  )
  expect_error(
    format_sort_term(sort = c("receivedate" = "ascc")),
    class = "openFDA_sort_invalid_ascdesc"
  )
  expect_error(
    format_sort_term(sort = c("receivedate" = "descc")),
    class = "openFDA_sort_invalid_ascdesc"
  )
})
