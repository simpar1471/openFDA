test_that("You can set and get an API key", {
  # Test should skip on CRAN automatically from use of `httr2::secret_decrypt()`
  # BUT just to be sure
  skip_on_cran()

  encrypted_api_key <- paste0("TEaDtqdFMq9_Montij5p9IY6T57IyqkbF8IYFVOpk",
                              "-ttxotFUNdJSxgccAnkq4nQhplaf-r3deQ")
  decrypted_key <- httr2::secret_decrypt(encrypted_api_key, "OPENFDA_KEY")
  # `set_api_key() should return NULL but issue a message
  expect_message(
    expect_null(set_api_key(decrypted_key)),
    class = "openfda_api_key_set"
  )

  # Now check whether the stored API key is the same as this one
  expect_equal(get_api_key(), decrypted_key)
})

test_that("API key functions throw expected errors", {
  ## Errors if api_key input is bad
  expect_error(
    set_api_key(c("string_one", "string_two")),
    class = "openFDA_invalid_string_param_length"
  )
  expect_error(
    set_api_key(239179024359703452790),
    class = "openFDA_invalid_string_param_class"
  )

  ## Errors if api_key input is empty
  expect_message(set_api_key(""), class = "openfda_api_key_set")
  expect_error(get_api_key(), class = "openFDA_api_key_empty")

  ## Errors if api_key wasn't set or has been removed
  if (keyring::has_keyring_support()) {
    keyring::key_delete("OPENFDA_KEY", "openFDA", "openFDA")
  } else {
    keyring::key_delete("OPENFDA_KEY", "openFDA")
  }
  expect_error(get_api_key(), class = "openFDA_api_key_not_set")
})

test_that("Error if `!interactive()` and missing api_key arg", {
  skip_if(interactive())
  expect_error(set_api_key(),
               class = "openfda_set_api_key_needs_interactive_mode")
})
