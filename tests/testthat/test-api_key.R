# Set/get API key with keyrings ------------------------------------------------

test_that("You can set and get an API key (with keyring support)", {
  # Only run on windows, as there's the Windows credential manager
  skip_on_os(c("mac", "linux", "solaris"))
  options(keyring_backend = keyring::backend_wincred)
  # Skip if no keyring support - I test a backend *with* keyring support as I
  # develop on Windows. On GitHub, the env-var backend is user which does not
  # have keyring support
  skip_if_not(keyring::has_keyring_support())

  encrypted_api_key <- paste0("TEaDtqdFMq9_Montij5p9IY6T57IyqkbF8IYFVOpk",
                              "-ttxotFUNdJSxgccAnkq4nQhplaf-r3deQ")
  decrypted_key <- httr2::secret_decrypt(encrypted_api_key, "OPENFDA_KEY")


  # 1. In interactive mode, can a user create a new a keyring and then save the
  #    API key? Use mocking to replicate interactive inputs.
  with_mocked_bindings(
    interactive = function() TRUE,
    keyring_unlock = function(keyring) {
      keyring::keyring_unlock(keyring = "openFDA", password = "password")
    },
    keyring_create = function(keyring) {
      keyring::keyring_create(keyring = keyring, password = "password")
    },
    key_set = function(service, user, keyring, prompt) {
      rlang::signal(message = prompt, class = "openfda_api_key_set_with_mock")
      keyring::key_set_with_value(service, user, decrypted_key, keyring)
    },
    code = {
      ## a. No keyring exists --> should issue messages
      try(keyring::key_delete(service = "OPENFDA_KEY",
                              username = "openFDA",
                              keyring = "openFDA"),
          silent = TRUE)
      try(keyring::keyring_delete(keyring = "openFDA"), silent = TRUE)
      expect_message(set_api_key(decrypted_key),
                     class = "openfda_keyring_doesnt_exist") |>
        expect_message(class = "openfda_api_key_set")

      ## b. keyring exists but is locked --> should issue messages
      keyring::keyring_lock(keyring = "openFDA")
      expect_message(set_api_key(decrypted_key),
                     class = "openfda_keyring_locked") |>
        expect_message(class = "openfda_api_key_set")

      ## c. keyring exists and is unlocked, no specific key provided --> should
      ##    still set a new API key, check that right code reached with
      ##    rlang::signal in mocked `key_set()`
      expect_condition(set_api_key(),
                       class = "openfda_api_key_set_with_mock") |>
        expect_message(class = "openfda_api_key_set")
    }
  )

  # 2. In non-interactive mode, can a user set the API key?
  try(keyring::keyring_create(keyring = "openFDA", password = "password"),
      silent = TRUE)
  try(keyring::keyring_unlock(keyring = "openFDA", password = "password"),
      silent = TRUE)
  # `set_api_key() should return NULL but issue a message
  expect_message(
    expect_null(set_api_key(decrypted_key)),
    class = "openfda_api_key_set"
  )

  # 3. Now check whether the stored API key is the same as this one
  with_mocked_bindings(
    interactive = function() TRUE,
    code = {
      expect_equal(get_api_key(), decrypted_key)
    }
  )

  # 4. Throw error if the returned API key is an empty string
  with_mocked_bindings(
    interactive = function() TRUE,
    key_get = function(service, username, keyring) "",
    code = {
      expect_error(get_api_key(), class = "openFDA_api_key_empty")
    }
  )
})

# Set/get API key without keyrings ---------------------------------------------

options(keyring_backend = keyring::backend_env)

test_that("You can set and get an API key", {
  # Test should skip on CRAN automatically from use of `httr2::secret_decrypt()`
  # BUT just to be sure
  skip_on_cran()

  encrypted_api_key <- paste0("TEaDtqdFMq9_Montij5p9IY6T57IyqkbF8IYFVOpk",
                              "-ttxotFUNdJSxgccAnkq4nQhplaf-r3deQ")
  decrypted_key <- httr2::secret_decrypt(encrypted_api_key, "OPENFDA_KEY")

  # 1. Check that you can set an API key through interactive/non-interactive
  #     means

  ## Interactive
  with_mocked_bindings(
    interactive = function() TRUE,
    key_set = function(service, user, prompt) {
      rlang::signal(message = prompt, class = "openfda_api_key_set_with_mock")
      keyring::key_set_with_value(service, user, decrypted_key)
    },
    code = {
      expect_condition(set_api_key(),
                       class = "openfda_api_key_set_with_mock") |>
        expect_message(class = "openfda_api_key_set")
      expect_equal(get_api_key(), decrypted_key)
    }
  )

  keyring::key_set_with_value(service = "OPENFDA_KEY",
                              username = "openFDA",
                              password = "temp-key")
  expect_equal(get_api_key(), "temp-key")

  ## Not interactive
  expect_message(
    expect_null(set_api_key(decrypted_key)),
    class = "openfda_api_key_set"
  )

  # Now check whether the stored API key is the same as this one
  expect_equal(get_api_key(), decrypted_key)
})

test_that("API key functions throw expected errors based on `api_key`", {
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
  expect_error(set_api_key(""),
               class = "openfda_set_api_key_used_empty_string")

  ## Errors if api_key wasn't set or has been removed
  if (keyring::has_keyring_support()) {
    keyring::key_delete("OPENFDA_KEY", "openFDA", "openFDA")
  } else {
    keyring::key_delete("OPENFDA_KEY", "openFDA")
  }
  expect_error(get_api_key(), class = "openFDA_api_key_missing")
})

test_that("Error if `!interactive()` and missing api_key arg", {
  with_mocked_bindings(
    interactive = function() FALSE,
    code = {
      expect_error(set_api_key(),
                   class = "openfda_set_api_key_needs_interactive_mode")
    }
  )
})

test_that("API key functions throw expected errors based on `user`", {
  ## Errors if user input is bad
  expect_error(
    set_api_key(api_key = "tester_string", user = c("one", "two")),
    class = "openFDA_invalid_string_param_length"
  )
  expect_error(
    set_api_key(api_key = "tester_string", user = 239179024359703452790),
    class = "openFDA_invalid_string_param_class"
  )
  expect_error(
    set_api_key(api_key = "tester_string", user = NULL),
    class = "openFDA_invalid_string_param_length"
  )

  ## Errors if user input is bad
  expect_error(
    get_api_key(user = c("one", "two")),
    class = "openFDA_invalid_string_param_length"
  )
  expect_error(
    get_api_key(user = 239179024359703452790),
    class = "openFDA_invalid_string_param_class"
  )
  expect_error(
    get_api_key(user = NULL),
    class = "openFDA_invalid_string_param_length"
  )
})
