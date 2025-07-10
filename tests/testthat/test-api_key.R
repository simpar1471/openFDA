# Set-up -----------------------------------------------------------------------

## Use env-var backed; supported on all systems (and default on GitHub)
options(keyring_backend = keyring::backend_env)

# Set/get API key with keyrings ------------------------------------------------

test_that("You can set and get an API key (with keyring support)", {
  # 1. In interactive mode, can a user create a new keyring and save the API
  #    key? Mocking interactive() + keyring functions.
  with_mocked_bindings(
    interactive = function() TRUE,
    has_keyring_support = function() TRUE,
    keyring_list = function() list(keyring = "not-openFDA"),
    keyring_create = function(keyring) {
      rlang::signal(message = keyring, class = "openfda_test_keyring_created")
    },
    key_set = function(service, username, keyring, prompt) {
      rlang::signal(message = prompt, class = "openfda_test_api_key_set")
    },
    code = {
      set_api_key() |>
        expect_null() |>
        expect_message(class = "openfda_keyring_doesnt_exist") |>
        expect_condition(class = "openfda_test_keyring_created") |>
        expect_condition(class = "openfda_test_api_key_set") |>
        expect_message(class = "openfda_api_key_set")
  })

  ## 2. Keyring exists but is locked
  with_mocked_bindings(
    interactive = function() TRUE,
    has_keyring_support = function() TRUE,
    keyring_list = function() list(keyring = "openFDA"),
    keyring_is_locked = function (keyring) TRUE,
    keyring_unlock = function(keyring) {
      rlang::signal(message = keyring, class = "openfda_test_keyring_unlocked")
    },
    key_set = function(service, username, keyring, prompt) {
      rlang::signal(message = prompt, class = "openfda_test_api_key_set")
    },
    code = {
      set_api_key() |>
        expect_null() |>
        expect_condition(class = "openfda_test_keyring_unlocked") |>
        expect_message(class = "openfda_keyring_locked") |>
        expect_condition(class = "openfda_test_api_key_set") |>
        expect_message(class = "openfda_api_key_set")
    }
  )

  # 4. Get API key from an existing keyring
  test_key <- "TEST-KEY-01"
  with_mocked_bindings(
    interactive = function() FALSE,
    keyring_list = function() list(keyring = "openFDA"),
    has_keyring_support = function() TRUE,
    keyring_is_locked = function(openFDA_keyring) FALSE,
    key_get = function(service, username, keyring) {
      rlang::signal(message = "{service}; {username}; {keyring}",
                    class = "openfda_test_api_key_get_from_keyring")
      test_key
    },
    code = {
      get_api_key() |>
        expect_equal(test_key) |>
        expect_condition(class = "openfda_test_api_key_get_from_keyring") |>
        expect_no_error(class = "openfda_api_key_missing") |>
        expect_no_error(class = "openfda_api_key_missing")
    }
  )

  # 5. Throw error if the returned API key is an empty string
  with_mocked_bindings(
    interactive = function() TRUE,
    key_get = function(service, username, keyring) "",
    code = {
      expect_error(get_api_key(), class = "openFDA_api_key_empty")
    }
  )
})

# Set/get API key without keyrings ---------------------------------------------

test_that("You can set and get an API key", {
  # 1. Check that you can set an API key interactively
  test_key <- "TEST-KEY-01"
  with_mocked_bindings(
    interactive = function() TRUE,
    key_set = function(service, username, prompt) {
      rlang::signal(message = prompt, class = "openfda_api_key_set_with_mock")
      keyring::key_set_with_value(service, username, password = test_key)
    },
    code = {
      set_api_key() |>
        expect_null() |>
        expect_condition(class = "openfda_api_key_set_with_mock") |>
        expect_message(class = "openfda_api_key_set")
      expect_equal(get_api_key(), test_key)
    }
  )

  # 2. Check that you can set an API key by passing in value
  test_key <- "TEST-KEY-02"
  with_mocked_bindings(
    interactive = function() FALSE,
    key_set_with_value = function(service, username, password) {
      rlang::signal(message = password, class = "openfda_api_key_set_with_mock")
      keyring::key_set_with_value(service, username, password = password)
    },
    code = {
      set_api_key(test_key) |>
        expect_null() |>
        expect_condition(class = "openfda_api_key_set_with_mock") |>
        expect_message(class = "openfda_api_key_set")
      expect_equal(get_api_key(), test_key)
    }
  )
})

test_that("API key functions throw expected errors", {
  ## Errors if api_key input is bad class/length
  expect_error(
    set_api_key(c("string_one", "string_two")),
    class = "openFDA_invalid_string_param_length"
  )
  expect_error(
    set_api_key(239179024359703452790),
    class = "openFDA_invalid_string_param_class"
  )
  ## Error if set_api_key() input is empty
  expect_error(set_api_key(""),
               class = "openfda_set_api_key_used_empty_string")

  ## Error if api_key wasn't set or has been removed
  with_mocked_bindings(
    has_keyring_support = function() FALSE,
    key_get = function(service, username, keyring = NULL) {
      cli::cli_abort(
        message = "Mocking keyring retrieval error with service {.val
                   {service}} and user {.val {user}}",
        class = "openfda_test_key_not_found"
      )
    },
    code = expect_error(get_api_key(), class = "openFDA_api_key_missing")
  )

  ## Error if not interactive and api_key not supplied
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
