test_that("NULL values trigger prompting", {
  withr::with_tempdir({
    mockery::stub(set_env_vars, "readline", function(prompt)
      "secret")

    mockery::stub(set_env_vars, "interactive", TRUE)

    set_env_vars(TEST_KEY = NULL, persist = FALSE)

    expect_equal(Sys.getenv("TEST_KEY"), "secret")
  })
})

test_that("NULL errors in non-interactive sessions", {
  withr::with_tempdir({
    mockery::stub(set_env_vars, "interactive", FALSE)

    expect_error(set_env_vars(TEST_KEY = NULL, persist = FALSE),
                 "non-interactive session")
  })
})
