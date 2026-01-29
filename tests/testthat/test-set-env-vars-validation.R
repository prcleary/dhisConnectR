
test_that("empty strings are rejected", {

  expect_error(
    set_env_vars(BAD = ""),
    "Missing values"
  )
})

# TODO
# test_that("all variables must be named", { ... })
# test_that("at least one variable is required", { ... })
