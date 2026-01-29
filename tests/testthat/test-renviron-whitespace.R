test_that(".Renviron replacement ignores whitespace", {
  withr::with_tempdir({
    renviron <- file.path(getwd(), '.Renviron')

    writeLines(c('X="old"', 'Y = "keep"', 'Z= "also_keep"'), renviron)

    set_env_vars(X = "new", renviron = renviron)

    lines <- readLines(renviron)

    expect_false(any(grepl('X\\s*=\\s*"old"', lines)))
    expect_true(any(grepl('X="new"', lines)))
    expect_true(any(grepl('Y = "keep"', lines)))
    expect_true(any(grepl('Z= "also_keep"', lines)))
  })
})

test_that("re-running does not duplicate entries", {
  withr::with_tempdir({
    renviron <- file.path(getwd(), '.Renviron')

    set_env_vars(A = "1", renviron = renviron)
    set_env_vars(A = "1", renviron = renviron)

    lines <- readLines(renviron)

    expect_equal(sum(grepl("^A\\s*=", lines)), 1)
  })
})


test_that("persist = FALSE does not modify .Renviron", {
  withr::with_tempdir({
    renviron <- file.path(getwd(), ".Renviron")

    # Simulate an existing file safely
    writeLines(c('EXISTING_VAR="keep"', 'OTHER="also_keep"'), renviron)

    before <- readLines(renviron)

    set_env_vars(TEMP = "x",
                 persist = FALSE,
                 renviron = renviron)

    after <- readLines(renviron)

    expect_identical(after, before)
    expect_equal(Sys.getenv("TEMP"), "x")
  })
})
