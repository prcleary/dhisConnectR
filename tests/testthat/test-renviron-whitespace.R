test_that(".Renviron replacement ignores whitespace", {

  withr::with_tempdir({
    Sys.setenv(HOME = getwd())

    renviron <- path.expand("~/.Renviron")

    writeLines(
      c(
        'X="old"',
        'Y = "keep"',
        'Z= "also_keep"'
      ),
      renviron
    )

    set_env_vars(X = "new")

    lines <- readLines(renviron)

    expect_false(any(grepl('X\\s*=\\s*"old"', lines)))
    expect_true(any(grepl('X="new"', lines)))
    expect_true(any(grepl('Y = "keep"', lines)))
    expect_true(any(grepl('Z= "also_keep"', lines)))
  })
})
