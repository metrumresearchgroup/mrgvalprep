test_that("read_spec_gsheet() works correctly [TST-FOO-028]", {
  skip_if_over_rate_limit_google()
  googlesheets4::gs4_deauth() # public sheets, so no need to authenticate
  spec <- read_spec_gsheets(
    ss_stories = "1HgsxL4qfYK-wjB-nloMilQiuBSLV6FZq_h2ToB6QNlI",
    ss_req = "1SnyUzxVDUUJFtMGEi2x4zJE0iB2JDV1Np-ivhaUkODk"
  )

  expect_equal(nrow(spec), SPECS_DF_ROWS_GS)
  expect_equal(ncol(spec), SPECS_DF_COLS_GS)

  # check that all cols except TestIds are full of text
  purrr::walk(names(spec)[1:ncol(spec)-1], function(.n) {
    expect_true(inherits(spec[[.n]], "character"))
    expect_true(all(purrr::map_lgl(spec[[.n]], ~!is.null(.x))))
    expect_true(all(purrr::map_lgl(spec[[.n]], ~nchar(.x) > 1)))
  })

  # check that TestIds are all character vectors
  expect_true(inherits(spec[["TestIds"]], "list"))
  expect_true(all(purrr::map_lgl(spec[["TestIds"]], ~!is.null(.x))))
  expect_true(all(purrr::map_lgl(spec[["TestIds"]], ~inherits(.x, "character"))))
  expect_true(all(purrr::map_lgl(spec[["TestIds"]], ~length(.x) > 0)))

  # there are duplicate StoryId's because the get blown out for multiple requirements
  expect_true(length(spec$StoryId) > length(unique(spec$StoryId)))

})
