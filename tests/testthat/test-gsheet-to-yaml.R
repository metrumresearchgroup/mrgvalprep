test_that("read_spec_gsheet() works correctly", {
  skip_if_over_rate_limit_google()
  googlesheets4::gs4_deauth() # public sheets, so no need to authenticate

  spec <- gsheet_to_yaml(
    ss = "1LpSX5Rb1XM5-xmQ8Wl2gQjMT5-3FIkuCM7oZhSgvWeI",
    file = file.path(tempdir(), "tmp.yaml"))
  spec_txt <- spec %>% yaml::read_yaml()

  expect_true(file.exists(spec))
  # expect_equal(length(spec_txt), 26)
  expect_equal(names(spec_txt), c("JUL-S001", "UTL-S001", "VSC-S001"))

  # check that all cols are full of text, and that names are correct
  purrr::walk(names(spec_txt), function(.n) {
    expect_true(inherits(spec_txt[[.n]], "list"))
    expect_equal(names(spec_txt[[.n]]),STORY_TO_GSHEET_ONLY_COLS)
    expect_true(all(purrr::map_lgl(spec_txt[[.n]], ~!is.null(.x))))
    expect_true(inherits(spec_txt[[.n]]$tests, "character"))
  })


  # ensure no duplicate storyIDs
  expect_true(length(names(spec_txt)) > length(unique(length(names(spec_txt)))))

})
