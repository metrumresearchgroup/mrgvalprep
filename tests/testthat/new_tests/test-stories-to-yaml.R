test_that("stories_to_yaml() works correctly with google sheet [TST-FOO-033]", {
  skip_if_over_rate_limit_google()
  googlesheets4::gs4_deauth() # public sheets, so no need to authenticate

  spec <- read_spec_gsheets(
    ss_stories = "1LpSX5Rb1XM5-xmQ8Wl2gQjMT5-3FIkuCM7oZhSgvWeI") %>%
    stories_to_yaml(
      file = file.path(tempdir(), "temp.yaml")
    )

  spec_txt <- spec %>% yaml::read_yaml()

  expect_true(file.exists(spec))
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


test_that("stories_to_yaml() works correctly with github issues [TST-FOO-034]", {
  skip_if_over_rate_limit_github()
  skip_if_no_github_pat()

  MILESTONES <- c("v0.6.0", "v0.6.1")

  spec <- parse_github_issues(
    org = ORG, repo = REPO, mile = MILESTONES, domain = DOMAIN) %>%
    stories_to_yaml(file = file.path(tempdir(), "temp.yaml"))

  spec_txt <- spec %>% yaml::read_yaml()

  expect_true(file.exists(spec))

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
