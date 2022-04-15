test_that("parse_github_issues() pulls from Github", {
  skip_if_over_rate_limit_github()
  skip_if_no_github_pat()

  spec <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONE, domain = DOMAIN, prefix = "FOO")
  expect_equal(nrow(spec), STORIES_DF_ROWS_GHP)
  expect_equal(ncol(spec), STORIES_DF_COLS_GHP)

  # check that all cols except TestIds are full of text
  purrr::walk(names(spec)[1:ncol(spec)-1], function(.n) {
    expect_true(inherits(spec[[.n]], "character"))
    expect_true(all(purrr::map_lgl(spec[[.n]], ~!is.null(.x))))
    expect_true(all(purrr::map_lgl(spec[[.n]], ~nchar(.x) > 1)))
  })

  # check that TestIds are all character vectors except the last (empty) one
  expect_true(inherits(spec[["TestIds"]], "list"))
  expect_equal(
    purrr::map_lgl(spec[["TestIds"]], ~length(.x) > 1),
    c(TRUE,  TRUE,  TRUE,  TRUE,  TRUE, FALSE)
  )

})

test_that("get_issues() pulls from Github", {
  skip_if_over_rate_limit_github()
  skip_if_no_github_pat()

  release_issues <- get_issues(org = ORG, repo = REPO, mile = MILESTONE, domain = DOMAIN)
  expect_equal(nrow(release_issues), STORIES_DF_ROWS_GHP)

  # check a few things that likely won't change if we update some stories
  expect_true(all(stringr::str_detect(release_issues$body, stringr::regex("Summary.+Tests", dotall = TRUE))))
  expect_true(all(stringr::str_detect(release_issues$milestone, MILESTONE)))
  expect_true(all(stringr::str_detect(release_issues$resource_path, REPO)))

})
