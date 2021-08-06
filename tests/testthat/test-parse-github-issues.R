test_that("get_issues() pulls from Github", {
  release_issues <- get_issues(org = ORG, repo = REPO, mile = MILESTONE, domain = DOMAIN)
  # check a few things that likely won't change if we update some stories
  expect_true(nrow(release_issues) > 5)
  expect_true(all(stringr::str_detect(release_issues$body, stringr::regex("Summary.+Tests", dotall = TRUE))))
  expect_true(all(stringr::str_detect(release_issues$milestone, MILESTONE)))
  expect_true(all(stringr::str_detect(release_issues$resource_path, REPO)))

})
