getOption("TEST_DIR")
getOption("TEST_DIR_TESTING")
withr::with_options(
  new = list(
    TEST_DIR = system.file("fake-tests", package = "mrgvalprep"),
    TEST_DIR_TESTING = TRUE
  ),{
    # getOption("TEST_DIR") %>% print()
    # getOption("TEST_DIR_TESTING")%>% print()

    test_that("assign_test_ids() returns the correct dataframe", {
      skip_if_over_rate_limit_github()
      skip_if_no_github_pat()
      MILESTONES <- c("v0.6.0")

      parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES, domain = DOMAIN) %>%
        assign_test_ids(test_type = "test_that")

    })

    test_that("assign_test_ids() updates test files", {
      skip_if_over_rate_limit_github()
      skip_if_no_github_pat()
    })

    test_that("assign_test_ids() spits back warnings", {
      skip_if_over_rate_limit_github()
      skip_if_no_github_pat()

      MILESTONES <- c("v0.6.0", "v0.6.1")
    })

  }
)
