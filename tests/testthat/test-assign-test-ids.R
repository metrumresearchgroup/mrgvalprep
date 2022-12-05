

withr::with_options(
  new = list(
    mrgvalprep.TEST_LOC = system.file("fake-tests", package = "mrgvalprep"),
    mrgvalprep.TESTING = TRUE
  ),{

    # Differs from parse_test_id, in that there are no brackets ([]) included
    parse_test_id2 <- function(string) {
      str_match(string, regex("([A-Z]+-[A-Z]+-[0-9]+)", ignore_case = TRUE))[, 2]
    }

    test_that("assign_test_ids() returns the correct dataframe", {
      skip_if_over_rate_limit_github()
      skip_if_no_github_pat()

      MILESTONES <- c("v0.6.0", "v0.6.1")
      stories_df <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES,
                                        domain = DOMAIN, prefix = "mrgvalref") %>%
        filter(StoryId != "MRGVALREF-S003")

      test_ids <- assign_test_ids(prefix = "MRGVAL")

      format_stories <- milestone_to_test_id(stories_df = stories_df, tests = test_ids)



      TestIds <- map(format_stories$TestIds, ~ {parse_test_id2(.x)})
      expect_true(any(unlist(map(TestIds, ~ {!is.na(.x)}))))

    })

    test_that("assign_test_ids() updates test files correctly", {
      skip_if_over_rate_limit_github()
      skip_if_no_github_pat()

      MILESTONES <- c("v0.6.0", "v0.6.1")
      stories_df <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES,
                                        domain = DOMAIN, prefix = "mrgvalref") %>%
        filter(StoryId != "MRGVALREF-S003")

      test_ids <- assign_test_ids(prefix = "MRGVAL") %>%
        filter(TestFile != "test-repeated-text.R")

      format_stories <- milestone_to_test_id(stories_df = stories_df, tests = test_ids)


      # Overwritten tests (for testing) show up in inst/fake-tests/new_tests
      test_path <- getOption("mrgvalprep.TEST_LOC")
      test_scripts <- testthat::find_test_scripts(test_path)
      test_dir <- file.path(test_path, "new_tests")
      test_file_loc <- file.path(test_dir, basename(test_scripts))

      tests_vec <- map(test_file_loc, parse_tests) %>%
        unlist()

      expect_true(!any(is.na(parse_test_id(tests_vec))))


      overwritten_tests <- tibble(TestFile = names(tests_vec),
             TestIds = parse_test_id(tests_vec),
             TestNames = strip_test_id(tests_vec, .data$TestIds),
             new = is.na(.data$TestIds)) %>%
        distinct() %>% filter(TestFile != "test-repeated-text.R")

      diff1 <- setdiff(overwritten_tests$TestIds, unlist(format_stories$TestIds))
      diff2 <- setdiff(unlist(format_stories$TestIds), overwritten_tests$TestIds)
      expect_true(rlang::is_empty(diff1))
      expect_true(rlang::is_empty(diff2))
    })

    test_that("assign_test_ids() spits back correct warnings", {
      skip_if_over_rate_limit_github()
      skip_if_no_github_pat()

      # expect both warnings
      MILESTONES <- c("v0.6.0")
      stories_df <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES,
                                        domain = DOMAIN, prefix = "mrgvalref")

      test_ids <- assign_test_ids(prefix = "MRGVAL")


      expect_message(milestone_to_test_id(stories_df = stories_df, tests = test_ids),
                     "The following tests were not found in github milestones")
      expect_message(milestone_to_test_id(stories_df = stories_df, tests = test_ids),
                     "The following github issues did not have a matching test")


      # expect github issue warning only
      MILESTONES <- c("v0.6.0", "v0.6.1")
      stories_df <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES,
                                        domain = DOMAIN, prefix = "mrgvalref")

      expect_message(milestone_to_test_id(stories_df = stories_df, tests = test_ids),
                     "The following github issues did not have a matching test")

    })

    test_that("milestone_to_test_id() with return_missing_ids", {
      skip_if_over_rate_limit_github()
      skip_if_no_github_pat()

      # expect both warnings
      MILESTONES <- c("v0.6.0", "v0.6.1")
      stories_df <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES,
                                        domain = DOMAIN, prefix = "mrgvalref")

      test_ids <- assign_test_ids(prefix = "MRGVAL", overwrite = TRUE)


      dd <- milestone_to_test_id(stories_df = stories_df, tests = test_ids, return_missing_ids = TRUE)

      expect_true(all(c("merged", "missing_milestones", "missing_ids") %in% names(dd)))
      expect_true(nrow(dd$missing_ids) == 23)

    })

    test_that("test name string is repeated in a non-testthat() environment", {
      skip_if_over_rate_limit_github()
      skip_if_no_github_pat()


      test_ids <- assign_test_ids(prefix = "MRGVAL", overwrite = TRUE)

      fake_test_path <- file.path(getOption("mrgvalprep.TEST_LOC"), "new_tests", "test-repeated-text.R")
      fake_test <- parse_tests(fake_test_path)

      expect_true(length(fake_test) == 3)
      expect_true(any(duplicated(fake_test)))
      expect_true(dplyr::n_distinct(fake_test) == 2)

    })

  }
)
