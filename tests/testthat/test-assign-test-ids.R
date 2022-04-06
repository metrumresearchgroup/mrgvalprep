

withr::with_options(
  new = list(
    TEST_DIR = system.file("fake-tests", package = "mrgvalprep"),
    TEST_DIR_TESTING = TRUE
  ),{

    # Differs from parse_test_id, in that there are no brackets ([]) included
    parse_test_id2 <- function(string) {
      str_match(string, "([A-Z]+-[A-Z]+-[0-9]+)")[, 2]
    }

    test_that("assign_test_ids() returns the correct dataframe", {
      skip_if_over_rate_limit_github()
      skip_if_no_github_pat()

      MILESTONES <- c("v0.6.0", "v0.6.1")
      stories_df <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES, domain = DOMAIN) %>%
        filter(StoryId != "mrgvalidatetestreference-3")

      test_ids <- assign_test_ids()

      format_stories <- milestone_to_test_id(stories_df = stories_df, tests = test_ids)



      TestIds <- purrr::map(format_stories$TestIds, ~ {parse_test_id2(.x)})
      expect_true(any(unlist(map(TestIds, ~ {!is.na(.x)}))))

    })

    test_that("assign_test_ids() updates test files correctly", {
      skip_if_over_rate_limit_github()
      skip_if_no_github_pat()

      MILESTONES <- c("v0.6.0", "v0.6.1")
      stories_df <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES, domain = DOMAIN) %>%
        filter(StoryId != "mrgvalidatetestreference-3")

      test_ids <- assign_test_ids()

      format_stories <- milestone_to_test_id(stories_df = stories_df, tests = test_ids)


      # Overwritten tests (for testing) show up in inst/fake-tests/new_tests
      test_path <- getOption("TEST_DIR")
      test_scripts <- testthat::find_test_scripts(test_path)
      test_dir <- file.path(test_path, "new_tests")
      test_file_loc <- file.path(test_dir, basename(test_scripts))

      tests_vec <- map(test_file_loc, ~ parse_tests(readLines(.x))) %>%
        flatten_chr() %>%
        unique()

      expect_true(!any(is.na(parse_test_id(tests_vec))))


      overwritten_tests <- data.frame(TestNames = tests_vec) %>%
        mutate(
          TestId = parse_test_id(tests_vec),
          TestNames = strip_test_id(.data$TestNames, .data$TestId))

      diff1 <- setdiff(overwritten_tests$TestId, unlist(format_stories$TestIds))
      diff2 <- setdiff(unlist(format_stories$TestIds), overwritten_tests$TestId)
      expect_true(rlang::is_empty(diff1))
      expect_true(rlang::is_empty(diff2))
    })

    test_that("assign_test_ids() spits back correct warnings", {
      skip_if_over_rate_limit_github()
      skip_if_no_github_pat()

      # expect both warnings
      MILESTONES <- c("v0.6.0")
      stories_df <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES, domain = DOMAIN)

      test_ids <- assign_test_ids()


      expect_message(milestone_to_test_id(stories_df = stories_df, tests = test_ids),
                     "The following tests were not found in github milestones")
      expect_message(milestone_to_test_id(stories_df = stories_df, tests = test_ids),
                     "The following github issues did not have a matching test")


      # expect github issue warning only
      MILESTONES <- c("v0.6.0", "v0.6.1")
      stories_df <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES, domain = DOMAIN)

      expect_message(milestone_to_test_id(stories_df = stories_df, tests = test_ids),
                     "The following github issues did not have a matching test")

    })

  }
)
