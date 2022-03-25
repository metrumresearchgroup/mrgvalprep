Sys.setenv(GITHUB_PAT = "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")

options(TEST_DIR = system.file("fake-tests", package = "mrgvalprep")) # for testing
options(TEST_DIR = file.path(devtools::as.package(".")$path, "tests", "testthat")) # set in aaa.R

# New proposed function
assign_github_testids <- function(stories_df, update_tests = TRUE, test_type = c("test_that", "it")){
  dd <- stories_df %>%
    mutate(TestIds = sapply(.data$TestIds, toString))

  test_path <- getOption("TEST_DIR")
  test_scripts <- testthat::find_test_scripts(test_path)
  if (length(test_scripts) == 0) {
    abort("No test files found")
  }

  test_lines <- lapply(test_scripts, readLines)

  test_type <- match.arg(test_type)
  tests_vec <- c()
  for(i in 1:length(test_lines)){
    test_file_i <- test_lines[[i]]
    if(test_type == "test_that"){
      test_rows <- grep('test_that\\("', test_file_i)
    }else if(test_type == "it"){
      test_rows <- grep('it\\("', test_file_i)
    }

    tests_str_i <- test_file_i[test_rows]
    tests_i <- regmatches(tests_str_i, gregexpr("(?<=\")(.*?)(?=\")", tests_str_i, perl = TRUE)) %>% unlist()
    tests_vec <- c(tests_vec,tests_i)
  }
  for(i in 1:length(tests_vec)){
    dd %>% filter(grepl(tests_vec[i], TestIds)) %>% print()
  }


}


# Test calls
## github milestones
MILESTONES <- c("v0.6.0", "v0.6.1")

stories_df <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES, domain = DOMAIN)

parse_github_issues(org = ORG, repo = REPO, mile = MILESTONES, domain = DOMAIN) %>%
  # new function goes here
  stories_to_yaml(file = file.path(tempdir(), "temp.yaml")) %>%
  file.edit()

# google sheets
read_stories_only_gsheet(ss = "1LpSX5Rb1XM5-xmQ8Wl2gQjMT5-3FIkuCM7oZhSgvWeI") %>%
  stories_to_yaml(file = file.path(tempdir(), "temp.yaml")) %>%
  file.edit()



