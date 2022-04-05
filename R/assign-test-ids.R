#' Assigns Test Id's to stories generated via milestones
#'
#'
#' This function will generate Test Id's with the pattern 'TST-FOO-XXX',
#' overwrite test documents to include these new IDs, and return a dataframe
#' with the new TestIds column.
#'
#'
#' @param test_path path to where tests are written.
#' @param overwrite_tests (T/F) whether or not to overwrite test files with new test ids
#'
#' @importFrom dplyr mutate arrange
#' @importFrom stringr str_pad
#' @importFrom purrr map flatten_chr
#' @export
assign_test_ids <- function(
  test_path = getOption("TEST_DIR"),
  overwrite_tests = TRUE
){

  test_scripts <- testthat::find_test_scripts(test_path)
  if (length(test_scripts) == 0) {
    abort("No test files found")
  }


  # Find tests from test files
  tests_vec <- map(test_scripts, ~ parse_tests(readLines(.x))) %>%
    flatten_chr() %>%
    unique()

  # Generate Test ID's
  num_tests <- length(tests_vec)
  id_vals <- str_pad(1:num_tests, nchar(num_tests) + 1, pad = "0")

  # Order by number of characters (decreasing) - necessary for when certain tests include exact text of another test
  test_ids <- data.frame(TestNames = tests_vec, TestId = paste0("TST-FOO-", id_vals, ""))
  test_ids <- sort_tests_by_nchar(test_ids)
  test_ids <- test_ids %>% mutate(newTestID = paste0(.data$TestNames, " [",.data$TestId,"]"))

  ### update test files ###
  overwrite_tests(test_scripts, test_ids, test_path)

  return(test_ids)
}


#' Update stories dataframe with newly generated test ids
#'
#' @details
#'
#' Users must first run [assign_test_ids()] to generate a list of test ids and overwrite existing test files.
#'
#' @param stories_df a dataframe of stories returned by `parse_github_issues()`.
#'
#' @importFrom stringi stri_replace_all_fixed
#'
#' @export
milestone_to_test_id <- function(stories_df, test_ids){

  # Ensure test ids are still sorted
  test_ids <- sort_tests_by_nchar(test_ids)

  dd <- stories_df %>%
    rename(TestNames = .data$TestIds) %>% mutate(TestIds = NA)

  # Replace test names with test ID's
  for(i in 1:nrow(dd)){
    str_search_i <- dd$TestNames[i]
    pattern <- test_ids$TestNames[is.na(parse_test_id(test_ids$TestNames))] # wont overwrite tests with existing ids
    replacement <- test_ids$TestId

    new_row <- map(str_search_i, ~ {stri_replace_all_fixed(.x, pattern, replacement, vectorize_all=FALSE) })

    # remove duplicates (introduced in some milestones)
    dd$TestIds[i] <- list(new_row[[1]][!duplicated(new_row[[1]])])
  }


  ### Scan for missed cases ###

  # Tests missed when scanning files
  milestone_tests_ref <- dd$TestIds
  # names(milestone_tests_ref) <- dd$StoryId
  milestone_tests_ref <- lapply(milestone_tests_ref, strsplit, split=", ") %>% unlist()
  # Tests in package files, but not in milestones
  missed_milestones <- setdiff(test_ids$TestId, milestone_tests_ref)
  # Tests in milestones, but couldnt find a matching test in package files
  missed_tests <- setdiff(milestone_tests_ref, test_ids$TestId)

  if(length(missed_milestones) > 0){
    tests_missing <- data.frame(TestId=missed_milestones, missing = TRUE)
    msg_dat <- full_join(test_ids, tests_missing) %>% filter(missing==TRUE) %>% select(-c(.data$newTestID, .data$nchars, .data$missing))
    message("\nWarning: The following tests were not found in github milestones.
            The corresponding Test Id's have still been added to the test files:\n", print_and_capture(msg_dat),"\n")
  }
  if(length(missed_tests) > 0){
    missed_df <- purrr::map(dd$TestIds, ~ {match(missed_tests, .x) })
    missed_df <- map(missed_df, ~{!all(is.na(.x))}) %>% unlist()
    missed_df <- dd[missed_df,] %>% select(-c(.data$TestNames))
    message("\nWarning: The following github issues did not have a matching test.
            Consider modifying the milestone/issue to ensure they are recognized.\n", print_and_capture(as.list(missed_df)),"\n")
  }


  dd <- dd %>% dplyr::select(-c(.data$TestNames))

  return(dd)

}

#' Reads and returns all tests from test files
#' @param lines character vector. Output of `readLines()` for a single test
#'
#' @keywords internal
parse_tests <- function(lines) {
  re <- "^ *(?:test_that|it) *\\( *(['\"])(?<name>.*)\\1 *, *(?:\\{ *)?$"
  stringr::str_match(lines, re) %>%
    `[`(, "name") %>%
    purrr::discard(is.na) %>%
    stringr::str_trim(side = "both")
}

#' Sort test ids by number of characters in test description
#' @param test_ids data.frame of test ids
#' @keywords internal
sort_tests_by_nchar <- function(test_ids){
  test_ids <- test_ids %>% mutate(nchars=nchar(.data$TestNames))
  test_ids <- test_ids %>% arrange(desc(nchars)) %>%
    select(-nchars)
  return(test_ids)
}


#' Overwrite test files with new Test Id's
#'
#' @details
#'
#' Make sure to set the following options for testing purposes:
#' `options(TEST_DIR = system.file("fake-tests", package = "mrgvalprep"))`
#' `options(TEST_DIR_TESTING = TRUE)`
#'
#' @param test_scripts list of test files
#' @param TestIds dataframe of test Ids, generated in `assign_test_ids()`
#' @param test_path path of tests. Only relevant for testing (i.e. when `getOption("TEST_DIR_TESTING")` returns a value)
#'
#' @keywords internal
overwrite_tests <- function(test_scripts, TestIds, test_path){
  test_lines <- lapply(test_scripts, readLines)

  # Directory for testing, otherwise overwrite existing test files
  outfiles <-
    if (is.null(getOption("TEST_DIR_TESTING"))) {
      test_scripts
    } else {
      test_dir <- file.path(test_path, "new_tests")
      fs::dir_create(test_dir)
      purrr::map_chr(test_scripts, ~ file.path(test_dir, basename(.x)))
    }

  purrr::walk2(test_scripts, outfiles, function(infile, outfile) {
    writeLines(
      replace_test_str(readLines(infile),
                       from = TestIds$TestNames, to = TestIds$newTestID),
      outfile)
  })

}

#'
#' @keywords internal
print_and_capture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}

#' Replace test names in test file
#'
#' @description
#' Replace test names ('str') in test file with string containing new TestId ('str' + `TST-FOO-XXX`)
#'
#' @param test_file character vector of test file
#' @param from character vector. To replace
#' @param to character vector. Replacement
#'
#' @importFrom stringi stri_replace_all_regex
#' @keywords internal
replace_test_str <- function(test_file, from, to){
  from <- paste0(paste0("(['\"] *)\\Q", from,"\\E( *['\"])"))
  to <- paste0("$1",to,"$2")

  test_file_new <- stri_replace_all_regex(test_file, from, to, vectorize_all=FALSE)
  return(test_file_new)
}
