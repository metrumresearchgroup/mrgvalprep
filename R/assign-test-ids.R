#' Assigns Test Id's to stories generated via milestones
#'
#'
#' This function will generate Test Id's with the pattern 'TST-FOO-XXX',
#' overwrite test documents to include these new IDs, and return a dataframe
#' with the new TestIds column.
#'
#' @param prefix character string. Prefix for TestIds; usually an acronym of 3 letters signifying the associated package.
#' @param test_path path to where tests are written.
#' @param overwrite (T/F) whether or not to overwrite test files with new test ids
#' @param first_id integer. Desired starting point for test ids; must be greater than or equal to 1.
#'
#' @importFrom dplyr mutate distinct
#' @importFrom testthat find_test_scripts
#' @importFrom stringr str_pad
#' @importFrom purrr map flatten_chr
#' @importFrom tibble tibble
#' @importFrom checkmate assert_string check_integerish
#' @export
assign_test_ids <- function(
  prefix,
  test_path = getOption("mrgvalprep.TEST_LOC"),
  overwrite = TRUE,
  first_id = 1)
{

  assert_string(prefix)
  check_integerish(first_id, lower = 1)

  test_scripts <- find_test_scripts(test_path)
  if (length(test_scripts) == 0) {
    abort("No test files found")
  }

  # Find tests from test files
  tests_vec <- map(test_scripts, parse_tests) %>%
    unlist()

  # Generate Test ID's
  tests <- tibble(TestFile = names(tests_vec),
                  TestIds = parse_test_id(tests_vec),
                  TestNames = strip_test_id(tests_vec, .data$TestIds),
                  new = is.na(.data$TestIds)) %>%
    distinct()

  n_missing <- sum(tests$new)
  if (n_missing == 0) {
    message("All tests have IDs")
  } else {
    tests[tests$new, "TestIds"] <- paste0(
      toupper(prefix),"-TEST-",
      str_pad(first_id:(n_missing + first_id - 1), max(nchar(n_missing + first_id) + 1, 3), pad = "0"))
  }

  ### update test files (Don't overwrite tests with existing ids) ###
  if(overwrite){
    overwrite_tests(test_scripts, filter(tests, .data$new), test_path)
  }

  return(tests)
}


#' Update stories dataframe with newly generated test ids
#'
#' @details
#'
#' Users must first run [assign_test_ids()] to generate a list of test ids and overwrite existing test files.
#'
#' @param stories_df a dataframe of stories returned by `parse_github_issues()`.
#' @param tests dataframe returned by [assign_test_ids()]
#'        Must have the following column names: "TestNames", "TestIds"
#' @param return_missing_ids logical (T/F). Whether or not to return the warning messages pertaining to missing ids.
#'        Note that this will affect piping to other functions, as a named list will be returned instead of a dataframe.
#'
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom tidyr chop unnest
#' @importFrom dplyr full_join
#' @importFrom stringr str_trim
#'
#' @export
milestone_to_test_id <- function(stories_df, tests, return_missing_ids=FALSE){

  if(!all(c("TestNames", "TestIds", "TestFile") %in% names(tests))){
    abort("Check dataframe passed to tests arg. Must have column names 'TestNames', 'TestIds', and 'TestFile'")
  }

  # Ensure test ids are sorted (necessary for replacement)
  tests <- sort_tests_by_nchar(tests)

  dd <- stories_df %>% rename(TestNames = "TestIds")

  merged <- unnest(dd, "TestNames") %>%
    mutate(TestNames = str_trim(.data$TestNames, "both")) %>%
    full_join(tests, by = c("TestNames" = "TestNames"))

  # remove duplicates (introduced in some milestones)
  merged <- merged[!duplicated(merged),]

  ### Scan for missed cases ###
  missing_ids <- filter(merged, is.na(.data$TestIds)) %>%
    select("StoryId", "StoryName", "StoryDescription", "TestNames")
  missing_milestones <- filter(merged, is.na(.data$StoryId)) %>%
    select("TestNames", "TestIds", "TestFile", "new")

  if(nrow(missing_milestones) > 0){
    msg_dat <- data.frame(missing_milestones) # tibble will be truncated
    message("\nWarning: The following tests were not found in github milestones.
            The corresponding Test Id's have still been added to the test files:\n", print_and_capture(msg_dat),"\n")
  }
  if(nrow(missing_ids) > 0){
    msg_dat <- missing_ids %>% chop("TestNames")
    message("\nWarning: The following github issues did not have a matching test.
            Consider modifying the milestone/issue to ensure they are recognized.\n", print_and_capture(as.list(msg_dat)),"\n")
  }


  merged <- merged %>% filter(!is.na(.data$StoryId)) %>%
    mutate(TestIds = ifelse(is.na(.data$TestIds), .data$TestNames, .data$TestIds)) %>%
    select("StoryId", "StoryName", "StoryDescription", "ProductRisk", "TestIds") %>%
    distinct() %>%
    chop("TestIds")

  if(return_missing_ids){
    return(
      list(
        merged = merged,
        missing_milestones = missing_milestones,
        missing_ids = missing_ids
      )
    )
  }else{
    return(merged)
  }

}

#' Reads and returns all tests from test files
#' @importFrom purrr discard
#' @importFrom stringr str_trim str_match
#'
#' @param test_file file path to test script
#'
#' @keywords internal
parse_tests <- function(test_file) {
  lines <- readLines(test_file)
  re <- "^ *(?:test_that|it) *\\( *(['\"])(?<name>.*)\\1 *, *(?:\\{ *)?$"
    test_names <- str_match(lines, re) %>%
      `[`(, "name") %>%
      discard(is.na) %>%
      str_trim(side = "both")
  setNames(test_names, rep(basename(test_file), length(test_names)))
}

#' Sort test ids by number of characters in test description
#'
#' @importFrom dplyr arrange desc
#'
#' @param test_ids data.frame of test ids
#' @keywords internal
sort_tests_by_nchar <- function(test_ids){
  test_ids <- test_ids %>% mutate(nchars=nchar(.data$TestNames))
  test_ids <- test_ids %>% arrange(desc(.data$nchars)) %>%
    select(-"nchars")
  return(test_ids)
}


#' Overwrite test files with new Test Id's
#'
#' @details
#'
#' Make sure to set the following options for testing purposes:
#' `options(mrgvalprep.TEST_LOC = system.file("fake-tests", package = "mrgvalprep"))`
#' `options(mrgvalprep.TESTING = TRUE)`
#'
#' @param test_scripts list of test files
#' @param TestIds dataframe of test Ids, generated in `assign_test_ids()`
#' @param test_path path of tests. Only relevant for testing (i.e. when `getOption("TEST_DIR_TESTING")` returns a value)
#'
#' @importFrom purrr map_chr walk2
#' @importFrom fs dir_create
#'
#' @keywords internal
overwrite_tests <- function(test_scripts, TestIds, test_path){

  # Directory for testing, otherwise overwrite existing test files
  outfiles <-
    if (is.null(getOption("mrgvalprep.TESTING"))) {
      test_scripts
    } else {
      test_dir <- file.path(test_path, "new_tests")
      dir_create(test_dir)
      map_chr(test_scripts, ~ file.path(test_dir, basename(.x)))
    }

  walk2(test_scripts, outfiles, function(infile, outfile) {
    writeLines(
      replace_test_str(readLines(infile),
                       from = TestIds$TestNames, to = TestIds$TestIds),
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
  from <- paste0("^( *(?:test_that|it) *\\( *['\"] *\\Q", from, "\\E)( *['\"])")
  to <- paste0("$1 [", to, "]$2")

  test_file_new <- stri_replace_all_regex(test_file, from, to, vectorize_all=FALSE)
  return(test_file_new)
}

