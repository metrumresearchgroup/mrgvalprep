#' Assigns Test Id's to stories generated via milestones
#'
#' @details
#'
#' This function will generate Test Id's with the pattern 'TST-FOO-XXX',
#' overwrite test documents to include these new IDs, and return a dataframe
#' with the new TestIds column.
#'
#'
#' @param stories_df a dataframe of stories returned by `parse_github_issues()`.
#' @param test_type string. Denotes whether the tests were written via `test_that()` or
#'        `describe()`/`it()` functionality. Specify as 'test_that' or 'it'
#'
#'
#' @importFrom testthat find_test_scripts
#' @importFrom stringi stri_replace_all_fixed stri_replace_all_regex
#' @importFrom purrr map map2
#' @export
assign_test_ids <- function(stories_df, test_type = c("test_that", "it")){

  dd <- stories_df %>%
    rename(TestNames = .data$TestIds) %>% mutate(TestIds = NA)

  test_path <- getOption("TEST_DIR")
  test_scripts <- testthat::find_test_scripts(test_path)
  if (length(test_scripts) == 0) {
    abort("No test files found")
  }

  test_type <- match.arg(test_type)

  # Find tests from test files
  tests_vec <- parse_tests_from_files(test_scripts, test_type)

  # Generate Test ID's
  Id_vals <- seq.int(length(tests_vec))
  for(i in 1:length(Id_vals)){
    if(nchar(Id_vals[i])==1){
      Id_vals[i] <- paste0("00",Id_vals[i])
    }else if(nchar(Id_vals[i])==2){
      Id_vals[i] <- paste0("0",Id_vals[i])
    }
  }
  # Order by number of characters (decreasing) - necessary for when certain tests include exact text of another test
  TestIds <- data.frame(TestNames = tests_vec, TestId = paste0("TST-FOO-", Id_vals, ""))
  TestIds <- TestIds %>% mutate(nchars=nchar(as.character(.data$TestNames)))
  TestIds <- TestIds[with(TestIds, order(nchars, TestNames, decreasing=TRUE)), ]

  # Replace test names with test ID's
  for(i in 1:nrow(dd)){
    str_search_i <- dd$TestNames[i]
    pattern <- TestIds$TestNames[is.na(parse_test_id(TestIds$TestNames))] # wont overwrite tests with existing ids
    replacement <- TestIds$TestId

    new_row <- purrr::map(str_search_i, ~ {stri_replace_all_fixed(.x, pattern, replacement, vectorize_all=FALSE) })

    # remove duplicates (introduced in some milestones)
    dd$TestIds[i] <- list(new_row[[1]][!duplicated(new_row[[1]])])
  }

  TestIds <- TestIds %>% mutate(newTestID = paste0(.data$TestNames, " [",.data$TestId,"]"))

  ### update test files ###
  overwrite_tests(test_scripts, TestIds)

  ### Scan for missed cases ###

  # Tests missed when scanning files
  milestone_tests_ref <- dd$TestIds
  # names(milestone_tests_ref) <- dd$StoryId
  milestone_tests_ref <- lapply(milestone_tests_ref, strsplit, split=", ") %>% unlist()
  # Tests in package files, but not in milestones
  missed_milestones <- setdiff(TestIds$TestId, milestone_tests_ref)
  # Tests in milestones, but couldnt find a matching test in package files
  missed_tests <- setdiff(milestone_tests_ref, TestIds$TestId)

  if(length(missed_milestones) > 0){
    tests_missing <- data.frame(TestId=missed_milestones, missing = TRUE)
    msg_dat <- full_join(TestIds, tests_missing) %>% filter(missing==TRUE) %>% select(-c(.data$newTestID, .data$nchars, .data$missing))
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
#' @param test_scripts character vector. File paths of tests
#' @param test_type string. Denotes whether the tests were written via `test_that()` or
#'        `describe()`/`it()` functionality. Specify as 'test_that' or 'it'
#'
#' @keywords internal
parse_tests_from_files <- function(test_scripts, test_type){
  test_lines <- lapply(test_scripts, readLines)

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
  tests_vec <- tests_vec[!duplicated(tests_vec)] # remove any duplicates introduced from human error
  return(tests_vec)
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
#'
#' @keywords internal
overwrite_tests <- function(test_scripts, TestIds){
  test_lines <- lapply(test_scripts, readLines)
  new_test_ids <- TestIds$newTestIDs

  # Directory for testing, otherwise overwrite existing test files
  if(!is.null(getOption("TEST_DIR_TESTING"))){
    test_dir <- file.path(getOption("TEST_DIR"), "new_tests")
    if(!dir.exists(test_dir)){ dir.create(test_dir) }
  }

  # TestIds$missing <- NA
  for(i in 1:length(test_lines)){
    test_file_i <- test_lines[[i]]

    # Make replacements all at once
    test_file_new_i <- replace_test_str(test_file_i, from = TestIds$TestNames, to = TestIds$newTestID)

    if(is.null(getOption("TEST_DIR_TESTING"))){
      test_file_loc <- test_scripts[i]
    }else{
      test_file_loc <- file.path(test_dir, basename(test_scripts[i]))
    }
    writeLines(test_file_new_i, test_file_loc)
  }

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
#' @keywords internal
replace_test_str <- function(test_file, from, to){
  from <- paste0(paste0("(['\"] *)\\Q", from,"\\E( *['\"])"))
  to <- paste0("$1",to,"$2")

  test_file_new <- stringi::stri_replace_all_regex(test_file, from, to, vectorize_all=FALSE)
  return(test_file_new)
}
