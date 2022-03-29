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
#' @importFrom stringi stri_replace_all_fixed
#' @export
assign_test_ids <- function(stories_df, test_type = c("test_that", "it")){
  dd <- stories_df %>%
    mutate(TestName = sapply(.data$TestIds, toString), TestIds = NA)

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
  tests_vec <- tests_vec[!duplicated(tests_vec)] # remove any duplicates introduced from human error

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
  TestIds <- data.frame(TestName = tests_vec, TestId = paste0("TST-FOO-", Id_vals, ""))
  TestIds <- transform(TestIds, nchars=nchar(as.character(TestName)))
  TestIds <- TestIds[with(TestIds, order(nchars, TestName, decreasing=TRUE)), ]

  # Replace test names with test ID's
  for(i in 1:nrow(dd)){
    str_search_i <- dd$TestName[i]
    pattern <- TestIds$TestName
    replacement <- TestIds$TestId

    new_row <- stringi::stri_replace_all_fixed(str_search_i, pattern, replacement, vectorize_all=FALSE)
    # need a warning here for when testId's are not replaced (happens on 3rd iteration)
    dd$TestIds[i] <- new_row

  }

  TestIds <- TestIds %>% mutate(newTestID = paste0(TestName, " [",TestId,"]"))
  # update test files
  overwrite_tests(test_scripts, TestIds)

  # # Scan for missed cases
  # tests_list <- str_match_all(dd$TestIds, "([A-Z]+-[A-Z]+-[0-9]+)")
  # tests_list <- lapply(seq_along(tests_list), function(i) {
  #   data.frame(tests[[i]]) [,2]
  # })
  # # names(tests_list) <- dd$StoryId
  # milestone_tests <- unlist(unname(tests_list))

  # Tests missed when scanning files
  milestone_tests_ref <- as.list(dd$TestIds)
  # lapply(milestone_tests_ref, strsplit, split=", ")
  names(milestone_tests_ref) <- dd$StoryId
  tmp <- lapply(milestone_tests_ref, strsplit, split=", ") %>% unlist()
  missed_milestones <- setdiff(TestIds$TestId, tmp)
  missed_tests <- setdiff(tmp, TestIds$TestId)



  # Tests missing from milestones
  # setdiff(milestone_tests, TestIds$TestId) # very unlikely scenario (in milestone but not test files)
  # tests_missing <- data.frame(TestId=setdiff(TestIds$TestId, milestone_tests), missing = TRUE)
  # TestIds <- full_join(TestIds, tests_missing)

  # if(any(TestIds$missing)){
  #   msg_dat <- TestIds %>% filter(missing==TRUE) %>% select(-c(newTestID, nchars))
  #   message("Warning: The following tests were not found in github milestones.
  #           The corresponding Test Id's have still been added to the test files.\n", print_and_capture(msg_dat))
  #
  # }
  if(length(missed_milestones) > 0){
    tests_missing <- data.frame(TestId=missed_milestones, missing = TRUE)
    TestIds <- full_join(TestIds, tests_missing) %>% filter(missing==TRUE) %>% select(-c(newTestID, nchars, missing))
    message("Warning: The following tests were not found in github milestones.
            The corresponding Test Id's have still been added to the test files.\n", print_and_capture(msg_dat))
  }
  if(length(missed_tests) > 0){
    tests_missing <- data.frame(TestId=missed_milestones, missing = TRUE)
    TestIds <- full_join(TestIds, tests_missing) %>% filter(missing==TRUE) %>% select(-c(newTestID, nchars))
    message("Warning: The following github issues did not have a matching test:\n", print_and_capture(data.frame(missed_tests)))
  }


  dd <- dd %>% dplyr::select(-TestName)

  return(dd)

  # TODO:
  # generate informative warnings (not a simple task)
  # write tests for new function

}


#' Overwrite test files with new Test Id's
#'
#' @details
#'
#' Make sure to set the following options for testing purposes:
#' `options(TEST_DIR = system.file("fake-tests", package = "mrgvalprep"))`
#' `options(TEST_DIR_TESTING = TRUE)`
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

  TestIds$missing <- NA
  for(i in 1:length(test_lines)){
    test_file_i <- test_lines[[i]]

    # # Scan for tests in files separately
    # # m <- gregexpr("(?<=\")(.*?)(?=\")", test_file_i, perl = TRUE)
    # tmp <- sapply(TestIds$TestName, function(test.x){
    #   tests_i <- regmatches(test_file_i, m) %>% unlist()
    #   grepl(test.x, test_file_i)
    # }) %>% unlist()


    # Make replacements all at once
    test_file_new_i <- stringi::stri_replace_all_fixed(test_file_i, TestIds$TestName, TestIds$newTestID, vectorize_all=FALSE)
    if(is.null(getOption("TEST_DIR_TESTING"))){
      test_file_loc <- test_scripts[i]
    }else{
      test_file_loc <- file.path(test_dir, basename(test_scripts[i]))
    }
    writeLines(test_file_new_i, test_file_loc)
  }

}

#' @keywords internal
print_and_capture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}
