#' Parse testthat output
#'
#' @param result List output as reported by [testthat::ListReporter]
#' @param roll_up_ids If `FALSE`, the default, will leave duplicated Test ID's
#'   as is. If `TRUE`, will roll up any duplicated (non-NA) ID's so that they
#'   are unique and the passed/failed count reflects the total sums for a tests
#'   with a given ID. The test names for a set of rolled up ID's must share a
#'   common prefix, which will be used as the new test name. Any trailing
#'   spaces, slashes, colons, or underscores will be removed from the extracted
#'   prefix.
#' @return A tibble formatted according to `mrgvalidate::input_formats`
#' @seealso `mrgvalidate::input_formats`, `mrgvalidate::create_validation_docs()`
#' @importFrom purrr map_chr map_lgl map_dfr
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace fixed
#' @export
parse_testthat_list_reporter <- function(result, roll_up_ids = FALSE) {
  test_results <- map_dfr(result, function(.r) {
    .t <- unique(map_chr(.r$results, ~ .x$test))
    if (length(.t) > 1) {
      abort(paste("DEV ERROR: parsed more than one test name from results:", paste(.t, collapse = ", ")))
    }
    tibble::tibble(TestName = .t,
                   passed = sum(map_lgl(.r$results, ~ inherits(.x, "expectation_success"))),
                   failed = sum(map_lgl(.r$results, ~ inherits(.x,
                                                               c("expectation_failure",
                                                                 "expectation_error",
                                                                 "expectation_skip"))))
    ) %>%
      mutate(
        TestId = parse_test_id(.data$TestName),
        TestName = strip_test_id(.data$TestName, .data$TestId)
      )
  })

  # Roll up over TestId
  if (isTRUE(roll_up_ids)) {
    test_results <- roll_up_test_ids(test_results)
  }

  return(select(test_results, "TestName", "passed", "failed", "TestId"))
}


#' Parse Go test output
#'
#' Parses output of `go test --json` into a tibble for `mrgvalidate` to consume.
#'
#' @details
#'
#' * Any tests _without_ Test Id's (correctly formatted _in brackets_) will be thrown out.
#'
#' * All tests and subtests with the same Test Id will be rolled up to a single
#' row in the output tibble. The `passed,failed` counts will reflect the number
#' of tests/subtests that were rolled up this way. The test names for a set of
#' rolled up ID's must share a common prefix, which will be used as the new test
#' name. Any trailing spaces, slashes, colons, or underscores will be removed
#' from the extracted prefix.
#'
#' * Any test with _more than one Test Id_ will have only the first Test Id
#' extracted. Any subsequent Test Id's for that test will be ignored. Generally,
#' the first Id will be the _least_ specific, since subsequent Id's will likely
#' have been added as part of subtests. Keep this in mind when adding Testd Id's
#' in your Go test code.
#'
#' @param test_file Path to a `.json` file containing the output from `go test --json`
#' @param roll_up_ids If `TRUE`, the default, will roll up any duplicated
#'   (non-NA) ID's so that they are unique and the passed/failed count reflects
#'   the total sums for a tests with a given ID. If `FALSE`, will leave
#'   duplicated Test ID's as is. Note: leaving this `FALSE` may cause strange
#'   outputs for subtests.
#' @return A tibble formatted according to `mrgvalidate::input_formats`
#' @seealso `mrgvalidate::input_formats`, `mrgvalidate::create_validation_docs()`
#' @importFrom dplyr summarise group_by filter select rename mutate
#' @importFrom stringr str_detect
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_dfr
#' @importFrom readr read_lines
#' @export
parse_golang_test_json <- function(test_file, roll_up_ids = TRUE) {
  line_by_line <- read_lines(test_file)
  df <- map_dfr(line_by_line, fromJSON)

  test_results <- df %>%
    filter(!is.na(.data$Test)) %>%
    filter(str_detect(.data$Test, "\\/")) %>% # TODO: I think this throws out only the full test function summaries, but should double check
    filter(.data$Action %in% c("pass", "fail", "skip")) %>%
    rename(TestName = "Test") %>%
    mutate(
      TestId = parse_test_id(.data$TestName),
      TestName = strip_test_id(.data$TestName, .data$TestId),
      passed = ifelse(.data$Action == "pass", 1, 0),
      failed = ifelse(.data$Action != "pass", 1, 0)
    )

  no_id <- sum(is.na(test_results$TestId))
  if (no_id > 0) {
    rlang::warn(glue("Throwing out {no_id} tests with no Test Id's"))
    test_results <- filter(test_results, !is.na(.data$TestId))
  }

  # Roll up over TestId
  if (isTRUE(roll_up_ids)) {
    test_results <- roll_up_test_ids(test_results)
  }

  return(select(test_results, "TestName", "passed", "failed", "TestId"))
}



#' Extract test ID from a string. Always takes the _first_ ID found in the string.
#' @importFrom stringr str_match
#' @keywords internal
parse_test_id <- function(string) {
  str_match(string, "\\[([A-Z]+-[A-Z]+-[0-9]+)\\]")[, 2]
}


#' Return a string without the embedded test ID.
#'
#' Note that any white space in the string will be collapsed to a single
#' character.
#' @importFrom stringr regex str_replace str_squish
#' @keywords internal
strip_test_id <- function(string, id) {
  string %>%
    str_replace(fixed(paste0("[", id, "]")), "") %>%
    str_squish
}


#' Leading longest common substring
#'
#' Get the longest string contained in _each_ element of `x`, _starting from the
#' beginning_ of each element.
#' @importFrom stringr str_extract str_replace
#' @importFrom purrr map_chr
#' @param x a character vector
#' @keywords internal
leading_lcs <- function(x) {
  checkmate::assert_character(x)
  if (length(x) == 1) return(x)
  lcs <- ""
  a <- x[1]
  for(n in seq_len(nchar(a))) {
    sb <- substr(a, 1, n)

    matched <- map_chr(x[2:length(x)], function(b) {
      str_extract(b, paste0("^\\Q", sb, "\\E"))
    })

    if(any(is.na(matched))) {
      break
    } else {
      lcs <- sb
    }
  }

  if (lcs == "") rlang::abort(paste("No leading overlap in\n", paste(x, collapse = "\n ")))

  return (str_replace(lcs, "[:\\/_ ]*$", ""))
}

#' Roll up so test ID's are unique and passed/failed sums up counts
#' @importFrom dplyr filter group_by summarise bind_rows
#' @keywords internal
roll_up_test_ids <- function(test_df) {

  # don't roll up tests with no ID
  no_id_tests <- filter(test_df, is.na(.data$TestId))

  # roll up tests with ID's
  test_df <- test_df %>%
    filter(!is.na(.data$TestId)) %>%
    group_by(.data$TestId) %>%
      summarise(
        TestName = leading_lcs(.data$TestName),
        passed = sum(.data$passed, na.rm = TRUE),
        failed = sum(.data$failed, na.rm = TRUE)
      )

  test_df %>%
    bind_rows(no_id_tests) %>%
    select("TestName", "passed", "failed", "TestId")
}
