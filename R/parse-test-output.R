#' Parse testthat output
#'
#' @param result List output as reported by [testthat::ListReporter]
#' @return A tibble formatted according to `mrgvalidate::input_formats`
#' @seealso `mrgvalidate::input_formats`, `mrgvalidate::create_validation_docs()`
#' @importFrom purrr map_chr map_lgl map_dfr
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace fixed
#' @importFrom rlang .data
#' @export
parse_testthat_list_reporter <- function(result) {
  map_dfr(result, function(.r) {
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
#' of tests/subtests that were rolled up this way.
#'
#' * Any test with _more than one Test Id_ will have only the first Test Id
#' extracted. Any subsequent Test Id's for that test will be ignored. Generally,
#' the first Id will be the _least_ specific, since subsequent Id's will likely
#' have been added as part of subtests. Keep this in mind when adding Testd Id's
#' in your Go test code.
#'
#' @param test_file Path to a file `.json` file containing the output from `go test --json`
#' @return A tibble formatted according to `mrgvalidate::input_formats`
#' @seealso `mrgvalidate::input_formats`, `mrgvalidate::create_validation_docs()`
#' @importFrom dplyr summarise group_by filter select rename mutate
#' @importFrom stringr str_detect
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_dfr
#' @importFrom readr read_lines
#' @importFrom rlang .data
#' @export
parse_golang_test_json <- function(test_file) {
  line_by_line <- read_lines(test_file)
  df <- map_dfr(line_by_line, fromJSON)

  test_results <- df %>%
    filter(!is.na(Test)) %>%
    filter(str_detect(Test, "\\/")) %>% # TODO: I think this throws out only the full test function summaries, but should double check
    filter(Action %in% c("pass", "fail", "skip")) %>%
    rename(TestName = Test) %>%
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

  # TODO: make this optional with an arg
  test_results <- test_results %>%
    group_by(.data$TestId) %>%
    summarise(
      TestName = leading_lcs(.data$TestName),
      passed = sum(.data$passed, na.rm = TRUE),
      failed = sum(.data$failed, na.rm = TRUE)
    )

  return(select(test_results, TestName, passed, failed, TestId))
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
      str_extract(b, paste0("^", sb))
    })

    if(any(is.na(matched))) {
      break
    } else {
      lcs <- sb
    }
  }

  if (lcs == "") rlang::abort(paste("No leading overlap in\n", paste(x, collapse = "\n ")))

  return (str_replace(lcs, "[\\/_ ]*$", ""))
}
