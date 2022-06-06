################################################################
# Helper functions for incorporating requirements
# into a repo that previously only tracked stories and tests
################################################################

#' @name requirements_from_tests
#' @title Requirements from tests
#'
#' @description If you were previously linking stories directly to tests, and
#'   you would like to begin using requirements, use [req_df_from_tests()] and
#'   [req_df_to_yaml()] to create a requirements YAML file that parses
#'   requirement descriptions from test names. Then use
#'   [stories_replace_tests_with_reqs()] to edit your stories YAML to reference
#'   the newly created requirements.
#'
#' @details Note that these functions are designed to help you convert a repo
#' that previously did _not_ use requirements into one that does. They are not
#' meant for continued use after that. Going forward, requirements should be
#' specified in the requirements YAML file, instead of being pull through from
#' test names
#'
#' @examples
#' \dontrun{
#'
#' # run your test suite to get a tibble of test names and IDs
#' test_res <- mrgvalprep::parse_testthat_list_reporter(
#'   devtools::test(Reporter = testthat::ListReporter)
#' )
#'
#' # create requirements tibble and write to new YAML
#' req_df <- req_df_from_tests(test_res)
#' req_df_to_yaml(req_df, "inst/validation/requirements.yaml")
#'
#' # edit existing stories YAML
#' stories_replace_tests_with_reqs("inst/validation/stories.yaml", req_df)
#'
#'}
NULL

#' @describeIn requirements_from_tests Create new requirements tibble from test
#'   results tibble.
#' @importFrom dplyr mutate select
#' @param test_df Tibble of tests results, as returned from
#'   [parse_testthat_list_reporter()] or [parse_golang_test_json()]
#' @export
req_df_from_tests <- function(test_df) {
  test_df %>%
    mutate(
      req_id = .data$TestId %>%
        stringr::str_replace("[A-Za-z]+\\-", "") %>%
        stringr::str_replace("([0-9]+)$", "R\\1"),
      req_desc = .data$TestName,
      test_ids = .data$TestId
    ) %>%
    select(.data$req_id, .data$req_desc, .data$test_ids)
}

#' @describeIn requirements_from_tests Write requirements YAML file from
#'   requirements tibble.
#' @importFrom purrr pmap
#' @importFrom rlang set_names
#' @importFrom yaml write_yaml
#' @param req_df Tibble of requirements, as returned from [req_df_from_tests()].
#' @param out_file Path to write requirements YAML file to.
#' @export
req_df_to_yaml <- function(req_df, out_file) {
  req_df %>%
    pmap(function(...) {
      .row <- list(...)
      list(
        description = .row$req_desc,
        tests = as.list(.row$test_ids)
      )
    }) %>%
    set_names(req_df$req_id) %>%
    write_yaml(out_file)
  message(paste("Requirements YAML written to", out_file))
}

#' @describeIn requirements_from_tests Modify existing stories YAML to reference
#'   new requirements instead of tests.
#' @importFrom readr read_lines
#' @importFrom stringr str_replace_all
#' @param req_spec Tibble of requirements, as returned from [req_df_from_tests()].
#' @param stories_file Path to write stories YAML file that will be modified on
#'   disk.
#' @export
stories_replace_tests_with_reqs <- function(stories_file, req_spec) {
  stories_str <- read_lines(stories_file)

  stories_str <- str_replace_all(
    stories_str,
    "tests:",
    "requirements:"
  )

  for (i in 1:nrow(req_spec)) {
    stories_str <- str_replace_all(
      stories_str,
      paste0("\\b\\Q", req_spec$test_ids[i], "\\E\\b"),
      req_spec$req_id[i]
    )
  }

  readr::write_lines(stories_str, stories_file)
  message(paste("Edited Stories YAML file:", stories_file))
}
