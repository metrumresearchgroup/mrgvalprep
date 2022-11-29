
#' Run tests and write output files for mrgvalidate
#'
#' mrgvalidate requires test results to be written in a particular format (see
#' `mrgvalidate::input_formats`). The function runs the tests underneath a
#' directory, writing the results to a specified directory.
#'
#' To use this function, the tests must be a part of a package and be underneath
#' the current directory's Git repository.
#'
#' @param tests_dir Directory containing test files (e.g., "tests/testthat/").
#' @param results_dir Write the results to this directory. If this exists, it
#'   must be empty.
#' @param executor Name of person executing to test. If not specified, the value
#'   is taken from the `USER` environment variable.
#' @param info A named list with additional information that should be included
#'   in the JSON output file.
#' @seealso [parse_testthat_list_reporter()], [get_sys_info()]
#' @export
test_and_write_results <- function(tests_dir,
                                   results_dir,
                                   executor = NULL,
                                   info = NULL) {
  if (fs::dir_exists(results_dir)) {
    if (length(fs::dir_ls(results_dir, all = TRUE)) != 0) {
      abort(paste("Directory exists and is not empty:", results_dir))
    }
  } else if (fs::file_exists(results_dir)) {
    abort(paste("Path exists and is not a directory:", results_dir))
  }

  current_repo <- git_string(c("rev-parse", "--show-toplevel"))
  tests_repo <- git_string(c("rev-parse", "--show-toplevel"),
                           wd = tests_dir)

  if (!identical(current_repo, tests_repo)) {
    abort(c("`tests_dir` is not within current directory's Git repository",
            paste("current repository:", current_repo),
            paste("`tests_dir` repository:", tests_repo)))
  }

  assert_clean_repo()
  commit_id <- git_string(c("rev-parse", "HEAD"))

  # This is a stripped-down version of what's done by devtools::test().
  pkg <- devtools::as.package(tests_dir)
  withr::local_envvar(devtools::r_env_vars())
  res <- testthat::test_dir(tests_dir,
                            package = pkg$name,
                            reporter = testthat::ListReporter,
                            stop_on_failure = TRUE,
                            load_package = "source")

  fs::dir_create(results_dir)
  utils::write.csv(parse_testthat_list_reporter(res, roll_up_ids = TRUE),
                   file.path(results_dir, "tests.csv"),
                   row.names = FALSE)

  info[["Package commit"]] <- commit_id
  mvers <- Sys.getenv("METWORX_VERSION")
  if (nzchar(mvers)) {
    info[["Metworx version"]] <- mvers
  }

  jsonlite::write_json(
    list("date" = as.character(Sys.time()),
         "executor" = executor %||% Sys.getenv("USER"),
         "info" = info),
    file.path(results_dir, "tests.json"),
    pretty = TRUE, auto_unbox = TRUE)
}
