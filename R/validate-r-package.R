#' Validate a locally cloned R package
#'
#' Creates validation documents for an R package from a local clone of the package repo.
#' Note that **you must have at least `mrgvalidate 2.0.0 to use `mrgvalprep::validate_r_package()`.**
#' TODO: add more...
#'
#' @importFrom desc description
#' @importFrom checkmate assert_directory_exists
#' @importFrom withr local_dir
#' @importFrom fs path_real
#'
#' @param path Path to the root directory of the package repo. This directory
#'   will be expected to contain DESCRIPTION and NEWS.md, as well be the root
#'   directory of a git repository.
#' @param tests_dir Path (relative to `path`) to directory containing tests to be run.
#' @param output_dir Path (relative to `path`) to directory to write resulting
#'   documents into. Documents will all be written into a sub-directory
#'   "{package_name}-{version}-validation-docs" under `output_dir`. This will be
#'   over-written if it already exists.
#' @param ... arguments passed through to `mrgvalidate::create_package_docs()`
#' @export
validate_r_package <- function(
  path,
  tests_dir = file.path("tests", "testthat"),
  output_dir = file.path("inst", "validation"),
  ...
) {
  if (!require("mrgvalidate", quietly = TRUE)) stop("Must have `mrgvalidate` installed to use `mrgvalprep::validate_r_package()`")
  if (packageVersion("mrgvalidate") < 2.0) stop(paste("Must have at least `mrgvalidate 2.0.0 to use `mrgvalprep::validate_r_package()` but found version", packageVersion("mrgvalidate")))

  # check that package dir exists and build absolute path
  checkmate::assert_directory_exists(path)
  path <- fs::path_real(path)

  args <- extract_r_package_info(path)

  # run tests and write result to disk
  withr::local_envvar(NOT_CRAN = "true")
  test_res <- testthat::test_dir(
    file.path(path, tests_dir),
    package = args$product_name,
    reporter = testthat::ListReporter,
    load_package = "source"
  )

  test_df <- parse_testthat_list_reporter(
    test_res,
    roll_up_ids = TRUE
  )

  test_results_dir <- file.path(tempdir(), glue("{args$product_name}-validation-tests"))
  if (fs::dir_exists(test_results_dir)) fs::dir_delete(test_results_dir)
  fs::dir_create(test_results_dir)

  write.csv(
    test_df,
    file.path(test_results_dir, glue("{args$product_name}-tests.csv"))
  )

  git_hash <- system(glue("git -C {path} rev-parse HEAD"), intern=TRUE)
  Sys.setenv("COMMIT_HASH" = git_hash)

  env_vars <- "COMMIT_HASH"
  if (nzchar(Sys.getenv("METWORX_VERSION"))) env_vars <- c(env_vars, "METWORX_VERSION")
  # TODO: add a check for some drone version variable?

  mrgvalprep::get_sys_info(
    out_path = file.path(test_results_dir, glue("{args$product_name}-tests.json")),
    env_vars = env_vars
  )

  # create output directory and write docs to it
  out_dir <- file.path(output_dir, glue("{args$product_name}-{args$version}-validation-docs"))
  if (fs::dir_exists(out_dir)) fs::dir_delete(out_dir)
  fs::dir_create(out_dir)

  # TODO: some checks on the elipses to make sure they didn't pass something that we pass
  mrgvalidate::create_package_docs(
    product_name = args$product_name,
    version = args$version,
    language = args$language,
    repo_url = args$repo_url,
    specs = args$specs,
    release_notes_file = args$release_notes_file,
    output_dir = out_dir,
    auto_test_dir = test_results_dir,
    ...
  )
}

