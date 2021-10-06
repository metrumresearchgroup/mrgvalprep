context("Unit tests for validate-tests.R")

test_that("validate_tests returns expected df", {
  skip_if_no_github_pat()

  # set up clean test dirs
  pkg_dir <- file.path(tempdir(), "validate_tests_pkg")
  if (fs::dir_exists(pkg_dir)) fs::dir_delete(pkg_dir)
  fs::dir_create(pkg_dir)
  on.exit({ fs::dir_delete(pkg_dir) })

  output_dir <- file.path(tempdir(), "validate_tests_output")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  test_df <- validate_tests(
    org = ORG,
    repo = REPO,
    version = TAG,
    domain = VALID_DOMAINS,
    root_dir = pkg_dir,
    output_dir = output_dir
  )

  expect_equal(nrow(test_df), TEST_DF_ROWS)
  expect_equal(ncol(test_df), TEST_DF_COLS)
  expect_equal(sum(test_df$failed), 0)
})


test_that("validate_tests writes output files", {
  skip_if_no_github_pat()

  # set up clean test dirs
  pkg_dir <- file.path(tempdir(), "validate_tests_pkg")
  if (fs::dir_exists(pkg_dir)) fs::dir_delete(pkg_dir)
  fs::dir_create(pkg_dir)
  on.exit({ fs::dir_delete(pkg_dir) })

  output_dir <- file.path(tempdir(), "validate_tests_output")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  out_file <- "validate_tests_res"

  validate_tests(
    org = ORG,
    repo = REPO,
    version = TAG,
    domain = VALID_DOMAINS,
    out_file = out_file,
    root_dir = pkg_dir,
    output_dir = output_dir,
    set_id_to_name = TRUE
  )

  test_df <- readr::read_csv(file.path(output_dir, paste0(out_file, ".csv")), col_types = "ciic")
  expect_equal(nrow(test_df), TEST_DF_ROWS)
  expect_equal(ncol(test_df), TEST_DF_COLS)
  expect_equal(sum(test_df$failed), 0)
  # set_id_to_name was honored.
  expect_equal(test_df$TestName, test_df$TestId)

  test_info <- jsonlite::fromJSON(file.path(output_dir, paste0(out_file, ".json")))
  expect_equal(names(test_info), c("date", "executor", "info"))
  expect_equal(test_info$info$env_vars$commit, get_commit_hash(pkg_dir, REPO))
})


test_that("validate_tests returns expected df with extra tests", {
  skip_if_no_github_pat()

  # set up clean test dirs
  pkg_dir <- file.path(tempdir(), "validate_tests_pkg")
  if (fs::dir_exists(pkg_dir)) fs::dir_delete(pkg_dir)
  fs::dir_create(pkg_dir)
  on.exit({ fs::dir_delete(pkg_dir) })

  output_dir <- file.path(tempdir(), "validate_tests_output")
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
  fs::dir_create(output_dir)
  on.exit({ fs::dir_delete(output_dir) })

  test_df <- validate_tests(
    org = ORG,
    repo = REPO,
    version = TAG,
    domain = VALID_DOMAINS,
    root_dir = pkg_dir,
    output_dir = output_dir,
    extra_test_dirs = EXTRA_TESTS
  )

  expect_equal(nrow(test_df), TEST_DF_ROWS_EXTRA_TESTS)
  expect_equal(ncol(test_df), TEST_DF_COLS)
  expect_equal(sum(test_df$failed), 0)
})
