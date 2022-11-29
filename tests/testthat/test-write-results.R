
test_that("test_and_write_results() aborts on test failure", {
  local_pkg_repo()

  add_test(fail = TRUE)
  expect_error(test_and_write_results("tests", "results"),
               "Test failures")
  expect_false(fs::dir_exists("results"))
})

test_that("test_and_write_results(): tests must belong to package directory", {
  local_git_repo()

  expect_error(test_and_write_results(".", "results"),
               "not find package root")
})

test_that("test_and_write_results(): tests must belong to current repo", {
  local_pkg_repo()

  test_path <- file.path(getwd(), "tests")

  local_git_repo()
  expect_error(test_and_write_results(test_path, "results"),
               "not within current")
})

test_that("test_and_write_results() requires clean repo state", {
  local_pkg_repo()

  cat("", file = "dirt")
  expect_error(test_and_write_results("tests", "results"),
               "dirty")
})

test_that("test_and_write_results() aborts on bad results directory", {
  local_pkg_repo()

  cat("", file = "results")
  expect_error(test_and_write_results("tests", "results"),
               "not a directory")

  unlink("results")
  fs::dir_create("results")
  cat("", file = file.path("results", "foo"))
  expect_error(test_and_write_results("tests", "results"),
               "not empty")
})

test_that("test_and_write_results() saves test results", {
  local_pkg_repo()

  test_and_write_results("tests", "results")
  results <- readr::read_csv(file.path("results", "tests.csv"))
  info <- jsonlite::read_json(file.path("results", "tests.json"))
  expect_identical(results$TestId, "FOO-TST-001")
  expect_identical(info$info[["Package commit"]],
                   git_string(c("rev-parse", "HEAD")))
})
