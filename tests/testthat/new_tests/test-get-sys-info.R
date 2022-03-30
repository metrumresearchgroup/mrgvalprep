test_that("get_sys_info() returns date and default values [TST-FOO-003]", {
  res <- get_sys_info()
  expect_true(stringr::str_detect(res$date, "[0-9]+\\-[0-9]+\\-[0-9]+"))
  expect_true(stringr::str_detect(res$date, "[0-9]+\\-[0-9]+\\-[0-9]+"))
  expect_equal(res$executor, Sys.getenv("USER"))
  expect_equal(res$info, list())
})

test_that("get_sys_info() returns system info [TST-FOO-004]", {
  res <- get_sys_info(sys_info = TRUE)
  expect_equal(names(res$info$sys), c("sysname", "version", "release", "machine"))
})

test_that("get_sys_info() writes to json [TST-FOO-005]", {
  tmp_file <- tempfile()
  res <- get_sys_info(out_path = tmp_file)
  withr::defer(unlink(tmp_file))

  expect_true(fs::file_exists(tmp_file))

  written_res <- jsonlite::fromJSON(tmp_file)
  expect_equal(names(written_res), c("date", "executor", "info"))
  expect_true(stringr::str_detect(written_res$date, "[0-9]+\\-[0-9]+\\-[0-9]+"))
  expect_true(stringr::str_detect(written_res$executor, Sys.getenv("USER")))
})

test_that("get_sys_info() captures environment variables [TST-FOO-006]", {
  res <- get_sys_info(env_vars = c("USER", "FAKE_VAR"))

  expect_true(length(res$info$env_vars$USER) > 0)
  expect_equal(res$info$env_vars$FAKE_VAR, "")
})

test_that("get_sys_info() captures sessionInfo [TST-FOO-007]", {
  res <- get_sys_info(session = TRUE)

  expect_true(any(stringr::str_detect(res$info$session, "R version")))
  expect_true(any(stringr::str_detect(res$info$session, "testthat")))
})
