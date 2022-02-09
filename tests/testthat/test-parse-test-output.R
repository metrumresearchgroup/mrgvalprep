
test_that("parse_testthat_list_reporter() returns expected tibble", {
  test_res <- readRDS(system.file("test-refs", "test-parse-test-output-ListReporter.RDS", package = "mrgvalprep"))
  test_ref <- readr::read_csv(system.file("test-refs", "test-parse-test-output-ListReporter-parsed.csv", package = "mrgvalprep"), col_types = "ciic")
  parsed_df <- parse_testthat_list_reporter(test_res)
  expect_equal(
    # using as.data.frame to blow away small differences in classes and formatting that are not relevant
    as.data.frame(parsed_df),
    as.data.frame(test_ref)
  )
})

test_that("parse_testthat_list_reporter() optionally rolls up ID's", {
  test_res <- readRDS(system.file("test-refs", "test-parse-test-output-ListReporter.RDS", package = "mrgvalprep"))

  # modify to have duplicates
  new_idx <- length(test_res)+1
  test_res[[new_idx]] <- test_res[[1]]

  # add some that are missing ID's
  no_id <- list(
    results = list(structure(list(test = "this test has no ID"), class = "expectation_success"))
  )
  test_res[[new_idx+1]] <- no_id
  test_res[[new_idx+2]] <- no_id

  parsed_df <- parse_testthat_list_reporter(test_res, roll_up_ids = TRUE)
  expect_equal(sum(is.na(parsed_df$TestId)), 2)
  expect_false(any(duplicated(parsed_df$TestId[!is.na(parsed_df$TestId)])))

  # check that the counts are rolled up
  id_to_check <- parse_test_id(test_res[[1]]$test)
  test_ref <- readr::read_csv(system.file("test-refs", "test-parse-test-output-ListReporter-parsed.csv", package = "mrgvalprep"), col_types = "ciic")
  expect_equal(
    parsed_df %>% filter(TestId == id_to_check) %>% dplyr::pull(passed),
    2 * (test_ref %>% filter(TestId == id_to_check) %>% dplyr::pull(passed))
  )
})

test_that("trailing colon is stripped when rolling up IDs", {
  df <- tibble::tribble(
    ~TestName, ~passed, ~failed, ~TestId,
    "t1", 2, 0, "FOO-BAR-001",
    "t2", 1, 0, "FOO-BAR-002",
    "t3: first case", 1, 0, "FOO-BAR-003",
    "t3: second case", 3, 0, "FOO-BAR-003")

  expect_equal(
    roll_up_test_ids(df),
    tibble::tribble(
      ~TestName, ~passed, ~failed, ~TestId,
    "t1", 2, 0, "FOO-BAR-001",
    "t2", 1, 0, "FOO-BAR-002",
    "t3", 4, 0, "FOO-BAR-003"))
})

test_that("parse_golang_test_json() happy path", {
  test_res_file <- system.file("test-refs", "test-parse-test-output-go-test-1.json", package = "mrgvalprep")
  test_ref_file <- system.file("test-refs", "test-parse-test-output-go-test-1-parsed.csv", package = "mrgvalprep")

  parsed_df <- parse_golang_test_json(test_res_file)
  test_ref <- readr::read_csv(test_ref_file, col_types = "ciic")

  expect_equal(
    # using as.data.frame to blow away small differences in classes and formatting that are not relevant
    as.data.frame(parsed_df),
    as.data.frame(test_ref)
  )
})

test_that("parse_golang_test_json() missing test id", {
  test_res_file <- system.file("test-refs", "test-parse-test-output-go-test-brackets-missing.json", package = "mrgvalprep")
  test_ref_file <- system.file("test-refs", "test-parse-test-output-go-test-brackets-missing.csv", package = "mrgvalprep")

  expect_warning({
    parsed_df <- parse_golang_test_json(test_res_file)
  }, regexp = "Throwing out.+no Test Id")
  test_ref <- readr::read_csv(test_ref_file, col_types = "ciic")

  expect_equal(
    # using as.data.frame to blow away small differences in classes and formatting that are not relevant
    as.data.frame(parsed_df),
    as.data.frame(test_ref)
  )
})

test_that("parse_golang_test_json() two test ids for single test", {
  test_res_file <- system.file("test-refs", "test-parse-test-output-go-test-two-ids.json", package = "mrgvalprep")
  test_ref_file <- system.file("test-refs", "test-parse-test-output-go-test-two-ids.csv", package = "mrgvalprep")

  parsed_df <- parse_golang_test_json(test_res_file)
  test_ref <- readr::read_csv(test_ref_file, col_types = "ciic")

  expect_equal(
    # using as.data.frame to blow away small differences in classes and formatting that are not relevant
    as.data.frame(parsed_df),
    as.data.frame(test_ref)
  )
})

test_that("parse_test_id() returns NA if there is no ID", {
  cases <- list(
    # input expected
    c("", NA),
    c("foo", NA),
    c("FOO-BAR-001", NA),
    c("[FOO-001]", NA)
  )
  for (case in cases) {
    expect_equal(parse_test_id(case[[1]]), case[[2]])
  }
})

test_that("parse_test_id() can parse IDs", {
  cases <- list(
    # input expected
    c("[FOO-BAR-001]", "FOO-BAR-001"),
    c("[F-B-0]", "F-B-0"),
    c("a [FOO-BAR-001] b", "FOO-BAR-001"),
    c("[FOO-BAR-001] b", "FOO-BAR-001"),
    c("a [FOO-BAR-001]", "FOO-BAR-001"),
    c("a[FOO-BAR-001]b", "FOO-BAR-001")
  )
  for (case in cases) {
    expect_equal(parse_test_id(case[[1]]), case[[2]])
  }
})

test_that("parse_test_id() extracts only the first match", {
  expect_equal(parse_test_id("[FOO-BAR-001] [FOO-BAR-002] "),
               "FOO-BAR-001")
})

test_that("strip_test_id() returns string without a bracketed test ID as is", {
  cases <- list(
    # input expected
    c("", ""),
    c("foo", "foo"),
    c("FOO-BAR-001", "FOO-BAR-001"),
    c("[FOO-001]", "[FOO-001]")
  )
  for (case in cases) {
    expect_equal(strip_test_id(case[[1]], "FOO-BAR-001"),
                 case[[2]])
  }
})

test_that("strip_test_id() strips only specified test ID", {
  expect_equal(strip_test_id("[DIFF-ID-002]",
                             "FOO-BAR-001"),
               "[DIFF-ID-002]")
  expect_equal(strip_test_id("[DIFF-ID-002][FOO-BAR-001]",
                             "FOO-BAR-001"),
               "[DIFF-ID-002]")
})

test_that("strip_test_id() strips only the first match", {
  expect_equal(strip_test_id("a[FOO-BAR-001]b[FOO-BAR-001]",
                             "FOO-BAR-001"),
               "ab[FOO-BAR-001]")
})

test_that("strip_test_id() collapses spaces", {
  cases <- list(
    # input expected
    c("[FOO-001]", "[FOO-001]"),
    c("[DIFF-ID-002]", "[DIFF-ID-002]"),
    c("a [FOO-BAR-001] b", "a b"),
    c("a  [FOO-BAR-001] b", "a b"),
    c("[FOO-BAR-001] b", "b"),
    c("a [FOO-BAR-001]", "a"),
    c("a[FOO-BAR-001]b", "ab"),
    c("  [FOO-BAR-001] desc", "desc")
  )
  for (case in cases) {
    expect_equal(strip_test_id(case[[1]], "FOO-BAR-001"),
                 case[[2]])
  }
})
