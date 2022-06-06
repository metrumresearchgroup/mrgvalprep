
test_that("Requirements from tests pipeline works", {
  # setup
  val_dir <- file.path(tempdir(), "mrgvalprep-reqs-from-tests")
  fs::dir_create(val_dir)
  on.exit(fs::dir_delete(val_dir))

  # load in fake test output and move stories YAML to temp dir
  test_res <- readr::read_csv(system.file("test-refs", "test-parse-test-output-ListReporter-parsed.csv", package = "mrgvalprep"), col_types = "ciic") %>%
    dplyr::mutate(TestId = stringr::str_replace(.data$TestId, "FOO-BAR", "TEST-ID")) %>%
    dplyr::slice_head(n = 3)
  req_path <- file.path(val_dir, "requirements.yaml")
  stories_path <- file.path(val_dir, "stories.yaml")
  fs::file_copy(
    system.file("yaml-input", "stories-only-1.yaml", package = "mrgvalprep"),
    stories_path
  )

  old_spec <- read_spec_yaml(stories_path)
  expect_true("TestIds" %in% names(old_spec))
  expect_false("RequirementId" %in% names(old_spec))

  # convert to requirements
  req_df <- req_df_from_tests(test_res)
  suppressMessages(req_df_to_yaml(req_df, req_path))
  suppressMessages(stories_replace_tests_with_reqs(stories_path, req_df))

  new_spec <- read_spec_yaml(stories_path, req_path)
  expect_true(all(
    "TestIds" %in% names(new_spec),
    "RequirementId" %in% names(new_spec)
  ))
  expect_true(all(
    c("S001", "S002") %in% new_spec$StoryId,
    c("ID-R001", "ID-R002", "ID-R003") %in% new_spec$RequirementId,
    c("TEST-ID-001", "TEST-ID-002", "TEST-ID-003") %in% unlist(new_spec$TestIds)
  ))
})



