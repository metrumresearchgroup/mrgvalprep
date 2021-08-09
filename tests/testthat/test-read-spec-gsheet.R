test_that("read_spec_gsheet() works correctly", {
  # It feels like we should test this, but I haven't looked into
  # programmatically supplying google creds
  # the Github tests expect you to have your creds in an environment variable
  # so we might take that approach
  expect_true(1 == 2, label = "test-read-spec-gsheet.R implementation")
})
