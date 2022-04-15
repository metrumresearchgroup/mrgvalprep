context("Fake Test - repeated text")

test_that("Fake test with same text repeated", {
  str <- "Fake test with same text repeated"
  expect_true(is.character(str))
})
