context("Fake Test - repeated text")

test_that("Fake test with same text repeated [MRGVAL-TEST-0098]", {
  str <- "Fake test with same text repeated"
  expect_true(is.character(str))
})
