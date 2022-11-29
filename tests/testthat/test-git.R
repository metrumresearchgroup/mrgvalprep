
test_that("git_lines() returns character(0) for empty output", {
  withr::with_tempdir({
    processx::run("git", "init")
    expect_identical(git_lines("ls-files"), character(0))
  })
})
