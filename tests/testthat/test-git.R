
test_that("git_lines() returns character(0) for empty output", {
  withr::with_tempdir({
    processx::run("git", "init")
    expect_identical(git_lines("ls-files"), character(0))
  })
})

test_that("assert_valid_git_ref() aborts on invalid refs", {
  cases <- c("",
             "foo",
             "refs/foo",
             "refs/foo/",
             "refs/foo/bar/",
             "refs/foo/bar//baz")
  for (case in cases) {
    expect_error(assert_valid_git_ref(case),
                 "Git reference")
  }
})

test_that("assert_valid_git_ref() succeeds on valid refs", {
  cases <- c("refs/x/y",
             "refs/foo/bar",
             "refs/foo/bar/baz")
  for (case in cases) {
    expect_identical(assert_valid_git_ref(case), NULL)
  }
})

test_that("assert_valid_git_id() aborts on invalid IDs", {
  cases <- c("",
             "xyz",
             "ac12q",
             " ab12",
             "ab12 ")
  for (case in cases) {
    expect_error(assert_valid_git_id(case),
                 "Invalid .* case")
  }

  # Custom label can be applied.
  expect_error(assert_valid_git_id(case, "something"),
               "Invalid .* something")
})

test_that("assert_clean_repo() aborts on dirty repo", {
  local_git_repo()

  assert_clean_repo()
  cat("", file = "foo")
  expect_error(assert_clean_repo(), "dirty")

  # Still detects untracked files despite user configuration.
  processx::run("git", c("config", "status.showUntrackedFiles", "no"))
  expect_error(assert_clean_repo(), "dirty")

  processx::run("git", c("add", "foo"))
  processx::run("git", c("commit", "-m", "foo"))
  assert_clean_repo()

  cat("change", file = "foo")
  expect_error(assert_clean_repo(), "dirty")
})
