
test_that("test_and_push_results() aborts remote doesn't exist", {
  local_git_repo()

  expect_error(test_and_push_results("tests"),
               "remote")
})

test_that("ingestion fails if missing files", {
  local_cloned_repo()

  fs::dir_create("results")
  expect_error(ingest_and_push_results("results"),
               "No *.csv files",
               fixed = TRUE)

  cat("", file = file.path("results", "test.csv"))
  expect_error(ingest_and_push_results("results"),
               "missing required *.json counterpart",
               fixed = TRUE)
})

test_that("test_and_push_results() works", {
  local_pkg_repo()
  ref <- "refs/mrgval/test-results"

  tree1 <- git_resolve_id("HEAD", "tree")
  test_and_push_results("tests")

  add_test()

  tree2 <- git_resolve_id("HEAD", "tree")
  test_and_push_results("tests")
  ref_commits_1 <- git_lines(c("rev-list", ref))
  # 3 commits: the initial empty one and then one for each
  # test_and_push_results() call.
  expect_length(ref_commits_1, 3)
  result_tree <- git_ls_tree(ref)
  expect_setequal(result_tree$path, c(tree1, tree2))

  # 4 files: test.csv/test.json for each test_and_push_results() call.
  expect_identical(nrow(git_ls_tree(ref, recurse = TRUE)),
                   4L)

  expect_error(test_and_push_results("tests"), "already exists")
  test_and_push_results("tests", force = TRUE)
  ref_commits_2 <- git_lines(c("rev-list", ref))
  # `force=TRUE` overwrites tree2's result, but the reference update is in a
  # fast-forward manner (i.e. we don't clobber history).
  expect_length(setdiff(ref_commits_2, ref_commits_1), 1)

  tree2_csv <- git_string(c("cat-file", "blob",
                            paste0(ref, ":", tree2, "/tests.csv")))
  results <- readr::read_csv(I(tree2_csv))
  tree2_info <- git_string(c("cat-file", "blob",
                             paste0(ref, ":", tree2, "/tests.json")))
  info <- jsonlite::parse_json(tree2_info)

  expect_setequal(results$TestId, c("FOO-TST-001", "FOO-TST-002"))
  expect_identical(info$info[["Package commit"]],
                   git_string(c("rev-parse", "HEAD")))

  expect_match(git_string(c("ls-remote", "origin", ref)),
               paste0("^", git_resolve_id(ref, "commit")))

  origin <- git_string(c("remote", "get-url", "origin"))
  fresh_clone <- withr::local_tempdir("mrgvalprep-tests-")
  processx::run("git", c("clone", origin, fresh_clone))
  withr::with_dir(fresh_clone, {
    # Even in a new clone that is on an outdated commit, an existing test result
    # is flagged.
    expect_error(test_and_push_results("tests"), "already exists")
    # And if a commit is added on top of that outdated base and new results are
    # pushed...
    add_test(name = "bar")
    test_and_push_results("tests")
    # ... the other results from the unseen commits are still retained (because
    # the reference is never updated destructively).
    expect_length(setdiff(git_lines(c("rev-list", ref)),
                          ref_commits_2),
                  1)
  })
})
