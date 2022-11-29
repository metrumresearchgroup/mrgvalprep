
local_git_repo <- function(clean = TRUE, .local_envir = parent.frame()) {
  repo <- withr::local_tempdir("mrgvalprep-test-repo-",
                               clean = clean,
                               .local_envir = .local_envir)
  processx::run("git", c("init", repo))
  processx::run("git", c("commit", "-m", "initial commit", "--allow-empty"),
                wd = repo)
  withr::local_dir(repo, .local_envir = .local_envir)

  return(repo)
}

local_cloned_repo <- function(clean = TRUE, .local_envir = parent.frame()) {
  tdir <- withr::local_tempdir("mrgvalprep-test-repo-",
                               clean = clean,
                               .local_envir = .local_envir)
  repo_a <- file.path(tdir, "repo-a")
  repo_b <- file.path(tdir, "repo-b")
  processx::run("git", c("init", "--bare", repo_a))
  processx::run("git", c("clone", "-o", "origin", repo_a, repo_b))

  processx::run("git", c("commit", "-m", "initial commit", "--allow-empty"),
                wd = repo_b)
  processx::run("git", c("push", "origin", "HEAD"),
                wd = repo_b)
  withr::local_dir(repo_b, .local_envir = .local_envir)

  return(repo_b)
}

add_test <- function(name = "foo", fail = FALSE) {
  fs::dir_create("tests")

  test_file <- file.path("tests", glue("test-{name}.R"))
  test_num <- if (file.exists(test_file)) {
    sum(grepl("^test_that", readr::read_lines(test_file))) + 1
  } else {
    1
  }
  readr::write_lines(
    c("",
      sprintf('test_that("%s %d [%s-TST-%03d]", {',
              name, test_num, toupper(name), test_num),
      if (isTRUE(fail)) "  fail()" else "  succeed()",
      "})"),
    test_file,
    append = TRUE)

  processx::run("git", c("add", test_file))
  processx::run(
    "git", c("commit", "-m", paste0(name, ": add test ", test_num)))
}

local_pkg_repo <- function(clean = TRUE, .local_envir = parent.frame()) {
  repo <- local_cloned_repo(clean = clean, .local_envir = .local_envir)
  readr::write_lines(
    c("Package: testpkg",
      "Version: 1.0.0"),
    "DESCRIPTION")
  cat("", file = "NAMESPACE")
  processx::run("git", c("add", c("DESCRIPTION", "NAMESPACE")))
  processx::run("git", c("commit", "-m", "set up package"))

  add_test()

  processx::run("git", c("push", "origin"))

  return(repo)
}
