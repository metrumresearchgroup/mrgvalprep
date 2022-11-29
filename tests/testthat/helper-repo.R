
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
