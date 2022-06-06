#' Run and summarize tests
#'
#' Will clone `repo` at `version` tag and run test suite. Package will be built
#' and installed (along with any missing dependencies) to a temporary library.
#' Then we run all tests for `repo`, roll up successes and failures, and write
#' to a CSV file. Also writes the commit hash of the package and some other
#' system info to a JSON file.
#'
#' @details
#'
#' The package found at `file.path(root_dir, repo)` will be built and installed
#' into a tempory library that is appended to the _beginning_ of your
#' `.libPaths()`, using `devtools::install(..., build = TRUE, upgrade =
#' "never")`. This will also install (into the temporary library) any
#' dependencies from the package that are _not_ found in your `.libPaths()`.
#'
#' @importFrom dplyr group_by summarize bind_rows
#' @importFrom purrr map_df map map_dfr
#' @importFrom rlang .data
#' @importFrom fs dir_exists dir_create
#'
#' @return Invisibly returns tibble of formatted test output. Note that this
#' function is primarily called for the side effect of writing out the
#' output to disk.
#'
#' @param org Github organization that the repo is under
#' @param repo The name of the repo for the package you are validating
#' @param version The version number of the package you are validating. This must correspond to a tag in the repo.
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"
#' @param root_dir The directory path to where the package has been cloned.
#'   `file.path(root_dir, repo)` should lead to the cloned repo that will be
#'   tested.
#' @param out_file File path to write out the test results to. Any extension
#'   will be ignored and replaced with `.csv` for the test output df and
#'   `.json` for the `get_sys_info()` output.
#' @param output_dir Directory to write the test outputs to. Defaults to
#'   working directory.
#' @param extra_test_dirs Character vector of paths (relative to package root
#'   dir) to directories that contain additional tests to run
#' @param set_id_to_name Whether to copy the `TestName` column over to the
#'   `TestId` column when writing the output CSV. This exists to support legacy
#'   issues and tests that don't use IDs. Starting with v1.0, mrgvalidate relies
#'   on test IDs and, as a temporary compatibility kludge, will auto-generate
#'   them if the `TestId` and `TestName` columns match. New tests and issues
#'   should use test IDs.
#' @export
validate_tests <- function(
  org,
  repo,
  version,
  domain = VALID_DOMAINS,
  out_file = NULL,
  root_dir = tempdir(),
  output_dir = getwd(),
  extra_test_dirs = NULL,
  set_id_to_name = FALSE

) {

  message(glue("Pulling repo {domain}/{org}/{repo}..."))
  commit_hash <- pull_tagged_repo(org = org, repo = repo, tag = version, dest_dir = root_dir, domain = domain)

  # install cloned package to temp dir
  tmp_lib <- withr::local_tempdir()
  withr::local_libpaths(tmp_lib, "prefix")
  # do this in a fresh session so it doesn't load all of package deps into this session
  callr::r(function(root_dir, repo) {
    devtools::install(
      file.path(root_dir, repo),
      build = TRUE,
      quiet = TRUE,
      upgrade = "never"
    )
  }, args = list(root_dir=root_dir, repo=repo))

  # run tests and parse output
  test_list <- run_tests(pkg = repo, root_dir = root_dir)
  test_df <- parse_testthat_list_reporter(test_list)

  if (!is.null(extra_test_dirs)) {
    extra_df <- map_dfr(extra_test_dirs, function(.t) {
      .tl <- run_tests(pkg = repo, test_path = .t, root_dir = root_dir)
      return(parse_testthat_list_reporter(.tl))
    })

    test_df <- bind_rows(test_df, extra_df)
  }

  if (isTRUE(set_id_to_name)) {
    test_df$TestId <- test_df$TestName
  }

  if (sum(test_df$failed) > 0) {
    warning(glue("`validate_tests(pkg = '{repo}', root_dir = '{root_dir}')` had {sum(test_df$failed)} failing tests."))
  }

  if (!is.null(out_file)) {
    if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
    out_csv <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".csv"))
    readr::write_csv(test_df, out_csv)

    out_json <- file.path(output_dir, paste0(tools::file_path_sans_ext(out_file), ".json"))
    withr::with_envvar(list(commit = commit_hash), {
      get_sys_info(
        out_path = out_json,
        env_vars = "commit"
      )
    })

  }

  return(invisible(test_df))
}

#' Clones the specified repo at the specified tag to disk
#' @importFrom glue glue
#' @importFrom processx run
#' @param org Github organization that the repo is under
#' @param repo The name of the repo for the package you are validating
#' @param tag The tag to pull from the repo. When this function is called internally, this is assumed to be the same as the version you are testing, though it can be any valid tag.
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"
#' @param dest_dir File path for directory to clone repo into. Defaults to [tempdir()]
#' @param overwrite Boolean indicating whether to overwrite `file.path(dest_dir, repo)` if something already exists there. TRUE by default.
#' @keywords internal
pull_tagged_repo <- function(
  org,
  repo,
  tag,
  domain = VALID_DOMAINS,
  dest_dir = tempdir(),
  overwrite = TRUE
) {
  if (isTRUE(overwrite)) {
    if (fs::dir_exists(file.path(dest_dir, repo))) fs::dir_delete(file.path(dest_dir, repo))
  }

  domain <- match.arg(domain)

  clone_string <- as.character(glue("git@{domain}:{org}/{repo}.git"))
  cmd_args <- c("clone", "--branch", tag, clone_string, "--depth=1")

  message(glue("Getting repo with `git {paste(cmd_args, collapse = ' ')}`"))
  proc <- processx::run(
    command = "git",
    args = cmd_args,
    wd = dest_dir
  )

  if (proc$status != "0") {
    stop(glue("Failed to clone {repo}\nCALL:\n  git {paste(cmd_args, collapse = ' ')}\nERROR:\n  {paste(proc$stderr, collapse = '  \n')}\n"))
  }

  # extract commit hash to return
  commit_hash <- get_commit_hash(dest_dir, repo)
  return(commit_hash)
}


#' Test a source package
#'
#' This is an internal function called by [validate_tests()]
#'
#' @param pkg Name of the package to test; should be installed to a library in
#'   [.libPaths()].
#' @param test_path Directory containing tests, where [testthat::test_dir()]
#'   will be run.
#' @param root_dir The directory path to where the package is (i.e. where the
#'   repo has been cloned). `file.path(root_dir, pkg, test_path)` should lead to
#'   the directory that will be tested.
#' @keywords internal
run_tests <- function(pkg, test_path = "tests/testthat", root_dir = tempdir()) {
  message(glue("run_tests() on {root_dir}/{pkg}/{test_path}"))

  results_list <- callr::r(
    function(root_dir, pkg, test_path, setup_package_env) {
      withr::local_dir(file.path(root_dir, pkg))
      stopifnot(require(pkg, character.only = TRUE))

      # load package environment
      env <- setup_package_env(pkg, test_path)

      # run tests
      results_list <- testthat::test_dir(
        path = test_path,
        reporter = testthat::ListReporter$new(),
        env = env,
        filter = NULL,
        stop_on_failure = FALSE,
        stop_on_warning = FALSE
      )

      return(results_list)
    },
    args = list(
      root_dir = root_dir,
      pkg = pkg,
      test_path = test_path,
      setup_package_env = setup_package_env
    )
  )
  return(results_list)
}

#' Helper for setting up package environment for testing
#'
#' These are all things that were copied out of internal functions called by testthat::test_check to set up the package environment.
#' They are necessary because testthat::test_dir does NOT do this, which causes some tests to fail.
#' Specifically, this code came mostly from testthat:::test_package_dir and testthat:::test_pkg_env and some from test_check itself.
#' @param package the package name
#' @param test_path path to folder with tests in it
#' @keywords internal
setup_package_env <- function(package, test_path) {

  if (!utils::file_test("-d", test_path)) {
    stop("No tests found for ", package, call. = FALSE)
  }

  env <-   list2env(
    as.list(getNamespace(package), all.names = TRUE),
    parent = parent.env(getNamespace(package))
  )

  withr::local_options(list(topLevelEnvironment = env))

  withr::local_envvar(list(
    TESTTHAT_PKG = package,
    TESTTHAT_DIR = fs::path_abs(".") # we're in the root dir because of withr::with_dir above
  ))

  return(env)
}

#' @importFrom glue trim
get_commit_hash <- function(root_dir, repo) {
  proc <- processx::run(command = "git", args = c("rev-parse", "HEAD"), wd = file.path(root_dir, repo))
  if (proc$status != "0") {
    stop(glue("Failed to get commit hash from {file.path(root_dir, repo)}\nCALL:\n  git {paste(cmd_args, collapse = ' ')}\nERROR:\n  {paste(proc$stderr, collapse = '  \n')}\n"))
  }
  return(trim(proc$stdout))
}
