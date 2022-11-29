#' Store test results in dedicated Git reference
#'
#' @description
#'
#' `test_and_push_results()` runs tests with [test_and_write_results()] and, if
#' there are no failures, stores the results in a Git reference and pushes that
#' to a remote.
#'
#' `ingest_and_push_results()`, on the other hand, updates a Git reference with
#' the results from the specified directory and pushes that to a remote.
#'
#' @param results_dir Directory with results to ingest.
#' @param commit Commit on which the tests were executed.
#' @param remote Push results to this remote.
#' @param ref Store the test results in this Git reference and push it to
#'   `remote`. This must be a fully qualified reference, and it should typically
#'   _not_ be underneath the standard "refs/heads/" namespace.
#' @param force If test results for this commit already exist, overwrite them
#'   rather than aborting. Note that the old results will still exist in the
#'   history of `ref`.
#' @inheritParams test_and_write_results
#' @name git_results
NULL

#' @rdname git_results
#' @export
test_and_push_results <- function(tests_dir,
                                  executor = NULL,
                                  info = NULL,
                                  remote = "origin",
                                  ref = "refs/mrgval/test-results",
                                  force = FALSE) {
  assert_valid_git_ref(ref)
  assert_clean_repo()

  commit <- git_resolve_id("HEAD", "commit")
  # Calling git_remote_ref_exists() here isn't necessary, but it avoids going
  # through tests only for ingest_and_push_results() to abort on an invalid
  # remote or ref.
  git_remote_ref_exists(remote, ref)

  message(glue("Running tests under {tests_dir}..."))
  results_dir <- withr::local_tempdir("mrgvalprep-")
  test_and_write_results(tests_dir,
                         results_dir,
                         executor = executor,
                         info = info)
  message(glue("Saving results to {ref} and pushing to {remote}..."))
  ingest_and_push_results(results_dir,
                          commit = commit,
                          remote = remote,
                          ref = ref,
                          force = force)
}

#' @rdname git_results
#' @export
ingest_and_push_results <- function(results_dir,
                                    commit = "HEAD",
                                    remote = "origin",
                                    ref = "refs/mrgval/test-results",
                                    force = FALSE) {
  assert_valid_git_ref(ref)

  tests_id <- git_resolve_id(commit, "tree")
  ref_info <- prepare_for_ingest(remote, ref, tests_id, force)
  commit <- ingest_results(results_dir, tests_id,
                           ref_info$tree, ref_info$commit)

  processx::run(
    "git", c("push", remote, paste0(commit, ":", ref)))
  # Push before updating the local ref to make sure the new commit is valid
  # (e.g., somebody else hasn't pushed to the ref after we fetched).
  processx::run(
    "git", c("update-ref", "-m", "commit", ref, commit, ref_info$commit))
  return(invisible(NULL))
}

prepare_for_ingest <- function(remote, ref, tests_id, force) {
  if (git_remote_ref_exists(remote, ref)) {
    git_fetch_ref(remote, ref)
  }

  base <- git_resolve_id(ref, "commit") %||% git_create_ref(ref)
  tree <- git_ls_tree(base)
  if (tests_id %in% tree$path) {
    if (isTRUE(force)) {
      tree <- dplyr::filter(tree, .data$path != tests_id)
    } else {
      abort(
        c(glue("Results ({tests_id}) already exists in {ref}"),
          "i" = "Use `force = TRUE` to continue anyway."))
    }
  }

  return(list(tree = tree, commit = base))
}

ingest_results <- function(results_dir, tests_id, ref_tree, ref_commit) {
  csvs <- fs::dir_ls(results_dir, type = "file", glob = "*.csv",
                     recurse = FALSE)
  if (length(csvs) == 0) {
    abort(paste("No *.csv files found in", results_dir))
  }

  jsons <- fs::path_ext_set(csvs, "json")
  missing_json <- !fs::file_exists(jsons)
  if (any(missing_json)) {
    abort(
      c("Some *.csv files are missing required *.json counterpart:",
        unname(csvs[missing_json])))
  }

  blob_df <- git_hash_files(c(csvs, jsons))
  tree_df <- tibble::tibble_row(mode = "040000",
                                type = "tree",
                                name = git_mktree(blob_df),
                                path = tests_id)
  result_tree <- git_mktree(dplyr::bind_rows(tree_df, ref_tree))
  git_string(c("commit-tree", "-m", "ingest results",
               "-p", ref_commit, result_tree))
}
