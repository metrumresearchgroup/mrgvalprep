
#' Invoke git and return standard output
#'
#' @param args Arguments passed as `args` to [processx::run()].
#' @param ... Additional arguments to pass through to [processx::run()].
#' @noRd
git_string <- function(args, ...) {
  p <- processx::run(command = "git", args = args, ...)
  stringr::str_trim(p$stdout)
}

#' Invoke git and return newline-split standard output
#'
#' @param args Arguments passed as `args` to [processx::run()].
#' @param ... Additional arguments to pass through to [processx::run()].
#' @noRd
git_lines <- function(args, ...) {
  stdout <- git_string(args, ...)
  lines <- purrr::flatten_chr(stringr::str_split(stdout, "\n"))
  if (length(lines) == 1 && !nzchar(lines)) {
    lines <- character(0)
  }
  return(lines)
}

assert_valid_git_id <- function(id, label = NULL) {
  label <- label %||% deparse(substitute(id))
  if (!checkmate::test_string(id, pattern = "^[a-f0-9]+$", min.chars = 40)) {
    abort(glue("Invalid Git object ID ({id}) for {label}"),
          call = rlang::caller_env())
  }
}

assert_valid_git_ref <- function(ref) {
  if (!checkmate::test_string(ref, pattern = "^refs/[^/]+(:?/[^/]+)+$")) {
    abort(paste("Expected full Git reference ('refs/{namespace}/...'), got",
                ref),
          call = rlang::caller_env())
  }
}

assert_clean_repo <- function() {
  if (nzchar(git_string(c("status", "--porcelain", "-unormal")))) {
    repo <- git_string(c("rev-parse", "--show-toplevel"))
    abort(c(paste("Repository", repo, "is dirty"),
            "i" = "Commit or discard changes first.") ,
          call = rlang::caller_env())
  }
}

git_remote_ref_exists <- function(remote, ref) {
  p <- processx::run(
    "git", c("ls-remote", "--exit-code", "-q", remote, ref),
    error_on_status = FALSE)
  if (p$status == 0) {
    TRUE
  } else if (p$status == 2) {
    FALSE
  } else {
    abort(paste0("git error:\n", p$stderr))
  }
}

git_fetch_ref <- function(remote, ref) {
  processx::run("git", c("fetch", "-q", remote, paste0(ref, ":", ref)))
}

git_create_ref <- function(ref) {
  empty_tree <- git_string("mktree")
  commit <- git_string(c("commit-tree", "-m", "initial commit", empty_tree))
  processx::run("git", c("update-ref", "-m", "initial", ref, commit, ""))
  return(commit)
}
