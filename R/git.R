
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

git_resolve_id <- function(obj, type = c("commit", "tree")) {
  type <- match.arg(type)
  p <- processx::run(
      "git",
      c("rev-parse", "--verify", "-q", paste0(obj, "^{", type, "}")),
      error_on_status = FALSE)

  id <- NULL
  if (p$status == 0) {
    id <- stringr::str_trim(p$stdout)
    assert_valid_git_id(id, paste0(obj, "'s ", type))
  } else if (p$status != 1) {
    abort(paste0("git error:\n", p$stderr))
  }

  return(id)
}

#' Return tibble of `git ls-tree` output
#'
#' @param tree A string naming a tree or something Git can dereference to one.
#' @param recurse Recurse into sub-trees.
#' @return A tibble with a row for each line of `git ls-tree` output.
#' @noRd
git_ls_tree <- function(tree, recurse = FALSE) {
  args  <- c("-c", "core.quotepath=false", "ls-tree")
  if (isTRUE(recurse)) {
    args <- c(args, "-r")
  }

  lines <- git_lines(c(args, tree))
  if (length(lines) == 0) {
    return(tibble::tibble(mode = character(0),
                          type = character(0),
                          name = character(0),
                          path = character(0)))
  }

  fields <- stringr::str_split_fixed(lines, "\t", 2)
  colnames(fields) <- c("rest", "path")
  tibble::as_tibble(fields) %>%
    tidyr::separate("rest", c("mode", "type", "name"), sep = " ")
}

#' Add files to Git object store
#'
#' @param files Files to add to object store. The basenames of these objects
#'   should be unique.
#' @return A tibble in the same form returned by `git_ls_tree()`, with the
#'   basename of each file in the `path` column.
#' @noRd
git_hash_files <- function(files) {
  purrr::map_dfr(files, git_hash_object)
}

git_hash_object <- function(f) {
  # Note that git_hash_files() could be rewritten to use --stdin-paths,
  # triggering one subprocess overall rather than a process for each file. For
  # this package, it's only going to be two files at a time, so go with the more
  # readable process-per-file approach.
  id <- git_string(c("hash-object", "-w", f))
  assert_valid_git_id(id, f)
  # Note: A blob can be mode 100644 or 100755, but hard coding it to the
  # non-executable version works for the purpose of this package.
  tibble::tibble_row(mode = "100644", type = "blob",
                     name = id, path = basename(f))
}

#' Make a Git tree from ls-tree entries
#'
#' @param entries A data frame of ls-tree entries in the same format returned by
#'   `git_ls_tree()`.
#' @return The ID of the tree.
#' @noRd
git_mktree <- function(entries) {
  lines <- paste(paste(entries$mode, entries$type, entries$name),
                 entries$path,
                 sep = "\t")
  p <- processx::process$new(
    "git", "mktree",
    stdin = "|", stdout = "|", stderr = "|")
  on.exit(p$kill(), add = TRUE)

  write_ret <- NULL
  while (!identical(write_ret, raw(0))) {
    write_ret <- p$write_input(lines)
  }
  close(p$get_input_connection())

  id <- stringr::str_trim(p$read_all_output())

  status <- p$get_exit_status()
  if (status != 0) {
    rlang::abort(paste0("git mktree:\n", p$read_all_error()))
  }

  assert_valid_git_id(id, "tree")
  return(id)
}
