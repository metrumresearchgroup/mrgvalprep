
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
