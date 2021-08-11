#' Skip test if over Github API rate limit
#'
#' This is based on a function of the same name from the `remotes` package.
#' Note, this uses no token, so it is checking the public rate limit.
#' @importFrom utils download.file
#' @importFrom jsonlite fromJSON
#' @param by if less than this number of requests are left before hitting the rate limit, skip the test
#' @keywords internal
skip_if_over_rate_limit_github <- function(by = 5) {

  tmp <- tempfile(fileext = '.json')

  on.exit(unlink(tmp),add = TRUE)

  download.file("https://api.github.com/rate_limit", destfile = tmp, quiet = TRUE)

  res <- jsonlite::fromJSON(tmp, simplifyDataFrame = FALSE)

  res <- res$rate$remaining

  if (is.null(res) || res <= by) testthat::skip("Over the GitHub rate limit")
}

#' Skip test if over Googlesheets API rate limit
#'
#' This is currently not implemented but at some point we
#' may need it.
#' @keywords internal
skip_if_over_rate_limit_google <- function(by = 5) {
  message("skip_if_over_rate_limit_google(): Not actually checking google API rate limit.")
}
