
#' Process issues from Github into a tibble of properly formatted stories
#'
#' Reads issues from a Github milestone and parses to a tibble in the format
#' expected by the `specs` argument of `mrgvalidate::create_package_docs()`.
#' See `mrgvalidate::input_formats` for details.
#' @importFrom tidyr unnest nest
#' @importFrom dplyr select mutate left_join
#' @importFrom rlang .data
#' @importFrom stringr str_pad
#' @importFrom checkmate assert_string
#' @param org Github organization that the repo is under.
#' @param repo The name of the repo for the package you are validating.
#' @param mile The name of the milestone associated with the release you are validating. All issues tied to this milestone with be pulled.
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com".
#' @param prefix character string. Prefix for StoryId; usually an acronym of 3 letters signifying the associated package.
#' @export
parse_github_issues <- function(org, repo, mile, domain = VALID_DOMAINS, prefix) {
  check_for_ghpm("parse_github_issues()")

  domain <- match.arg(domain)

  assert_string(prefix)

  release_issues <- get_issues(org, repo, mile, domain)

  n_stories <- length(release_issues$issue)
  story_ids <- paste0("S",str_pad(1:n_stories, max(nchar(n_stories) + 1, 3), pad = "0"))

  release_issues %>%
    mutate(
      StoryId = paste(toupper(prefix), story_ids, sep = "-"),
      StoryName = .data$title,
      StoryDescription = map_chr(.data$body, extract_issue_summary),
      TestIds = map(.data$body, extract_issue_tests)
    ) %>%
    left_join(
      get_risk(org, repo, domain),
      by = "issue"
    ) %>%
    select(.data$StoryId, .data$StoryName, .data$StoryDescription, .data$ProductRisk, .data$TestIds)

}


#' Get tibble of issues associated with a specific milestone
#'
#' Mainly a helper function called by [parse_github_issues()] but can also be
#' used to pull the raw content for issues associated with a given milestone.
#' @importFrom dplyr filter
#' @inheritParams parse_github_issues
#' @importFrom rlang .data
#' @seealso [parse_github_issues()]
#' @export
get_issues <- function(org, repo, mile, domain = VALID_DOMAINS) {
  check_for_ghpm("get_issues()")

  domain <- match.arg(domain)
  if (domain == "github.com") {
    domain <- paste0("api.", domain)
  }
  pkg_issues <- ghpm::get_issues(org, repo, ghpm::api_url(hostname = domain))
  release_issues <- pkg_issues %>% filter(.data$milestone %in% mile)

  return(release_issues)
}


#' Get risk field for each issue
#' @param org Github organization that the repo is under
#' @param repo The name of the repo for the package you are validating
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"
#' @importFrom dplyr filter select mutate
#' @importFrom rlang .data
#' @keywords internal
get_risk <- function(org, repo, domain = VALID_DOMAINS) {
  check_for_ghpm("get_risk()")

  domain <- match.arg(domain)
  if (domain == "github.com") {
    domain <- paste0("api.", domain)
  }
  issue_lab <- ghpm::get_issue_labels(org, repo, ghpm::api_url(hostname = domain))
  issue_lab <- filter(issue_lab, grepl("risk", .data$label, fixed = TRUE))
  risk <- select(issue_lab, .data$issue, risk = .data$label)
  risk %>%
    mutate(ProductRisk = sub("risk: ", "", .data$risk, fixed = TRUE)) %>%
    select(-.data$risk)
}

#' @keywords internal
check_for_ghpm <- function(.func) {
  ghpm_installed <- requireNamespace("ghpm", quietly = TRUE)
  if (!ghpm_installed || (ghpm_installed && utils::packageVersion("ghpm") < "0.5.1")) {
    stop(paste("must have ghpm >= 0.5.1 to use", .func))
  }
}
