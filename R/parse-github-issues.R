
#' Process issues from Github into a tibble of properly formatted stories
#'
#' Reads issues from a Github milestone and parses to a tibble in the format
#' expected by the `specs` argument of `mrgvalidate::create_validation_docs()`.
#' See `mrgvalidate::input_formats` for details.
#' @importFrom tidyr unnest nest
#' @importFrom dplyr select mutate left_join
#' @importFrom rlang .data
#' @param org Github organization that the repo is under.
#' @param repo The name of the repo for the package you are validating.
#' @param mile The name of the milestone associated with the release you are validating. All issues tied to this milestone with be pulled.
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com".
#' @export
parse_github_issues <- function(org, repo, mile, domain = VALID_DOMAINS) {
  domain <- match.arg(domain)

  release_issues <- get_issues(org, repo, mile, domain)

  release_issues %>%
    mutate(
      StoryId = paste(repo, .data$issue, sep = "-"),
      StoryName = .data$title,
      StoryDescription = map_chr(.data$body, extract_issue_summary),
      TestIds = map(.data$body, extract_issue_tests)
      #'
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
#' @importFrom ghpm api_url
#' @inheritParams parse_github_issues
#' @importFrom rlang .data
#' @seealso [parse_github_issues()]
#' @export
get_issues <- function(org, repo, mile, domain = VALID_DOMAINS) {
  domain <- match.arg(domain)
  if (domain == "github.com") {
    domain <- paste0("api.", domain)
  }
  pkg_issues <- ghpm::get_issues(org, repo, api_url(hostname = domain))
  release_issues <- pkg_issues %>% filter(.data$milestone == mile)

  return(release_issues)
}


#' Get risk field for each issue
#' @param org Github organization that the repo is under
#' @param repo The name of the repo for the package you are validating
#' @param domain Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"
#' @importFrom dplyr filter select mutate
#' @importFrom ghpm api_url
#' @importFrom rlang .data
#' @keywords internal
get_risk <- function(org, repo, domain = VALID_DOMAINS) {
  domain <- match.arg(domain)
  if (domain == "github.com") {
    domain <- paste0("api.", domain)
  }
  issue_lab <- ghpm::get_issue_labels(org, repo, api_url(hostname = domain))
  issue_lab <- filter(issue_lab, grepl("risk", .data$label, fixed = TRUE))
  risk <- select(issue_lab, .data$issue, risk = .data$label)
  risk %>%
    mutate(ProductRisk = sub("risk: ", "", .data$risk, fixed = TRUE)) %>%
    select(-.data$risk)
}
