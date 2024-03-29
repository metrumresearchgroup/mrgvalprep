% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse-github-issues.R
\name{get_issues}
\alias{get_issues}
\title{Get tibble of issues associated with a specific milestone}
\usage{
get_issues(org, repo, mile, domain = VALID_DOMAINS)
}
\arguments{
\item{org}{Github organization that the repo is under.}

\item{repo}{The name of the repo for the package you are validating.}

\item{mile}{The name of the milestone associated with the release you are validating. All issues tied to this milestone with be pulled.}

\item{domain}{Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com".}
}
\description{
Mainly a helper function called by \code{\link[=parse_github_issues]{parse_github_issues()}} but can also be
used to pull the raw content for issues associated with a given milestone.
}
\seealso{
\code{\link[=parse_github_issues]{parse_github_issues()}}
}
