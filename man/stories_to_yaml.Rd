% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stories-to-yaml.R
\name{stories_to_yaml}
\alias{stories_to_yaml}
\title{Write a stories tibble to YAML file}
\usage{
stories_to_yaml(sheet_df, file)
}
\arguments{
\item{sheet_df}{a dataframe of stories returned by \code{read_stories_only_gsheet()} or \code{parse_github_issues()}.}

\item{file}{output}
}
\description{
Write a stories tibble to YAML file
}
\details{
Takes a tibble with the following columns. For example, the tibble returned from either
\code{\link[=parse_github_issues]{parse_github_issues()}} or \code{\link[=read_spec_gsheets]{read_spec_gsheets()}} (only Option 2 - Stories Only).
}
\examples{

\dontrun{

# github milestones

MILESTONES <- c("v0.6.0", "v0.6.1")
parse_github_issues(org = "metrumresearchgroup", repo = "mrgvalidatetestreference",
 mile = MILESTONES, domain = "github.com") \%>\%
  stories_to_yaml(file = file.path(tempdir(), "temp.yaml")) \%>\%
  file.edit()


# google sheets

read_spec_gsheets(ss_stories = "1LpSX5Rb1XM5-xmQ8Wl2gQjMT5-3FIkuCM7oZhSgvWeI") \%>\%
  stories_to_yaml(file = file.path(tempdir(), "temp.yaml")) \%>\%
  file.edit()

}

}
