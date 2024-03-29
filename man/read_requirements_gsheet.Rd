% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-spec-gsheet.R
\name{read_requirements_gsheet}
\alias{read_requirements_gsheet}
\title{Read requirements from a Google Sheet.}
\usage{
read_requirements_gsheet(
  ss,
  sheet = NULL,
  req_id_col = "RequirementId",
  req_description_col = "RequirementDescription",
  test_ids_col = "TestIds"
)
}
\arguments{
\item{ss, sheet}{Sheet identifiers passed \code{\link[googlesheets4:range_read]{googlesheets4::read_sheet()}}.}

\item{req_id_col, req_description_col, test_ids_col}{Names of relevant columns
in input Google Sheet.}
}
\value{
Tibble with the above columns.
}
\description{
Read requirements from a Google Sheet.
}
\keyword{internal}
