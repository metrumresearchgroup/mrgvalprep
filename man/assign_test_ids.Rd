% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign-test-ids.R
\name{assign_test_ids}
\alias{assign_test_ids}
\title{Assigns Test Id's to stories generated via milestones}
\usage{
assign_test_ids(
  prefix,
  test_path = getOption("mrgvalprep.TEST_LOC"),
  overwrite = TRUE,
  first_id = 1
)
}
\arguments{
\item{prefix}{character string. Prefix for TestIds; usually an acronym of 3 letters signifying the associated package.}

\item{test_path}{path to where tests are written.}

\item{overwrite}{(T/F) whether or not to overwrite test files with new test ids}

\item{first_id}{integer. Desired starting point for test ids; must be greater than or equal to 1.}
}
\description{
This function will generate Test Id's with the pattern 'TST-FOO-XXX',
overwrite test documents to include these new IDs, and return a dataframe
with the new TestIds column.
}
