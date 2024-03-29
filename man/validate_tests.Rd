% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate-tests.R
\name{validate_tests}
\alias{validate_tests}
\title{Run and summarize tests}
\usage{
validate_tests(
  org,
  repo,
  version,
  domain = VALID_DOMAINS,
  out_file = NULL,
  root_dir = tempdir(),
  output_dir = getwd(),
  extra_test_dirs = NULL,
  set_id_to_name = FALSE
)
}
\arguments{
\item{org}{Github organization that the repo is under}

\item{repo}{The name of the repo for the package you are validating}

\item{version}{The version number of the package you are validating. This must correspond to a tag in the repo.}

\item{domain}{Domain where repo lives. Either "github.com" or "ghe.metrumrg.com", defaulting to "github.com"}

\item{out_file}{File path to write out the test results to. Any extension
will be ignored and replaced with \code{.csv} for the test output df and
\code{.json} for the \code{get_sys_info()} output.}

\item{root_dir}{The directory path to where the package has been cloned.
\code{file.path(root_dir, repo)} should lead to the cloned repo that will be
tested.}

\item{output_dir}{Directory to write the test outputs to. Defaults to
working directory.}

\item{extra_test_dirs}{Character vector of paths (relative to package root
dir) to directories that contain additional tests to run}

\item{set_id_to_name}{Whether to copy the \code{TestName} column over to the
\code{TestId} column when writing the output CSV. This exists to support legacy
issues and tests that don't use IDs. Starting with v1.0, mrgvalidate relies
on test IDs and, as a temporary compatibility kludge, will auto-generate
them if the \code{TestId} and \code{TestName} columns match. New tests and issues
should use test IDs.}
}
\value{
Invisibly returns tibble of formatted test output. Note that this
function is primarily called for the side effect of writing out the
output to disk.
}
\description{
Will clone \code{repo} at \code{version} tag and run test suite. Package will be built
and installed (along with any missing dependencies) to a temporary library.
Then we run all tests for \code{repo}, roll up successes and failures, and write
to a CSV file. Also writes the commit hash of the package and some other
system info to a JSON file.
}
\details{
The package found at \code{file.path(root_dir, repo)} will be built and installed
into a tempory library that is appended to the \emph{beginning} of your
\code{.libPaths()}, using \code{devtools::install(..., build = TRUE, upgrade = "never")}. This will also install (into the temporary library) any
dependencies from the package that are \emph{not} found in your \code{.libPaths()}.
}
