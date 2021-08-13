# mrgvalprep 0.0.1

First release! This release moved some functionality that used to be in `mrgvalidate 0.1.2` into its own package. Much of the old code has been heavily refactored, and new functionality has been added as well.

## Current capabilities

* Pull an R package Github repo at a specific tag and run its test suite, then format the output for `mrgvalidate` (`validate_tests()`)
* Parses output from `testthat::ListReporter` (`parse_testthat_list_reporter()`)
* Pulls Stories and Requirements from Googlesheets (`read_spec_gsheets()`)
* Pulls Stories and Requirements from Github issues and milestones (`parse_github_issues()`)
