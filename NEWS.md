# mrgvalprep 0.0.3

## New features and changes

* Added `roll_up_ids` argument to `parse_testthat_list_reporter()` and `parse_golang_test_json()` (#16)

* Moved `ghpm` to Suggests and put checks on all functions that touch Github (#15)

# mrgvalprep 0.0.2

## New features and changes

* Added `parse_golang_test_json()` for Go test `go test --json` output (#10)

## Developer-facing changes

* Enabled drone CI (#11)
* Added `skip_if_no_github_pat()` to test suite

# mrgvalprep 0.0.1

First release! This release moved some functionality that used to be in `mrgvalidate 0.1.2` into its own package. Much of the old code has been heavily refactored, and new functionality has been added as well.

## Current capabilities

* Pull an R package Github repo at a specific tag and run its test suite, then format the output for `mrgvalidate` (`validate_tests()`)
* Parses output from `testthat::ListReporter` (`parse_testthat_list_reporter()`)
* Pulls Stories and Requirements from Googlesheets (`read_spec_gsheets()`)
* Pulls Stories and Requirements from Github issues and milestones (`parse_github_issues()`)
