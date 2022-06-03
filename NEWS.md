# mrgvalprep 0.0.5

## New features and changes

* Added helpers for converting legacy formats of specifying stories and requirements into the preferred YAML format.
  * `stories_to_yaml()` helper for helping to convert stories specified in either Googlesheets or Github milestones into YAML. (#31, #35)
  * `assign_test_ids()` and `milestone_to_test_id()` for converting tests specified with full test names into test IDs. (#36, #39, #42)
  
* Added helpers for converting from using only stories to using stories and requirements. (#44)

## Bug fixes

* Moved `checkmate` to `Imports` instead of `Suggests` because it is used in core functionality. (#29)

* Fixed check for unique requirement IDs in `read_spec_yaml()`. (#43)

# mrgvalprep 0.0.4

## New features and changes

* With the new function `read_spec_yaml()`, stories and requirements can now be provided through YAML files. (#23)

* `googlesheets4` has been moved from `Imports` to `Suggests` so that users that only want to use YAML input don't need to install it. (#24)

* `parse_testthat_list_reporter()` and `parse_golang_test_json()` now strip a trailing colon from the test name when rolling up test IDs. (#19)

## Docs

* The documentation for `parse_testthat_list_reporter()` and `parse_golang_test_json()` now describe the requirements for the test name when `roll_up_ids = TRUE`.  (#19)


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
