
#' @importFrom stringr str_squish
read_requirements_yaml <- function(content) {
  purrr::map_dfr(
    content,
    ~ list(RequirementDescription = str_squish(.x[["description"]]),
           TestIds = purrr::set_names(.x["tests"], NULL)),
    .id = "RequirementId")
}

#' @importFrom stringr str_squish
extract_stories_shared_cols <- function(x) {
  list(StoryName = str_squish(x[["name"]]),
       StoryDescription = str_squish(x[["description"]]),
       ProductRisk = x[["ProductRisk"]])
}

read_stories_yaml <- function(content, stories_only) {
  extract <- if (stories_only) {
    function(x)
      c(extract_stories_shared_cols(x),
        list(TestIds = purrr::set_names(x["tests"], NULL)))
  } else {
    function(x)
      c(extract_stories_shared_cols(x),
        list(RequirementIds = purrr::set_names(x["requirements"], NULL)))
  }

  purrr::map_dfr(content, extract, .id = "StoryId")
}

#' Read stories and requirements from YAML files
#'
#' Convert a set of stories and optionally requirements from YAML files into a
#' data frame that is ready to be passed as the `specs` argument to
#' `mrgvalidate::create_validation_docs()`.
#'
#' @details
#'
#' ## Option 1: Stories and requirements
#'
#' If you provide a value for both `stories` and `requirements`, the `stories`
#' argument should be one or more YAML files with the following structure:
#'
#' ```
#' STORY-ID-001:
#'   name: Story name
#'   description: >
#'     Longer story description
#'   requirements:
#'   - REQ-ID-001
#'   - REQ-ID-002
#'   ProductRisk: risk
#' STORY-ID-002:
#' [...]
#' ```
#'
#' The `requirements` argument should be a set of YAML files that have entries
#' that link those requirements to tests:
#'
#' ```
#' REQ-ID-001:
#'   description: >
#'     Description
#'   tests:
#'   - TEST-ID-001
#'   - TEST-ID-001
#' ```
#'
#' The information above is used to construct a data frame with the following
#' columns:
#'
#' * `StoryId` (character scalar)
#' * `StoryName` (character scalar)
#' * `StoryDescription` (character scalar)
#' * `ProductRisk` (character scalar)
#' * `RequirementIds` (character vector)
#' * `RequirementDescription` (character scalar)
#' * `TestIds` (character vector)
#'
#' ## Option 2: Stories linked directly to tests
#'
#' If you provide only a `stories` argument, the YAML entries should specify a
#' list of tests rather than a set of requirements:
#'
#' ```
#' STORY-ID-001:
#'   name: Story name
#'   description: >
#'     Longer story description
#'   tests:
#'   - TEST-ID-001
#'   - TEST-ID-002
#'   ProductRisk: risk
#' STORY-ID-002:
#' [...]
#' ```
#'
#' Those entries are used to construct a data frame with the following columns:
#'
#' * `StoryId` (character scalar)
#' * `StoryName` (character scalar)
#' * `StoryDescription` (character scalar)
#' * `ProductRisk` (character scalar)
#' * `TestIds` (character vector)
#'
#' @param stories,requirements Character vector of YAML files to read stories
#'   and requirements from.
#' @return Tibble with columns as described by `mrgvalidate::input_formats`.
#' @export
read_spec_yaml <- function(stories, requirements = NULL) {

  stories_only <- is.null(requirements)
  res <- dplyr::bind_rows(
    purrr::map(
      stories,
      ~ read_stories_yaml(yaml::read_yaml(.x), stories_only))) %>%
    check_uniq_col_vals("StoryId")

  if (!stories_only) {
    df_reqs <- dplyr::bind_rows(
      purrr::map(
        requirements,
        ~ read_requirements_yaml(yaml::read_yaml(.x)))) %>%
      check_uniq_col_vals("RequirementId")
    res <- merge_requirements_and_stories(res, df_reqs)
  }

  return(res)
}

#' Check unique values of column
check_uniq_col_vals <- function(df, col_uniq) {
  uids <- df[[col_uniq]]
  if (any(duplicated(uids))) {
    rlang::abort(paste0("Duplicate values for ", col_uniq, " found:\n",
                        paste0(" - ", uids[duplicated(uids)],
                               collapse = "\n")),
                 "mrgvalprep_input_error")
  }
  return(df)
}
