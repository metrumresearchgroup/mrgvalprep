
#' Read requirements and stories from Google Sheets.
#'
#' @details
#' ## Option 1: Stories and Requirements
#'
#' The stories sheet passed to `ss_stories` must have the following columns:
#' * `StoryId` (character scalar)
#' * `StoryName` (character scalar)
#' * `StoryDescription` (character scalar)
#' * `ProductRisk` (character scalar)
#' * `RequirementIds` (character vector)
#'
#' The requirements sheet passed to `ss_req` must have the following columns:
#' * `RequirementId` (character scalar)
#' * `RequirementDescription` (character scalar)
#' * `TestIds` (character vector)
#'
#' ## Option 2: Stories linked directly to Tests
#'
#' Don't pass anything to `ss_req`. The stories sheet passed to `ss_stories` must have the following columns:
#' * `StoryId` (character scalar)
#' * `StoryName` (character scalar)
#' * `StoryDescription` (character scalar)
#' * `ProductRisk` (character scalar)
#' * `TestIds` (character vector)
#'
#' (Note: these are the same columns as Option 1, but replacing `RequirementIds` with `TestIds`)
#'
#' @param ss_stories,ss_req,sheet_stories,sheet_req Sheet identifiers for the
#'   stories and requirements passed along as the `ss` and `sheet` arguments to
#'   [googlesheets4::read_sheet()].
#' @return Tibble joining requirements and stories by RequirementId.
#' @export
read_spec_gsheets <- function
(
  ss_stories, ss_req = NULL,
  sheet_stories = NULL, sheet_req = NULL
) {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    rlang::abort("Need to install googlesheets4 to use read_spec_gsheets()")
  }
  res <- if (is.null(ss_req)) {
    read_stories_only_gsheet(ss = ss_stories, sheet = sheet_stories)
  } else {
    stories <- read_stories_gsheet(ss = ss_stories, sheet = sheet_stories)
    reqs <- read_requirements_gsheet(ss = ss_req, sheet = sheet_req)
    merge_requirements_and_stories(stories, reqs)
  }

  return(res)
}

#' Read requirements from a Google Sheet.
#' @param ss,sheet Sheet identifiers passed [googlesheets4::read_sheet()].
#' @param req_id_col,req_description_col,test_ids_col Names of relevant columns
#'   in input Google Sheet.
#' @return Tibble with the above columns.
#' @importFrom dplyr rename select mutate
#' @importFrom rlang .data
#' @keywords internal
read_requirements_gsheet <- function
(
  ss, sheet = NULL,
  req_id_col = "RequirementId",
  req_description_col = "RequirementDescription",
  test_ids_col = "TestIds"
) {
  dd <- googlesheets4::read_sheet(ss = ss, sheet = sheet)
  dd %>%
    rename(RequirementId = !!req_id_col,
           RequirementDescription = !!req_description_col,
           TestIds = !!test_ids_col) %>%
    select(.data$RequirementId, .data$RequirementDescription, .data$TestIds) %>%
    mutate(TestIds = stringr::str_split(.data$TestIds, "[\\s,;]+"))
}


#' Read stories from a Google Sheet.
#' @param ss,sheet Sheet identifiers passed [googlesheets4::read_sheet()].
#' @param story_id_col,story_name_col,story_description_col,risk_col,req_ids_col
#'   Names of relevant columns in input Google Sheet.
#' @return Tibble with the above columns.
#' @importFrom dplyr rename select mutate
#' @importFrom rlang .data
#' @keywords internal
read_stories_gsheet <- function
(
  ss, sheet = NULL,
  story_id_col = "StoryId",
  story_name_col = "StoryName",
  story_description_col = "StoryDescription",
  risk_col = "ProductRisk",
  req_ids_col = "RequirementIds"
) {
  dd <- googlesheets4::read_sheet(ss = ss, sheet = sheet)
  dd %>%
    rename(StoryId = !!story_id_col,
           StoryName = !!story_name_col,
           StoryDescription = !!story_description_col,
           ProductRisk = !!risk_col,
           RequirementIds = !!req_ids_col) %>%
    select(.data$StoryId, .data$StoryName, .data$StoryDescription,
           .data$ProductRisk, .data$RequirementIds) %>%
    mutate(RequirementIds = stringr::str_split(.data$RequirementIds, "[\\s,;]+"))
}

#' Read a stories gsheet that maps to tests instead of requirements
#' @keywords internal
read_stories_only_gsheet <- function
(
  ss, sheet = NULL,
  story_id_col = "StoryId",
  story_name_col = "StoryName",
  story_description_col = "StoryDescription",
  risk_col = "ProductRisk",
  test_ids_col = "TestIds"
) {
  dd <- googlesheets4::read_sheet(ss = ss, sheet = sheet)
  dd %>%
    rename(StoryId = !!story_id_col,
           StoryName = !!story_name_col,
           StoryDescription = !!story_description_col,
           ProductRisk = !!risk_col,
           TestIds = !!test_ids_col) %>%
    select(.data$StoryId, .data$StoryName, .data$StoryDescription,
           .data$ProductRisk, .data$TestIds) %>%
    mutate(TestIds = stringr::str_split(.data$TestIds, "[\\s,;]+"))
}

#' Read stories from a Google Sheet and write to a yaml file.
#'
#' @details
#'
#' ## Option 1: Stories and Requirements
#'
#' (Note: Option 1 is not currently implemented for `gsheet_to_yaml()`)
#'
#'
#' ## Option 2: Stories linked directly to Tests
#'
#' The stories sheet passed to `ss_stories` must have the following columns:
#' * `StoryId` (character scalar)
#' * `StoryName` (character scalar)
#' * `StoryDescription` (character scalar)
#' * `ProductRisk` (character scalar)
#' * `TestIds` (character vector)
#'
#'
#'
#' @param ss,sheet Sheet identifiers passed [googlesheets4::read_sheet()].
#' @param file output
#' @param story_id_col,story_name_col,story_description_col,risk_col,test_ids_col
#'   Names of relevant columns in input Google Sheet.
#' @importFrom stringr str_split
#' @importFrom yaml write_yaml
#' @importFrom stats setNames
#' @importFrom utils file.edit
#' @export
gsheet_to_yaml <- function(
  ss,
  sheet = NULL,
  file
){
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    rlang::abort("Need to install googlesheets4 to use read_spec_gsheets()")
  }
  dd <- read_stories_only_gsheet(ss = ss, sheet = sheet) %>%
    mutate(TestIds = sapply(.data$TestIds, toString))
  dl <- dd %>%
    rename(
      name = .data$StoryName,
      description = .data$StoryDescription,
      tests = .data$TestIds
    ) %>%
    split(seq(nrow(dd))) %>%
    setNames(dd$StoryId) %>%
    map(~ {
      .x <- as.list(.x)
      .x$StoryId <- NULL
      .x$tests <- unlist(stringr::str_split(.x$tests, ", *"))
      .x
    })
  yaml::write_yaml(dl, file)
return(file)
}

