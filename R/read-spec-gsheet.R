
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
  res <- if (is.null(ss_req)) {
    read_stories_only_gsheet(ss = ss_stories, sheet = sheet_stories)
  } else {
    stories <- read_stories_gsheet(ss = ss_stories, sheet = sheet_stories)
    reqs <- read_requirements_gsheet(ss = ss_req, sheet = sheet_req)
    merge_requirements_and_stories(stories, reqs)
  }

  return(res)
}

#' Join the stories and requirements by requirement ID.
#' @param stories Data frame of stories, where the RequirementIds column has a
#'   list of requirement IDs associated with each story.
#' @param reqs Data frame of requirements, with each row identified by a unique
#'   requirement ID.
#' @importFrom dplyr full_join
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @keywords internal
merge_requirements_and_stories <- function(stories, reqs) {
  stories_flat <- stories %>%
    unnest("RequirementIds") %>%
    rename(RequirementId = .data$RequirementIds)
  return(full_join(stories_flat, reqs, by = "RequirementId"))
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
