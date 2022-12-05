#' Write a stories tibble to YAML file
#'
#' @details
#'
#' Takes a tibble with the following columns. For example, the tibble returned from either
#'  [parse_github_issues()] or [read_spec_gsheets()] (only Option 2 - Stories Only).
#'
#'
#' @param sheet_df a dataframe of stories returned by `read_stories_only_gsheet()` or `parse_github_issues()`.
#' @param file output
#'
#' @examples
#'
#'\dontrun{
#'
#' # github milestones
#'
#' MILESTONES <- c("v0.6.0", "v0.6.1")
#' parse_github_issues(org = "metrumresearchgroup", repo = "mrgvalidatetestreference",
#'  mile = MILESTONES, domain = "github.com") %>%
#'   stories_to_yaml(file = file.path(tempdir(), "temp.yaml")) %>%
#'   file.edit()
#'
#'
#' # google sheets
#'
#' read_spec_gsheets(ss_stories = "1LpSX5Rb1XM5-xmQ8Wl2gQjMT5-3FIkuCM7oZhSgvWeI") %>%
#'   stories_to_yaml(file = file.path(tempdir(), "temp.yaml")) %>%
#'   file.edit()
#'
#'}
#'
#' @importFrom stringr str_split
#' @importFrom yaml write_yaml
#' @importFrom stats setNames
#' @importFrom utils file.edit
#' @export
stories_to_yaml <- function(
  sheet_df,
  file
){
  dd <- sheet_df %>%
    mutate(TestIds = sapply(.data$TestIds, toString))
  dl <- dd %>%
    rename(
      name = "StoryName",
      description = "StoryDescription",
      tests = "TestIds"
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
