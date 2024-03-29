
############################
# reading and writing files
############################

#' Read in the csv will results from all the tests
#' @importFrom readr read_csv cols
#' @importFrom dplyr rename mutate_at
#' @param test_path File path to file containing test results
#' @keywords internal
read_test_df <- function(test_path = ALL_TESTS) {
  tst <- read_csv(test_path, col_types = cols())
  tst <- rename(tst,test_name = .data$tests, test_file = .data$file, number=.data$nb)
  tst <- mutate_at(tst,c("test_name","test_file"), .funs=list(trimws))
  return(tst)
}


#####################
# formatting helpers
#####################

#' Extract Summary section from Github issue
#' @importFrom purrr flatten_chr map_df
#' @importFrom tibble tibble
#' @param txt The raw text scraped from the body of the Github issue
#' @keywords internal
extract_issue_summary <- function(txt) {
  sp <- sp_sections(txt)
  ts <- strsplit(sp[2], "[\n\r]+") %>% flatten_chr %>% rm_blank
  story <- trimws(gsub("^ *[\r\n]+ *", "", sp[1]))
  return(story)
}

#' Extract Tests section from Github issue
#' @importFrom purrr flatten_chr map_df map_chr
#' @importFrom tibble tibble
#' @param txt The raw text scraped from the body of the Github issue
#' @keywords internal
extract_issue_tests <- function(txt) {
  sp <- sp_sections(txt)
  ts <- strsplit(sp[2], "[\n\r]+") %>% flatten_chr %>% rm_blank
  story <- trimws(gsub("^ *[\r\n]+ *", "", sp[1]))

  # get bulleted list entries of test file names and test names
  file_names_bullets <- which(grepl("test-.*\\.R$", ts))
  test_bullets <- which(grepl("(-|\\*) [A-Za-z0-9]+", ts))

  # filter to only test names (files names are no longer used in mrgvalidate >= 1.0.0)
  test_bullets <- setdiff(test_bullets, file_names_bullets)

  # clean test names and return
  map_chr(test_bullets, ~{rm_s(rm_h(ts[.x]))})
}



######################################
# helper functions for string cleanup
######################################

#' remove leading hyphen
#' @param x input to clean
#' @keywords internal
rm_h <- function(x) trimws(gsub("^ *- *", "",x))

#' remove leading star/bullet
#' @param x input to clean
#' @keywords internal
rm_s <- function(x) trimws(gsub("^ *\\* *", "",x))

#' drop blanks
#' @param x input to clean
#' @keywords internal
rm_blank <- function(x) x[!grepl("^\\s*$",x)]

#' split body by Test and Summary
#' @param x input to clean
#' @importFrom purrr flatten_chr
#' @keywords internal
sp_sections <- function(x) {
  x <- strsplit(x,"(\\# *Test\\w*|\\# *Summ\\w*)") %>% flatten_chr
  x <- rm_blank(x)
  if(length(x)!=2) {
    stop(paste("sp_sections() failed by splitting into wrong number of sections. Please see 'Basic Usage' vignette and check formatting in the following issue: ", x, collapse = " -------- "))
  }
  x
}


####################################
# Stories and requirements wrangling
####################################

#' Join the stories and requirements by requirement ID.
#' @param stories Data frame of stories, where the RequirementIds column has a
#'   list of requirement IDs associated with each story.
#' @param reqs Data frame of requirements, with each row identified by a unique
#'   requirement ID.
#' @importFrom dplyr full_join
#' @importFrom tidyr unnest
#' @keywords internal
merge_requirements_and_stories <- function(stories, reqs) {
  stories_flat <- stories %>%
    unnest("RequirementIds") %>%
    rename(RequirementId = "RequirementIds")
  return(full_join(stories_flat, reqs, by = "RequirementId"))
}
