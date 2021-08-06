
############################
# reading and writing files
############################

#' Read in the csv will results from all the tests
#' @importFrom readr read_csv cols
#' @importFrom dplyr rename mutate_at
#' @importFrom rlang .data
#' @param test_path File path to file containing test results
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
extract_issue_summary <- function(txt) {
  sp <- sp_sections(txt)
  ts <- strsplit(sp[2], "[\n\r]+") %>% flatten_chr %>% rm_blank
  story <- trimws(gsub("^ *[\r\n]+ *", "", sp[1]))
  return(story)
}

#' Extract Tests section from Github issue
#' @importFrom purrr flatten_chr map_df
#' @importFrom tibble tibble
#' @param txt The raw text scraped from the body of the Github issue
extract_issue_tests <- function(txt) {
  sp <- sp_sections(txt)
  ts <- strsplit(sp[2], "[\n\r]+") %>% flatten_chr %>% rm_blank
  story <- trimws(gsub("^ *[\r\n]+ *", "", sp[1]))

  file_start <- which(grepl("test-.*\\.R$", ts))
  test_start <- file_start+1
  test_end <- c((file_start - 1)[-1],length(ts))

  if (length(file_start) == 0) {
    character()
  }

  labs <- rm_h(ts[file_start])
  .lab <- basename(labs)
  map(seq_along(file_start), function(i) {
    se <- seq(test_start[i],test_end[i])
    rm_s(rm_h(ts[se]))
  }) %>%
    unlist()
}



######################################
# helper functions for string cleanup
######################################

#' remove leading hyphen
#' @param x input to clean
rm_h <- function(x) trimws(gsub("^ *- *", "",x))

#' remove leading star/bullet
#' @param x input to clean
rm_s <- function(x) trimws(gsub("^ *\\* *", "",x))

#' drop blanks
#' @param x input to clean
rm_blank <- function(x) x[!grepl("^\\s*$",x)]

#' split body by Test and Summary
#' @param x input to clean
#' @importFrom purrr flatten_chr
sp_sections <- function(x) {
  x <- strsplit(x,"(\\# *Test\\w*|\\# *Summ\\w*)") %>% flatten_chr
  x <- rm_blank(x)
  if(length(x)!=2) {
    stop(paste("sp_sections() failed by splitting into wrong number of sections. Please see 'Basic Usage' vignette and check formatting in the following issue: ", x, collapse = " -------- "))
  }
  x
}

