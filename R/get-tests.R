
#' Get tests/testthat directory from package directory
#'
#' @param pkg_dir a file path pointing to an unpacked/untarred package directory
#'
#' @export
get_testing_dir <- function(pkg_dir){
  checkmate::assert_directory_exists(pkg_dir)

  pkg_dir_ls <- fs::dir_ls(pkg_dir)
  test_dir_outer <- pkg_dir_ls[grep("^(/[^/]+)+/tests$", pkg_dir_ls)]
  if(length(test_dir_outer) == 0){
    stop(glue::glue("no testing directory found at {pkg_dir}"))
  }

  test_dir_ls <- fs::dir_ls(test_dir_outer)
  test_dir_inner <- test_dir_ls[grep("^(/[^/]+)+/testthat$", test_dir_ls)]
  if(length(test_dir_inner) == 0){
    stop(glue::glue("no `testthat` directory found at {test_dir_outer}"))
  }

  return(test_dir_inner)
}

#' Tabulate all tests in a package directory
#'
#' @inheritParams get_testing_dir
#' @param test_dirs one or more directories containing test scripts
#'
#' @details
#' Note that `get_testing_dir` will only return the directory `tests/testthat` (if it exists).
#' You must specify other directories manually if you have multiple.
#'
#' ```
#' # Example
#' test_dirs = c(
#'   get_testing_dir(pkg_dir),
#'   'inst/other-tests'
#' )
#' ```
#'
#' @returns a dataframe containing all test names and the test file they are associated with
#'
#' @export
get_tests <- function(
    pkg_dir,
    test_dirs = get_testing_dir(pkg_dir)
){

  test_names_df <- purrr::map_dfr(test_dirs, function(test_dir_x){
    test_files <- fs::dir_ls(test_dir_x)
    test_scripts <- test_files[grep("\\.R$", test_files, ignore.case = TRUE)]

    tests_names_lst <- purrr::map(test_scripts, ~{
      test_script <- readLines(.x)

      # Find test_that calls and extract the names of the tests
      test_that_calls <- grep('(?:\\s|^)test_that\\("([^"]+)"', test_script)
      test_that_names <- test_script[test_that_calls]

      # Find it calls and extract the names of the tests
      it_calls <- grep('(?:\\s|^)it\\("([^"]+)"', test_script)
      it_names <- test_script[it_calls]

      # Combine the names of tests and count the number of tests in each file
      n_tests <- length(c(test_that_names[test_that_calls], it_names[it_calls]))
      test_names <- c(test_that_names, it_names)

      # Remove `test_that` and `it` calls and code
      stringr::str_extract_all(test_names, "\"([^\"]*)\"|'([^']*)'") %>%
        gsub("[\"',/{]", "", .)
    }) %>% stats::setNames(basename(test_scripts))

    # Remove empty scripts (setup/non-test scripts)
    tests_names_lst <- Filter(Negate(rlang::is_empty), tests_names_lst)

    # Convert to dataframe
    test_names_tbl <- purrr::map2_df(tests_names_lst, names(tests_names_lst),
                                     ~ tibble(result_file = .y, TestName = .x)) %>%
      dplyr::mutate(test_dir = fs::path_rel(test_dir_x, start = pkg_dir))

    # Make sure no elements were lost (this can be part of a test)
    checkmate::assert_true(all(unname(unlist(tests_names_lst)) %in% test_names_tbl$TestName))

    test_names_tbl
  })

  test_names_df

  return(test_names_df)
}
