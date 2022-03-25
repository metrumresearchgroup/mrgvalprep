
##################
# constants, etc.
##################

VALID_DOMAINS <- c("github.com", "ghe.metrumrg.com")

# default file paths

ALL_TESTS <- "all_tests.csv"

NO_TESTS_STRING <- "No tests"

# pkg <- devtools::as.package(".")
# path <- pkg$path
options(TEST_DIR = file.path(devtools::as.package(".")$path, "tests", "testthat"))
# rm(list= c("pkg", "path"))
