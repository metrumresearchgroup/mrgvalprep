library(stringr)

###########################
# constants for test input
###########################

DOMAIN <- "github.com"
ORG <- "metrumresearchgroup"
REPO <- "mrgvalidatetestreference"
MILESTONE <- "v0.6.0"
TAG <- "0.6.3"

STORY_RDS <- "stories_df.RDS"

OUTPUT_DIR <- "level2"

EXTRA_TESTS <- "inst/extra-tests"

# for optional GHE test in test-ghe.R
GHE_DOMAIN <- "ghe.metrumrg.com"
GHE_ORG <- "tech-solutions"
GHE_REPO <- "mrgvalidatetestreference"
GHE_MILESTONE <- "v0.6.0"
GHE_TAG <- "0.6.0"



#############################
# reference for test results
#############################

COMMIT_REF <- "15430d3a6d77adc2e955d4f1e22209e63e6d7f60"
GHE_COMMIT_REF <- "0817f579b2858cfce24975776f83302bff9162ba"

TEST_DF_ROWS <- 173
TEST_DF_COLS <- 4
TEST_DF_ROWS_EXTRA_TESTS <- 177

STORIES_DF_ROWS_GHP <- 6 # this will change when new stories are added for new test cases
STORIES_DF_COLS_GHP <- 5
STORIES_DF_ROWS_GHE <- 5 # this shouldn't ever change because the GHE repo is static

SPECS_DF_ROWS_GS <- 6
SPECS_DF_COLS_GS <- 7

VAL_FILE <- "validation-testing.md"
REQ_FILE <- "requirements-specification.md"
MAT_FILE <- "traceability-matrix.md"

#######
# helper functions

#' Check that content from spec is rendered in docs
#' @param spec Tibble mapping Stories (and optionally Requirements) to Tests
#' @param docs_output_dir Directory where rendered validation docs to check are
#' @param set_id_to_name Same as in [validate_tests()]
check_docs <- function(spec, docs_output_dir, set_id_to_name = FALSE) {
  # check that files exist
  expect_true(fs::file_exists(file.path(docs_output_dir, paste0(tools::file_path_sans_ext(REQ_FILE), ".docx"))))
  expect_true(fs::file_exists(file.path(docs_output_dir, paste0(tools::file_path_sans_ext(VAL_FILE), ".docx"))))
  expect_true(fs::file_exists(file.path(docs_output_dir, paste0(tools::file_path_sans_ext(MAT_FILE), ".docx"))))
  expect_true(fs::file_exists(file.path(docs_output_dir, REQ_FILE)))
  expect_true(fs::file_exists(file.path(docs_output_dir, VAL_FILE)))
  expect_true(fs::file_exists(file.path(docs_output_dir, MAT_FILE)))

  # check for files content
  req_text <- readr::read_file(file.path(docs_output_dir, REQ_FILE))
  expect_true(any(str_detect(req_text, spec$StoryId)))
  expect_true(any(str_detect(req_text, unlist(spec$TestIds))))
  if ("RequirementId" %in% names(spec)) expect_true(any(str_detect(req_text, spec$RequirementId)))

  val_text <- readr::read_file(file.path(docs_output_dir, VAL_FILE))
  expect_true(any(str_detect(val_text, unlist(spec$TestIds))))

  mat_text <- readr::read_file(file.path(docs_output_dir, MAT_FILE))
  expect_true(any(!!str_detect(mat_text, fixed(str_extract(str_trim(spec$StoryDescription), "^.+")))))
  if (isFALSE(set_id_to_name)) expect_true(any(str_detect(mat_text, unlist(spec$TestIds))))
}
