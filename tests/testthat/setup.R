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

STORIES_ONLY_COLS <- c(
  "StoryId", "StoryName", "StoryDescription", "ProductRisk", "TestIds")

STORIES_AND_REQS_COLS <- c(
  "StoryId", "StoryName", "StoryDescription",
  "RequirementId", "RequirementDescription",
  "ProductRisk", "TestIds")

STORY_TO_GSHEET_ONLY_COLS <-  c(
  "name","description", "ProductRisk", "tests")

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

#######
# helper functions

yaml_file <- function(name) {
  system.file("yaml-input", name, package = "mrgvalprep")
}
