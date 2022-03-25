# reference character vectors for checking the contents of output files

# lists for combining and merging
LIST1 <- list(naw = 4, paw = 6)
LIST2 <- list(naw = 5, saw = "hey")

# file names and file paths
MOD_ID <- "1"
MODEL_DIR <- "model-examples"
OUTPUT_DIR <- as.character(glue::glue("{MODEL_DIR}/{MOD_ID}"))
YAML_TEST_FILE <- as.character(glue::glue("{MODEL_DIR}/{MOD_ID}.yaml"))
CTL_TEST_FILE <- as.character(glue::glue("{MODEL_DIR}/{MOD_ID}.ctl"))
MOD_TEST_FILE <- as.character(glue::glue("{MODEL_DIR}/{MOD_ID}.mod"))
LST_TEST_FILE <- as.character(glue::glue("{MODEL_DIR}/{MOD_ID}/{MOD_ID}.lst"))
GRD_TEST_FILE <- as.character(glue::glue("{MODEL_DIR}/{MOD_ID}/{MOD_ID}.grd"))
EXT_TEST_FILE <- as.character(glue::glue("{MODEL_DIR}/{MOD_ID}/{MOD_ID}.ext"))

# for combine_directory_path()
ABS_CTL_PATH <- file.path(getwd(), MODEL_DIR, glue::glue("{MOD_ID}.ctl"))
FAKE_CTL_PATH <- file.path(getwd(), MODEL_DIR, CTL_TEST_FILE)

# fake result object
MOD1 <- read_model(YAML_TEST_FILE, .directory = NULL)

# constants for workflow unit tests. Used in:
# - test-copy-model-from.R
# - test-modify-model-field.R
# - test-new-model.R
# - test-run-log.R

NEW_MOD2 <- file.path(MODEL_DIR, "2")
NEW_MOD3 <- file.path(MODEL_DIR, "3")

LEVEL2_DIR <- file.path(MODEL_DIR, "level2")
LEVEL2_MOD <- file.path(LEVEL2_DIR, "1")

ORIG_DESC <- "original acop model"
NEW_DESC <- "new description"
DESC_IN_CTL <- "PK model 1 cmt base"

ORIG_TAGS <- c("acop tag", "other tag")
NEW_TAGS <- c("new tag 1", "new tag 2")

NEW_TEXT1 <- c("naw", "paw")
NEW_TEXT2 <- c("all", "done")

MODEL_CLASS_LIST <- c("bbi_nonmem_model", "list")

MOD1_ABS_PATH <- file.path(getwd(), tools::file_path_sans_ext(YAML_TEST_FILE))
MOD2_ABS_PATH <- file.path(getwd(), NEW_MOD2)
MOD3_ABS_PATH <- file.path(getwd(), NEW_MOD3)
MOD4_ABS_PATH <- file.path(getwd(), LEVEL2_MOD)


# model refs

REF_LIST_1 <- list(
  description = ORIG_DESC,
  model_type = "nonmem",
  tags = ORIG_TAGS,
  bbi_args = list(
    overwrite = TRUE,
    threads = 4L
  ),
  model_working_dir = file.path(getwd(), "model-examples"),
  orig_yaml_file = "1.yaml",
  yaml_md5 = "ee5a30a015c4e09bc29334188ff28b58",
  model_path = "1.ctl",
  output_dir = "1"
)
class(REF_LIST_1) <- MODEL_CLASS_LIST


REF_LIST_TMP <- list(
  description = ORIG_DESC,
  model_type = "nonmem",
  tags = ORIG_TAGS,
  bbi_args = list(
    overwrite = TRUE,
    threads = 4L
  ),
  model_working_dir = file.path(getwd(), "model-examples"),
  orig_yaml_file = "tmp.yml",
  yaml_md5 = "ee5a30a015c4e09bc29334188ff28b58",
  model_path = "tmp.ctl",
  output_dir = "tmp"
)
class(REF_LIST_TMP) <- MODEL_CLASS_LIST
