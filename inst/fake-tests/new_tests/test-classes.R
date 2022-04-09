context("creating S3 objects")

# reference
MOD_CLASS <- "bbi_nonmem_model"
SUM_CLASS <- "bbi_nonmem_summary"
PROC_CLASS <- "babylon_process"
LOG_CLASS <- "bbi_run_log_df"

test_that("create_model_object() correctly assigns class [mrgval-TEST-0001]", {
  .mod <- list()
  .mod[[WORKING_DIR]] <- "naw"
  .mod[[YAML_YAML_NAME]] <- "naw"
  .mod[[YAML_YAML_MD5]] <- "naw"
  .mod[[YAML_MOD_TYPE]] <- "nonmem"
  .mod[[YAML_DESCRIPTION]] <- "naw"
  .mod[[YAML_MOD_PATH]] <- "naw.ctl"
  .mod[[YAML_OUT_DIR]] <- "naw"
  expect_false(inherits(.mod, MOD_CLASS))
  .mod <- create_model_object(.mod)
  expect_true(inherits(.mod, MOD_CLASS))
})

test_that("create_model_object() fails with non-valid model type [mrgval-TEST-0002]", {
  .mod <- list()
  .mod[[WORKING_DIR]] <- "naw"
  .mod[[YAML_MOD_TYPE]] <- "naw"
  .mod[[YAML_DESCRIPTION]] <- "naw"
  .mod[[YAML_MOD_PATH]] <- "naw.ctl"
  .mod[[YAML_OUT_DIR]] <- "naw"
  expect_error(create_model_object(.mod), regexp = "Invalid model_type")
})

test_that("create_model_object() fails with non-valid model file extension [mrgval-TEST-0003]", {
  .mod <- list()
  .mod[[WORKING_DIR]] <- "naw"
  .mod[[YAML_YAML_NAME]] <- "naw"
  .mod[[YAML_YAML_MD5]] <- "naw"
  .mod[[YAML_MOD_TYPE]] <- "nonmem"
  .mod[[YAML_DESCRIPTION]] <- "naw"
  .mod[[YAML_MOD_PATH]] <- "naw"
  .mod[[YAML_OUT_DIR]] <- "naw"
  expect_error(create_model_object(.mod), regexp = "model_path defined in yaml at naw must have either a .ctl or .mod extension")
})

test_that("create_model_object() errors if keys are missing [mrgval-TEST-0004]", {
  .mod <- list()
  .mod[[WORKING_DIR]] <- "naw"
  .mod[[YAML_MOD_TYPE]] <- "naw"
  #.mod[[YAML_DESCRIPTION]] <- "naw"
  expect_error(create_model_object(.mod), regexp = "Model list must have keys")
})


test_that("create_summary_object() correctly assigns class [mrgval-TEST-0005]", {
  .sum <- list()
  .sum[[SUMMARY_DETAILS]] <- "naw"
  expect_false(inherits(.sum, SUM_CLASS))
  .sum <- create_summary_object(.sum)
  expect_true(inherits(.sum, SUM_CLASS))
})

test_that("create_summary_object() errors if keys are missing [mrgval-TEST-0006]", {
  .sum <- list()
  #.sum[[SUMMARY_DETAILS]] <- "naw"
  .sum$naw <- "naw"
  expect_error(create_summary_object(.sum), regexp = "Summary object must have the following named elements")
})


test_that("create_process_object() correctly assigns class [mrgval-TEST-0007]", {
  .proc <- list()
  .proc[[PROC_PROCESS]] <- "naw"
  .proc[[PROC_STDOUT]] <- "naw"
  .proc[[PROC_BBI]] <- "naw"
  .proc[[PROC_CMD_ARGS]] <- "naw"
  .proc[[PROC_WD]] <- "naw"
  expect_false(inherits(.proc, PROC_CLASS))
  .proc <- create_process_object(.proc)
  expect_true(inherits(.proc, PROC_CLASS))
})

test_that("create_process_object() errors if keys are missing [mrgval-TEST-0008]", {
  .proc <- list()
  .proc[[PROC_PROCESS]] <- "naw"
  .proc[[PROC_STDOUT]] <- "naw"
  .proc[[PROC_BBI]] <- "naw"
  #.proc[[PROC_CMD_ARGS]] <- "naw"
  .proc[[PROC_WD]] <- "naw"
  expect_error(create_process_object(.proc), regexp = "Process object must have the following named elements")
})


test_that("create_run_log_object() correctly assigns class [mrgval-TEST-0009]", {
  .log_df <- tibble::tibble(
    !!ABS_MOD_PATH      := c("naw", "dawg"),
    !!YAML_YAML_MD5     := c("naw", "dawg"),
    !!YAML_MOD_TYPE     := c("naw", "dawg"),
    !!YAML_DESCRIPTION  := c("naw", "dawg"),
    !!YAML_BBI_ARGS     := c("naw", "dawg"),
    !!YAML_BASED_ON     := c("naw", "dawg"),
    !!YAML_TAGS         := c("naw", "dawg"),
    !!YAML_DECISIONS    := c("naw", "dawg")
  )
  expect_false(inherits(.log_df, LOG_CLASS))
  .log_df <- create_run_log_object(.log_df)
  expect_true(inherits(.log_df, LOG_CLASS))
})

test_that("create_run_log_object() errors if keys are missing [mrgval-TEST-0010]", {
  .log_df <- tibble::tibble(
    !!ABS_MOD_PATH      := c("naw", "dawg"),
    !!YAML_YAML_MD5     := c("naw", "dawg"),
    !!YAML_MOD_TYPE     := c("naw", "dawg"),
    #!!YAML_DESCRIPTION  := c("naw", "dawg"),
    !!YAML_BBI_ARGS     := c("naw", "dawg"),
    !!YAML_BASED_ON     := c("naw", "dawg"),
    !!YAML_TAGS         := c("naw", "dawg"),
    !!YAML_DECISIONS    := c("naw", "dawg")
  )
  expect_error(create_run_log_object(.log_df), regexp = "data.frame must have the following columns")
})


test_that("create_run_log_object() errors if ABS_MOD_PATH is not character [mrgval-TEST-0011]", {
  .log_df <- tibble::tibble(
    !!ABS_MOD_PATH      := c(1, 2),
    !!YAML_YAML_MD5     := c("naw", "dawg"),
    !!YAML_MOD_TYPE     := c("naw", "dawg"),
    !!YAML_DESCRIPTION  := c("naw", "dawg"),
    !!YAML_BBI_ARGS     := c("naw", "dawg"),
    !!YAML_BASED_ON     := c("naw", "dawg"),
    !!YAML_TAGS         := c("naw", "dawg"),
    !!YAML_DECISIONS    := c("naw", "dawg")
  )
  expect_error(create_run_log_object(.log_df), regexp = "column must be character type")
})

test_that("create_run_log_object() errors if ABS_MOD_PATH is not unique [mrgval-TEST-0012]", {
  .log_df <- tibble::tibble(
    !!ABS_MOD_PATH      := c("naw", "naw"),
    !!YAML_YAML_MD5     := c("naw", "dawg"),
    !!YAML_MOD_TYPE     := c("naw", "dawg"),
    !!YAML_DESCRIPTION  := c("naw", "dawg"),
    !!YAML_BBI_ARGS     := c("naw", "dawg"),
    !!YAML_BASED_ON     := c("naw", "dawg"),
    !!YAML_TAGS         := c("naw", "dawg"),
    !!YAML_DECISIONS    := c("naw", "dawg")
  )
  expect_error(create_run_log_object(.log_df), regexp = "column must contain unique values")
})

test_that("create_run_log_object() errors if ABS_MOD_PATH has missing values [mrgval-TEST-0013]", {
  .log_df <- tibble::tibble(
    !!ABS_MOD_PATH      := c(NA_character_, "naw", NA_character_),
    !!YAML_YAML_MD5     := c("aww", "naw", "dawg"),
    !!YAML_MOD_TYPE     := c("aww", "naw", "dawg"),
    !!YAML_DESCRIPTION  := c("aww", "naw", "dawg"),
    !!YAML_BBI_ARGS     := c("aww", "naw", "dawg"),
    !!YAML_BASED_ON     := c("aww", "naw", "dawg"),
    !!YAML_TAGS         := c("aww", "naw", "dawg"),
    !!YAML_DECISIONS    := c("aww", "naw", "dawg")
  )
  expect_error(create_run_log_object(.log_df), regexp = "column must NOT have any NA values.+1, 3")
})

