create_all_models <- function() {
  mod1 <- read_model(YAML_TEST_FILE)
  mod2 <- copy_model_from(mod1, NEW_MOD2, "level 1 copy of 1")
  mod3 <- copy_model_from(mod1, NEW_MOD3, "level 1 copy of 1")
  fs::dir_create(LEVEL2_DIR)
  mod4 <- copy_model_from(mod2, LEVEL2_MOD, "level 2 copy of 2")

  # load or create models and assign model objects to global environment
  assign("mod1", mod1, pos = parent.frame())
  assign("mod2", mod2, pos = parent.frame())
  assign("mod3", mod3, pos = parent.frame())
  assign("mod4", mod4, pos = parent.frame())
}

cleanup <- function() {
  # delete tmp files if they are leftover from previous test
  mods_to_kill <- purrr::map_chr(seq(2, 7), ~ file.path(MODEL_DIR, .x))
  for (m in mods_to_kill) {
    if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
    if (fs::file_exists(paste0(m, ".yml"))) fs::file_delete(paste0(m, ".yml"))
    if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
  }
  if (fs::dir_exists(LEVEL2_DIR)) fs::dir_delete(LEVEL2_DIR)

  # delete model objects from memory
  suppressSpecificWarning(rm(mod1, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod2, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod3, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(mod4, pos = parent.frame()), .regexpr = "object.+not found")
  suppressSpecificWarning(rm(log_df, pos = parent.frame()), .regexpr = "object.+not found")
}
