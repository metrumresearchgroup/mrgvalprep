
test_that("read_spec_yaml() supports stories-only mode", {
  spec <- read_spec_yaml(yaml_file("stories-only-1.yaml"))
  expect_setequal(names(spec), STORIES_ONLY_COLS)
  expect_equal(nrow(spec), 2)
  expect_setequal(spec$TestIds,
                  list(c("TEST-ID-001", "TEST-ID-002"),
                       "TEST-ID-001"))
})

test_that("read_spec_yaml() can read stories from multiple YAMLs", {
  spec <- read_spec_yaml(c(yaml_file("stories-only-1.yaml"),
                           yaml_file("stories-only-2.yaml")))
  expect_setequal(names(spec), STORIES_ONLY_COLS)
  expect_equal(nrow(spec), 3)
})

test_that("read_spec_yaml() supports requirements", {
  spec <- read_spec_yaml(
    stories = yaml_file("stories-1.yaml"),
    requirements = c(yaml_file("requirements-1.yaml"),
                     yaml_file("requirements-2.yaml")))

  expect_setequal(names(spec), STORIES_AND_REQS_COLS)
  expect_equal(nrow(spec), 4)
  expect_setequal(spec$TestIds,
                  list(c("TEST-ID-001", "TEST-ID-002"),
                       "TEST-ID-001",
                       "TEST-ID-001",
                       "TEST-ID-003"))
})

test_that("read_spec_yaml() aborts on repeated IDs", {
  # With just stories, the story ID can't be repeated.
  expect_error(
    read_spec_yaml(c(yaml_file("stories-only-1.yaml"),
                     yaml_file("stories-only-1.yaml"))),
    class = "mrgvalprep_input_error")

  # With stories and requirements, it's the requirement ID that can't be
  # repeated with requirements spec
  expect_error(
    read_spec_yaml(
      stories = yaml_file("stories-1.yaml"),
      requirements = c(yaml_file("requirements-1.yaml"),
                       yaml_file("requirements-1.yaml"))),
    class = "mrgvalprep_input_error")
})
