# Intended as a mini end-to-end test

skip_if_not_installed("mrgvalidate", minimum_version = "1.0.0")

test_that("Googlesheets end-to-end works", {
  skip_if_over_rate_limit_google()
  googlesheets4::gs4_deauth() # public sheets, so no need to authenticate
  spec <- read_spec_gsheets(
    ss_stories = "1HgsxL4qfYK-wjB-nloMilQiuBSLV6FZq_h2ToB6QNlI",
    ss_req = "1SnyUzxVDUUJFtMGEi2x4zJE0iB2JDV1Np-ivhaUkODk"
  )

  docs_output_dir <- file.path(tempdir(), "mrgvalprep_end_to_end_googlesheets_docs_output")
  if (fs::dir_exists(docs_output_dir)) fs::dir_delete(docs_output_dir)
  fs::dir_create(docs_output_dir)
  on.exit({ fs::dir_delete(docs_output_dir) })

  # test with pre-generated test outputs
  fake_product <- "fake Googlesheets product"
  mrgvalidate::create_validation_docs(
    fake_product,
    "vFake",
    spec,
    auto_test_dir = system.file("test-inputs", "validation-results-sample", package = "mrgvalidate"),
    man_test_dir = system.file("test-inputs", "manual-tests-sample", package = "mrgvalidate"),
    output_dir = docs_output_dir
  )

  check_docs(spec, docs_output_dir)

})

test_that("Github end-to-end works", {
  # get issues from Github
  skip_if_over_rate_limit_github()
  skip_if_no_github_pat()

  spec <- parse_github_issues(org = ORG, repo = REPO, mile = MILESTONE, domain = DOMAIN)

  # run test to generate output
  dir_prefix <- "mrgvalprep_end_to_end_github"
  pkg_dir <- file.path(tempdir(), glue::glue("{dir_prefix}_pkg"))
  if (fs::dir_exists(pkg_dir)) fs::dir_delete(pkg_dir)
  fs::dir_create(pkg_dir)
  on.exit({ fs::dir_delete(pkg_dir) })

  test_output_dir <- file.path(tempdir(), glue::glue("{dir_prefix}_test_output"))
  if (fs::dir_exists(test_output_dir)) fs::dir_delete(test_output_dir)
  fs::dir_create(test_output_dir)
  on.exit({ fs::dir_delete(test_output_dir) })

  docs_output_dir <- file.path(tempdir(), glue::glue("{dir_prefix}_docs_output"))
  if (fs::dir_exists(docs_output_dir)) fs::dir_delete(docs_output_dir)
  fs::dir_create(docs_output_dir)
  on.exit({ fs::dir_delete(docs_output_dir) })

  # run test to generate output
  validate_tests(
    org = ORG,
    repo = REPO,
    version = TAG,
    domain = VALID_DOMAINS,
    out_file = glue::glue("{dir_prefix}_test_res"),
    root_dir = pkg_dir,
    output_dir = test_output_dir,
    set_id_to_name = TRUE
  )

  # build docs
  suppressWarnings( # suppressing warning about `set_id_to_name=T` being legacy functionality
    mrgvalidate::create_validation_docs(
      "fake Github product",
      "vFake",
      spec,
      auto_test_dir = test_output_dir,
      output_dir = docs_output_dir
    )
  )


  check_docs(spec, docs_output_dir, set_id_to_name = TRUE)
})
