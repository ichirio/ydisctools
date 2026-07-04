# Tests for ars_generate_ard() (siera-driven combined-ARD runner).

test_that("ars_generate_ard validates its inputs", {
  skip_if_not_installed("siera")
  expect_error(ars_generate_ard("no/such/ars.xlsx", tempdir()), "not found")
  expect_error(ars_generate_ard(42, tempdir()), "must be an ARS file path")
  ex <- system.file("ars-examples", package = "ydisctools")
  skip_if(identical(ex, ""), "ars-examples folder not installed")
  expect_error(
    ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"), "no/such/adam"),
    "`adam_path` folder not found"
  )
})

test_that("ars_generate_ard runs the programmes and combines the ARDs", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_on_cran()

  ex <- system.file("ars-examples", package = "ydisctools")
  skip_if(identical(ex, ""), "ars-examples folder not installed")
  adam <- file.path(ex, "adam")

  out_dir <- withr::local_tempdir()
  ard <- ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"), adam_path = adam,
                          output_path = out_dir)
  # combined: both outputs present, keyed by OutputId
  expect_s3_class(ard, "data.frame")
  expect_setequal(unique(ard$OutputId), c("Out_dm", "Out_ae"))
  expect_setequal(
    unique(ard$AnalysisId),
    sprintf("An_%02d", 1:10)
  )
  # generated programmes are kept and reported
  scripts <- attr(ard, "scripts")
  expect_setequal(basename(scripts), c("ARD_Out_dm.R", "ARD_Out_ae.R"))
  expect_true(all(file.exists(scripts)))

  # per-output list mode and equivalence with the combined frame
  ards <- ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"), adam_path = adam,
                           output_path = withr::local_tempdir(),
                           combine = FALSE)
  expect_named(ards, c("Out_ae", "Out_dm"), ignore.order = TRUE)
  expect_equal(sum(vapply(ards, nrow, integer(1))), nrow(ard))

  # spec_output restricts the run to one output
  ard_dm <- ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"), adam_path = adam,
                             output_path = withr::local_tempdir(),
                             spec_output = "Out_dm")
  expect_equal(unique(ard_dm$OutputId), "Out_dm")

  # build_ars() sheet list accepted directly
  ard2 <- ars_generate_ard(build_ars(file.path(ex, "params_dm_ae.xlsx")),
                           adam_path = adam,
                           output_path = withr::local_tempdir())
  expect_equal(nrow(ard2), nrow(ard))
})
