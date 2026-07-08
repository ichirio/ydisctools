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
    sprintf("An_%02d", 1:12)
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

test_that("the combined ARD keeps canonical column order (group1 first) (#55)", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_on_cran()
  ex <- system.file("ars-examples", package = "ydisctools")
  skip_if(identical(ex, ""), "ars-examples folder not installed")
  adam <- file.path(ex, "adam")

  ard <- suppressWarnings(suppressMessages(
    ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"), adam_path = adam,
                     output_path = withr::local_tempdir())))
  nm <- names(ard)
  # siera's per-analysis dplyr::bind_rows() (total_n first, no grouping cols)
  # used to push group1* to the back; tidy_ard_column_order() restores it
  expect_equal(nm[1:2], c("group1", "group1_level"))
  expect_lt(match("group1", nm), match("variable", nm))
  expect_lt(match("variable", nm), match("stat_name", nm))

  # per-output list mode is reordered too
  ards <- suppressWarnings(suppressMessages(
    ars_generate_ard(file.path(ex, "ARS_dm_ae.xlsx"), adam_path = adam,
                     output_path = withr::local_tempdir(), combine = FALSE)))
  expect_true(all(vapply(ards, function(a) names(a)[1] == "group1",
                         logical(1))))
})

test_that("combining two outputs with identical stats drops no rows (#55)", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_on_cran()
  adam <- withr::local_tempdir()
  adsl <- as.data.frame(cards::ADSL[, c("USUBJID", "ARM", "AGE", "SEX",
                                        "SAFFL")])
  utils::write.csv(adsl, file.path(adam, "ADSL.csv"), row.names = FALSE,
                   na = "")
  mk <- function(oid) {
    list(
      outputs = data.frame(output_id = oid, name = oid, population = "SAFFL",
                           group_by = "ARM", groups = "",
                           stringsAsFactors = FALSE),
      analyses = data.frame(
        output_id = oid, analysis_id = paste0(oid, sprintf("_An_%02d", 1:3)),
        name = c("N", "AGE", "SEX"),
        method = c("total_n", "continuous_summary", "categorical_summary"),
        dataset = "ADSL", variable = c("USUBJID", "AGE", "USUBJID"),
        population = "SAFFL", group_by = c("ARM", "ARM", "ARM, SEX"),
        groups = "", groups2 = "", where = "",
        denominator = c("", "", "auto"), options = "",
        stringsAsFactors = FALSE))
  }
  a <- mk("Out_a"); b <- mk("Out_b")
  p <- list(outputs = rbind(a$outputs, b$outputs),
            analyses = rbind(a$analyses, b$analyses))
  ard <- suppressWarnings(suppressMessages(
    ars_generate_ard(build_ars(p), adam_path = adam,
                     output_path = withr::local_tempdir())))
  # both displays' N-per-ARM stats are identical; bind_ard() would have
  # de-duplicated them across OutputId, dplyr::bind_rows() keeps both
  expect_setequal(unique(ard$OutputId), c("Out_a", "Out_b"))
  expect_equal(as.integer(table(ard$OutputId)[["Out_a"]]),
               as.integer(table(ard$OutputId)[["Out_b"]]))
  expect_equal(names(ard)[1], "group1")
})
