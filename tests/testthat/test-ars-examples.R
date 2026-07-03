# Tests for the shipped end-to-end example set in inst/ars-examples/.

.examples_dir <- function() {
  d <- system.file("ars-examples", package = "ydisctools")
  skip_if(identical(d, ""), "ars-examples folder not installed")
  d
}

test_that("the example set ships all artefacts of the chain", {
  d <- .examples_dir()
  expect_true(file.exists(file.path(d, "params_dm_ae.xlsx")))
  expect_true(file.exists(file.path(d, "ARS_dm_ae.xlsx")))
  expect_true(file.exists(file.path(d, "ARD_Out_dm.R")))
  expect_true(file.exists(file.path(d, "ARD_Out_ae.R")))
  expect_true(file.exists(file.path(d, "adam", "ADSL.csv")))
  expect_true(file.exists(file.path(d, "adam", "ADAE.csv")))
  # the generated programmes must reference the ADaM data portably
  expect_true(any(grepl("adam/ADSL.csv", readLines(file.path(d, "ARD_Out_dm.R")),
                        fixed = TRUE)))
})

test_that("the shipped ARS workbook is in sync with the shipped parameters", {
  d <- .examples_dir()
  regen <- build_ars(file.path(d, "params_dm_ae.xlsx"))
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  write_ars_xlsx(regen, tmp, overwrite = TRUE)

  shipped_sheets <- readxl::excel_sheets(file.path(d, "ARS_dm_ae.xlsx"))
  expect_setequal(shipped_sheets, names(regen))

  # Compare both workbooks through the same reader; About carries build
  # provenance (package version) and is excluded.
  for (sheet in setdiff(names(regen), "About")) {
    expect_equal(
      as.data.frame(readxl::read_excel(tmp, sheet = sheet)),
      as.data.frame(readxl::read_excel(file.path(d, "ARS_dm_ae.xlsx"),
                                       sheet = sheet)),
      info = sheet
    )
  }
})

test_that("the shipped ARD programmes run and match the shipped ADaM data", {
  skip_if_not_installed("cards")
  skip_on_cran()

  d <- .examples_dir()
  withr::local_dir(d)   # scripts read 'adam/*.csv' relative to the folder

  adsl <- read.csv("adam/ADSL.csv", stringsAsFactors = FALSE)
  saf <- adsl[adsl$SAFFL == "Y", ]
  truth_bign <- vapply(split(saf$USUBJID, saf$TRT01A), length, numeric(1))

  ard <- run_ard_script("ARD_Out_dm.R")
  expect_setequal(unique(ard$AnalysisId),
                  c("An_01", "An_02", "An_03", "An_04", "An_05"))
  n01 <- ard[ard$AnalysisId == "An_01" & ard$stat_name == "n", ]
  got <- vapply(n01$stat, as.numeric, numeric(1))
  names(got) <- n01$variable_level
  expect_equal(got[names(truth_bign)], truth_bign)

  ard2 <- run_ard_script("ARD_Out_ae.R")
  expect_setequal(unique(ard2$AnalysisId),
                  c("An_06", "An_07", "An_08", "An_09", "An_10"))
  # any-TEAE distinct-subject counts against an independent computation
  adae <- read.csv("adam/ADAE.csv", stringsAsFactors = FALSE)
  m <- merge(adae[adae$TRTEMFL == "Y", ],
             adsl[adsl$SAFFL == "Y", c("USUBJID", "TRT01A")], by = "USUBJID")
  truth_any <- vapply(split(m$USUBJID, m$TRT01A),
                      function(x) length(unique(x)), numeric(1))
  a07 <- ard2[ard2$AnalysisId == "An_07" & ard2$stat_name == "n", ]
  got_any <- vapply(a07$stat, as.numeric, numeric(1))
  names(got_any) <- a07$group1_level
  expect_equal(got_any[names(truth_any)], truth_any)
})
