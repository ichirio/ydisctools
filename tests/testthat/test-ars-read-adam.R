# read_adam() format priority + ars_generate_ard() xpt / generate-only
# (issue #50: siera 0.5.6 hardcodes readr::read_csv('<DATASET>.csv'); real
# ADaM are .xpt / .sas7bdat / .rds / .rda, and code must generate without
# data present).

.ra_adsl <- function() {
  as.data.frame(cards::ADSL[1:20, c("USUBJID", "ARM", "AGE", "AGEGR1",
                                    "SEX", "SAFFL")])
}

test_that("read_adam picks formats in priority xpt > sas7bdat > rds > rda > csv", {
  skip_if_not_installed("cards")
  skip_if_not_installed("haven")
  d <- withr::local_tempdir()
  base <- .ra_adsl()
  # tag AGE per format so we can tell which file was read
  tag <- function(k) { x <- base; x$AGE <- x$AGE + k; x }
  haven::write_xpt(tag(1000), file.path(d, "ADSL.xpt"))
  suppressWarnings(haven::write_sas(tag(2000), file.path(d, "ADSL.sas7bdat")))
  saveRDS(tag(3000), file.path(d, "ADSL.rds"))
  ADSL_obj <- tag(4000); save(ADSL_obj, file = file.path(d, "ADSL.rda"))
  readr::write_csv(tag(5000), file.path(d, "ADSL.csv"))

  bump <- function() read_adam(file.path(d, "ADSL"))$AGE[1] - base$AGE[1]
  expect_equal(bump(), 1000)                       # xpt wins
  file.remove(file.path(d, "ADSL.xpt"))
  expect_equal(bump(), 2000)                        # then sas7bdat
  file.remove(file.path(d, "ADSL.sas7bdat"))
  expect_equal(bump(), 3000)                        # then rds
  file.remove(file.path(d, "ADSL.rds"))
  expect_equal(bump(), 4000)                        # then rda (object != stem)
  file.remove(file.path(d, "ADSL.rda"))
  expect_equal(bump(), 5000)                        # finally csv
})

test_that("read_adam resolves case-insensitively and errors when absent", {
  skip_if_not_installed("cards")
  d <- withr::local_tempdir()
  readr::write_csv(.ra_adsl(), file.path(d, "ADSL.csv"))
  # lowercase stem finds ADSL.csv; a passed extension is stripped
  expect_s3_class(read_adam(file.path(d, "adsl")), "data.frame")
  expect_s3_class(read_adam(file.path(d, "ADSL.xpt")), "data.frame")
  expect_error(read_adam(file.path(d, "NOPE")), "No ADaM file found")
  expect_error(read_adam(character(0)), "single dataset stem")
})

test_that("the read_csv -> read_adam rewrite keeps the blank-fill pipe", {
  code <- paste(
    "ADSL <- readr::read_csv('adam/ADSL.csv',",
    "                        show_col_types = FALSE,",
    "                        progress = FALSE) |>",
    "  dplyr::mutate(x)", sep = "\n")
  out <- ydisctools:::.ars_csv_to_read_adam(code)
  expect_match(out, "ydisctools::read_adam('adam/ADSL')", fixed = TRUE)
  expect_false(grepl("read_csv", out))
  expect_match(out, "dplyr::mutate(x)", fixed = TRUE)   # tail untouched
})

.ra_ars <- function() {
  p <- list(
    outputs = data.frame(output_id = "Out_dm", name = "DM",
                         population = "SAFFL", group_by = "ARM", groups = "",
                         stringsAsFactors = FALSE),
    analyses = data.frame(
      output_id = "Out_dm", analysis_id = sprintf("An_%02d", 1:3),
      name = c("N", "AGE", "AGEGR1"),
      method = c("total_n", "continuous_summary", "categorical_summary"),
      dataset = "ADSL", variable = c("USUBJID", "AGE", "USUBJID"),
      population = "SAFFL", group_by = c("ARM", "ARM", "ARM, AGEGR1"),
      groups = "", groups2 = "", where = "",
      denominator = c("", "", "auto"), options = "",
      stringsAsFactors = FALSE)
  )
  build_ars(p)
}

test_that("ars_generate_ard(run = FALSE) generates code with NO data present", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_on_cran()
  empty <- withr::local_tempdir()   # deliberately no ADaM files
  scripts <- ars_generate_ard(.ra_ars(), adam_path = empty, run = FALSE)
  expect_true(all(file.exists(scripts)))
  code <- readLines(scripts[1])
  expect_true(any(grepl("ydisctools::read_adam", code)))
  expect_false(any(grepl("readr::read_csv", code)))
})

test_that("a lowercase `dataset` yields ONE read, not adsl + ADSL (#53)", {
  # build_ars must fold the dataset casing across the sheets siera reads:
  # the user's `adsl` vs the "ADSL" a bare-flag population bakes in
  p <- list(
    outputs = data.frame(output_id = "Out_dm", name = "DM",
                         population = "SAFFL", group_by = "ARM", groups = "",
                         stringsAsFactors = FALSE),
    analyses = data.frame(
      output_id = "Out_dm", analysis_id = sprintf("An_%02d", 1:3),
      name = c("N", "AGE", "AGEGR1"),
      method = c("total_n", "continuous_summary", "categorical_summary"),
      dataset = "adsl", variable = c("USUBJID", "AGE", "USUBJID"),
      population = "SAFFL", group_by = c("ARM", "ARM", "ARM, AGEGR1"),
      groups = "", groups2 = "",
      where = c("APHASE EQ P2", "APHASE EQ P2", "APHASE EQ P2"),
      denominator = c("", "", "auto"), options = "",
      stringsAsFactors = FALSE)
  )
  ars <- build_ars(p)
  spellings <- unique(c(
    ars$Analyses$dataset,
    ars$AnalysisSets$condition_dataset,
    ars$DataSubsets$condition_dataset[
      !is.na(ars$DataSubsets$condition_dataset)]))
  spellings <- spellings[nzchar(spellings)]
  expect_equal(spellings, "adsl")               # one spelling, the user's

  # end to end: exactly one read line in the generated programme
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_on_cran()
  scripts <- ars_generate_ard(ars, run = FALSE)
  code <- paste(readLines(scripts[1]), collapse = "\n")
  expect_equal(lengths(regmatches(code, gregexpr("read_adam\\(", code))), 1L)
})

test_that("a genuine cross-dataset output keeps its two reads (#53)", {
  # AE: events on adae, population/denominator on adsl -> two DISTINCT reads
  p <- list(
    outputs = data.frame(output_id = "Out_ae", name = "AE",
                         population = "SAFFL", group_by = "TRTA", groups = "",
                         stringsAsFactors = FALSE),
    analyses = data.frame(
      output_id = "Out_ae", analysis_id = sprintf("An_%02d", 1:3),
      name = c("N", "SOC", "PT"),
      method = c("total_n", "categorical_summary", "categorical_summary"),
      dataset = c("adsl", "adae", "adae"),
      variable = c("USUBJID", "USUBJID", "USUBJID"),
      population = "SAFFL",
      group_by = c("TRTA", "TRTA, AEBODSYS", "TRTA, AEDECOD"),
      groups = "", groups2 = "", where = "",
      denominator = c("", "auto", "auto"), options = "",
      stringsAsFactors = FALSE)
  )
  ars <- build_ars(p)
  expect_setequal(unique(ars$Analyses$dataset), c("adsl", "adae"))
  # the population (bare SAFFL) folds onto the authored adsl, not "ADSL"
  expect_equal(unique(ars$AnalysisSets$condition_dataset), "adsl")
})

test_that("ars_generate_ard reads .xpt and matches the .csv result (#50)", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_on_cran()
  ars <- .ra_ars()
  adsl <- .ra_adsl()

  dcsv <- withr::local_tempdir()
  readr::write_csv(adsl, file.path(dcsv, "ADSL.csv"))
  ard_csv <- suppressWarnings(suppressMessages(
    ars_generate_ard(ars, adam_path = dcsv)))

  dxpt <- withr::local_tempdir()
  haven::write_xpt(adsl, file.path(dxpt, "ADSL.xpt"))
  ard_xpt <- suppressWarnings(suppressMessages(
    ars_generate_ard(ars, adam_path = dxpt)))

  flat <- function(a) {
    d <- as.data.frame(a)
    k <- paste(d$variable, ifelse(is.na(d$variable_level), "",
                                  as.character(d$variable_level)),
               d$stat_name)
    v <- vapply(d$stat, function(x)
      suppressWarnings(as.numeric(if (is.list(x)) x[[1]] else x)), numeric(1))
    o <- tapply(v[!is.na(v)], k[!is.na(v)], `[`, 1)
    o[order(names(o))]
  }
  s_csv <- flat(ard_csv); s_xpt <- flat(ard_xpt)
  common <- intersect(names(s_csv), names(s_xpt))
  expect_gt(length(common), 8)
  expect_equal(s_xpt[common], s_csv[common], tolerance = 1e-8)
})
