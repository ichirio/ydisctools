# Tests for ars_params_from_code() / write_ars_params() and the display
# metadata support in build_ars().

.write_src <- function(code, envir = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".R", .local_envir = envir)
  writeLines(code, f)
  f
}

# -- recovery from the shipped siera-generated ARD programmes -----------------

test_that("parameters are recovered from siera-generated ARD programmes", {
  ex <- system.file("ars-examples", package = "ydisctools")
  skip_if(identical(ex, ""), "ars-examples folder not installed")

  rec <- ars_params_from_code(
    file.path(ex, c("ARD_Out_dm.R", "ARD_Out_ae.R")),
    output_id = c("Out_dm", "Out_ae"),
    output_name = c("Summary of Demographic Data", "Summary of TEAE")
  )
  an <- rec$analyses
  expect_equal(rec$outputs$output_id, c("Out_dm", "Out_ae"))
  # analysis ids re-numbered across files, no duplicates
  expect_false(anyDuplicated(an$analysis_id) > 0)

  dm <- an[an$output_id == "Out_dm", ]
  expect_equal(dm$method,
               c("total_n", "continuous_summary", rep("categorical_summary", 5)))
  expect_equal(dm$group_by[3:7],
               c("TRT01A, AGEGR1", "TRT01A, AGEGR2", "TRT01A, SEX",
                 "TRT01A, RACE", "TRT01A, ETHNIC"))
  expect_true(all(dm$population == "SAFFL"))
  expect_true(all(dm$denominator[3:7] == "auto"))

  ae <- an[an$output_id == "Out_ae", ]
  expect_true(all(ae$where[ae$method == "categorical_summary"] == "TRTEMFL EQ Y"))
  expect_true(any(grepl("REVIEW: a dataset is built by merge", rec$notes)))
})

# -- cards / cardx call handling ----------------------------------------------

test_that("ard_stack with .by / .total_n and group-level filters convert", {
  src <- .write_src('
    library(cards); library(dplyr)
    arm_levels <- c("Placebo", "Active")
    adsl <- pharmaverseadam::adsl |>
      filter(TRT01A %in% arm_levels, SAFFL == "Y")
    ard <- ard_stack(data = adsl, .by = TRT01A,
      ard_continuous(variables = AGE),
      ard_categorical(variables = c(AGEGR, SEX)), .total_n = TRUE)')
  rec <- ars_params_from_code(src)
  an <- rec$analyses
  expect_equal(an$method,
               c("total_n", "continuous_summary", "categorical_summary",
                 "categorical_summary"))
  # the %in% filter on the grouping variable becomes the group levels
  expect_true(all(an$groups == "Placebo | Active"))
  expect_true(all(an$population == "SAFFL"))
  expect_equal(an$group_by[3], "TRT01A, AGEGR")
})

test_that("ard_stack_hierarchical flattens with notes; over_variables = any-event", {
  src <- .write_src('
    library(cards); library(dplyr)
    adsl <- pharmaverseadam::adsl |> filter(SAFFL == "Y")
    adae <- pharmaverseadam::adae |> filter(TRTEMFL == "Y")
    ard <- ard_stack_hierarchical(data = adae, variables = c(AESOC, AEDECOD),
                                  by = TRT01A, denominator = adsl, id = USUBJID)
    ard_any <- ard_stack_hierarchical(data = adae, variables = c(AESOC, AEDECOD),
                                      by = TRT01A, denominator = adsl,
                                      id = USUBJID, over_variables = TRUE)')
  rec <- ars_params_from_code(src)
  an <- rec$analyses
  expect_setequal(an$group_by,
                  c("TRT01A, AESOC", "TRT01A, AEDECOD", "TRT01A"))
  expect_true("total_n" %in% an$method)
  expect_true(any(grepl("^LIMITATION: ard_stack_hierarchical", rec$notes)))
  # any-event row
  expect_true(any(an$name == "Subjects with at least one event, n (%)"))
  # where captured from the adae pipeline
  expect_true(all(an$where[an$dataset == "ADAE"] == "TRTEMFL EQ Y"))
})

test_that("cardx test functions map onto catalog methods", {
  src <- .write_src('
    library(cardx)
    adsl <- cards::ADSL |> dplyr::filter(SAFFL == "Y")
    ard_stats_fisher_test(adsl, by = ARM, variables = AGEGR1)
    ard_stats_aov(adsl, by = ARM, variables = AGE)
    ard_stats_prop_test(adsl, by = ARM, variables = EFFFL)
    ard_stats_t_test(adsl, by = ARM, variables = AGE)')
  rec <- ars_params_from_code(src)
  expect_equal(rec$analyses$method,
               c("fishers_exact", "anova", "risk_difference"))
  expect_true(all(rec$analyses$population == "SAFFL"))
  expect_true(any(grepl("Unsupported ARD function skipped: ard_stats_t_test",
                        rec$notes)))
})

test_that("a file without cards calls yields notes, not an error", {
  src <- .write_src('x <- 1 + 1')
  rec <- ars_params_from_code(src)
  expect_null(rec$analyses)
  expect_true(any(grepl("No convertible cards/cardx calls", rec$notes)))
  expect_error(ars_params_from_code("no/such/file.R"), "not found")
})

# -- display furniture ---------------------------------------------------------

test_that("rtfreporter furniture is captured and classified", {
  src <- .write_src('
    library(cards)
    ae_col_header <- c("System Organ Class /\nPreferred Term",
                       paste0(arms, "\nN = 99"))
    hdr <- rtf_header(rows = list(
      c(l = "Acme Biopharma, Inc.", r = "Page {AUTO_PAGE}"),
      c(c = "Table 14.3.1"),
      c(c = "Treatment-Emergent Adverse Events"),
      c(c = "")))
    ftr <- rtf_footer(rows = list(
      c(l = "Note: counted once."),
      c(l = "Program: t_ae.R", r = "Generated: 2026-06-17")))
    ard <- ard_tabulate(cards::ADSL, by = "ARM", variables = "AGEGR1")')
  rec <- ars_params_from_code(src, output_id = "Out_x")
  d <- rec$displays
  expect_equal(d$text[d$section_type == "Title"],
               c("Table 14.3.1", "Treatment-Emergent Adverse Events"))
  expect_equal(sum(d$section_type == "Header"), 2)   # l/r row -> Header
  expect_equal(d$text[d$section_type == "Footnote"], "Note: counted once.")
  expect_equal(sum(d$section_type == "Footer"), 2)
  expect_equal(d$text[d$section_type == "Rowlabel Header"],
               "System Organ Class / Preferred Term")
  expect_true(any(grepl("ASSUMED: rtf_header", rec$notes)))
})

test_that("titles=/footnotes= arguments are captured", {
  src <- .write_src('
    ard <- cards::ard_tabulate(cards::ADSL, by = "ARM", variables = "AGEGR1")
    doc <- rtf_tables(doc, pages,
                      titles = list(c("Table 1", "Demographics")),
                      footnotes = list("Note: safety population."))')
  rec <- ars_params_from_code(src)
  d <- rec$displays
  expect_equal(d$text[d$section_type == "Title"], c("Table 1", "Demographics"))
  expect_equal(d$text[d$section_type == "Footnote"],
               "Note: safety population.")
})

# -- write_ars_params / read_ars_params round trip ------------------------------

test_that("write_ars_params round-trips through read_ars_params/build_ars", {
  ex <- system.file("ars-examples", package = "ydisctools")
  skip_if(identical(ex, ""), "ars-examples folder not installed")

  rec <- ars_params_from_code(file.path(ex, "ARD_Out_dm.R"),
                              output_id = "Out_dm",
                              output_name = "Summary of Demographic Data")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  write_ars_params(rec, tmp, overwrite = TRUE)
  expect_error(write_ars_params(rec, tmp), "already exists")
  expect_true(all(c("Outputs", "Analyses", "Notes") %in%
                    readxl::excel_sheets(tmp)))

  ars <- build_ars(tmp)
  expect_equal(nrow(ars$Analyses), 7)
  expect_true("Outputs" %in% names(ars))

  expect_error(write_ars_params(list(analyses = data.frame()), tmp),
               "`outputs` and `analyses`")
})

# -- build_ars display sheets ----------------------------------------------------

test_that("build_ars emits Outputs always and Displays when present", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  ars_param_template(tmp, overwrite = TRUE)
  ars <- build_ars(tmp)

  expect_equal(ars$Outputs$id, c("Out_demog", "Out_teae"))
  expect_equal(ars$Outputs$display1_Id, c("Disp_Out_demog", NA))
  d <- ars$Displays
  expect_true(all(d$id == "Disp_Out_demog"))
  # displayTitle = last Title subsection
  expect_true(all(d$displayTitle == "Summary of Demographic Data"))
  expect_equal(d$displaySection_subSection_id[1], "Disp_Out_demog_Header_01")
  expect_true("GlobalDisplaySections" %in% names(ars))

  # without a Displays sheet: Outputs only
  params <- read_ars_params(tmp)
  params$displays <- NULL
  ars2 <- build_ars(params)
  expect_true("Outputs" %in% names(ars2))
  expect_false("Displays" %in% names(ars2))
})

test_that("build_ars validates display metadata", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  ars_param_template(tmp, overwrite = TRUE)
  params <- read_ars_params(tmp)

  p <- params; p$displays$section_type[1] <- "Banner"
  expect_error(build_ars(p), "section_type")
  p <- params; p$displays$output_id[1] <- "Out_nope"
  expect_error(build_ars(p), "not present in `outputs`")
  p <- params; p$displays$text <- NULL
  expect_error(build_ars(p), "missing column")
})

# -- full loop -------------------------------------------------------------------

test_that("recovered parameters build an ARS that siera runs end-to-end", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_on_cran()

  ex <- system.file("ars-examples", package = "ydisctools")
  skip_if(identical(ex, ""), "ars-examples folder not installed")

  rec <- ars_params_from_code(file.path(ex, "ARD_Out_dm.R"),
                              output_id = "Out_dm_rec",
                              output_name = "Summary of Demographic Data")
  wd <- withr::local_tempdir()
  write_ars_xlsx(build_ars(list(outputs = rec$outputs,
                                analyses = rec$analyses)),
                 file.path(wd, "ARS_rec.xlsx"))
  out_dir <- file.path(wd, "scripts"); dir.create(out_dir)
  siera::readARS(file.path(wd, "ARS_rec.xlsx"), output_path = out_dir,
                 adam_path = file.path(ex, "adam"))

  withr::local_dir(ex)   # generated read_csv paths point into ex/adam
  ard <- run_ard_script(file.path(out_dir, "ARD_Out_dm_rec.R"))
  n01 <- ard[ard$AnalysisId == "An_01" & ard$stat_name == "n", ]
  adsl <- read.csv(file.path(ex, "adam", "ADSL.csv"), stringsAsFactors = FALSE)
  saf <- adsl[adsl$SAFFL == "Y", ]
  truth <- vapply(split(saf$USUBJID, saf$TRT01A), length, numeric(1))
  got <- vapply(n01$stat, as.numeric, numeric(1))
  names(got) <- n01$variable_level
  expect_equal(got[names(truth)], truth)
})
