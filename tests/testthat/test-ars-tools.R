# Tests for the ARS generator (ars_param_template / read_ars_params /
# build_ars / write_ars_xlsx).

# -- condition mini-syntax ----------------------------------------------------

test_that("bare flag conditions expand to <dataset>.<flag> EQ Y", {
  c1 <- ydisctools:::.ars_parse_condition("SAFFL", default_dataset = "ADSL")
  expect_equal(c1, list(dataset = "ADSL", variable = "SAFFL",
                        comparator = "EQ", value = "Y"))
})

test_that("full conditions parse dataset, comparator and value", {
  c1 <- ydisctools:::.ars_parse_condition("ADSL.AGE GE 65",
                                          default_dataset = "ADAE")
  expect_equal(c1$dataset, "ADSL")
  expect_equal(c1$variable, "AGE")
  expect_equal(c1$comparator, "GE")
  expect_equal(c1$value, "65")

  c2 <- ydisctools:::.ars_parse_condition("TRTEMFL EQ Y",
                                          default_dataset = "ADAE")
  expect_equal(c2$dataset, "ADAE")

  c3 <- ydisctools:::.ars_parse_condition("SEX IN M | F",
                                          default_dataset = "ADSL")
  expect_equal(c3$comparator, "IN")
  expect_equal(c3$value, "M|F")

  c4 <- ydisctools:::.ars_parse_condition("PARAM CONTAINS Weeks 1 to 12",
                                          default_dataset = "ADVS")
  expect_equal(c4$value, "Weeks 1 to 12")
})

test_that("unparsable conditions error clearly", {
  expect_error(
    ydisctools:::.ars_parse_condition("AGE ?? 65", default_dataset = "ADSL"),
    "Cannot parse ARS condition"
  )
  expect_error(
    ydisctools:::.ars_parse_condition_chain(" ; ", default_dataset = "ADSL"),
    "Empty ARS condition"
  )
})

test_that("condition chains split on ';' into one row per condition", {
  df <- ydisctools:::.ars_parse_condition_chain(
    "TRTEMFL EQ Y; AOCC01FL EQ Y", default_dataset = "ADAE"
  )
  expect_equal(nrow(df), 2)
  expect_equal(df$variable, c("TRTEMFL", "AOCC01FL"))
  expect_equal(df$dataset, c("ADAE", "ADAE"))
})

test_that("group_by specs parse variables with optional dataset", {
  gb <- ydisctools:::.ars_parse_group_by("TRT01A, ADAE.AEBODSYS")
  expect_equal(gb$dataset, c("ADSL", "ADAE"))
  expect_equal(gb$variable, c("TRT01A", "AEBODSYS"))
  expect_error(ydisctools:::.ars_parse_group_by("A B"),
               "Cannot parse group_by")
})

test_that("group level specs split on '|' and blank means data-driven", {
  expect_equal(ydisctools:::.ars_parse_groups("Placebo | Active"),
               c("Placebo", "Active"))
  expect_null(ydisctools:::.ars_parse_groups(""))
  expect_null(ydisctools:::.ars_parse_groups(NA_character_))
})

# -- parameter template round trip -------------------------------------------

test_that("ars_param_template writes a workbook read_ars_params reads back", {
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  ars_param_template(tmp, overwrite = TRUE)
  expect_error(ars_param_template(tmp), "already exists")

  params <- read_ars_params(tmp)
  expect_named(params, c("study", "outputs", "analyses", "displays"))
  expect_true(all(c("output_id", "name") %in% names(params$outputs)))
  expect_true(all(c("output_id", "method", "dataset", "variable") %in%
                    names(params$analyses)))
  # blank cells come back as NA
  expect_true(anyNA(params$analyses$where))
})

test_that("read_ars_params validates the file and sheets", {
  expect_error(read_ars_params(file.path(tempdir(), "no_such_file.xlsx")),
               "not found")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Foo = data.frame(a = 1)), tmp)
  expect_error(read_ars_params(tmp), "missing required sheet")
})

# -- build_ars ----------------------------------------------------------------

# Compact params used across the build_ars tests (mirrors the shipped
# template, deterministic).
.demo_params <- function() {
  tmp <- file.path(withr::local_tempdir(.local_envir = parent.frame()),
                   "params.xlsx")
  ars_param_template(tmp, overwrite = TRUE)
  read_ars_params(tmp)
}

test_that("build_ars produces every sheet siera::readARS() requires", {
  ars <- build_ars(.demo_params())
  required <- c("MainListOfContents", "OtherListsOfContents", "DataSubsets",
                "AnalysisSets", "AnalysisGroupings", "Analyses",
                "AnalysisMethods", "AnalysisMethodCodeTemplate",
                "AnalysisMethodCodeParameters")
  expect_true(all(required %in% names(ars)))
})

test_that("build_ars de-duplicates analysis sets, groupings and subsets", {
  ars <- build_ars(.demo_params())
  # one shared Safety population
  expect_equal(nrow(ars$AnalysisSets), 1)
  expect_equal(ars$AnalysisSets$name, "Safety Population")
  expect_equal(unique(ars$Analyses$analysisSetId), "AnalysisSet_01")
  # groupings: predefined TRT01A (demog), data-driven AGEGR1, AGEGR2, SEX,
  # RACE, ETHNIC, TRT01A (teae), AEBODSYS, AESEV
  expect_equal(nrow(unique(ars$AnalysisGroupings["id"])), 9)
  pre <- ars$AnalysisGroupings[ars$AnalysisGroupings$dataDriven == "FALSE", ]
  expect_equal(pre$group_condition_value, c("Placebo", "Active"))
  # the two TEAE analyses share one data subset
  expect_equal(nrow(ars$DataSubsets), 1)
  expect_equal(ars$DataSubsets$condition_dataset, "ADAE")
  teae <- ars$Analyses[ars$Analyses$id %in% c("An_09", "An_10"), ]
  expect_equal(unique(teae$dataSubsetId), "Dss_01")
})

test_that("build_ars wires numerator/denominator analyses", {
  ars <- build_ars(.demo_params())
  an <- ars$Analyses
  # auto resolution in the demog output
  expect_equal(
    an$referencedAnalysisOperations_analysisId2[an$id %in% c("An_03", "An_04")],
    c("An_01", "An_01")
  )
  # numerator references the analysis itself
  expect_equal(
    an$referencedAnalysisOperations_analysisId1[an$id == "An_03"], "An_03"
  )
  # explicit denominator in the TEAE output
  expect_equal(
    an$referencedAnalysisOperations_analysisId2[an$id == "An_09"], "An_08"
  )
  # total_n rows carry no reference
  expect_true(is.na(an$referencedAnalysisOperations_analysisId2[an$id == "An_01"]))
})

test_that("build_ars lays out the contents lists in siera's fill-down shape", {
  ars <- build_ars(.demo_params())
  main <- ars$MainListOfContents
  # output rows carry outputId and no analysisId; analysis rows the reverse
  out_rows <- main[!is.na(main$listItem_outputId), ]
  ana_rows <- main[!is.na(main$listItem_analysisId), ]
  expect_equal(out_rows$listItem_outputId, c("Out_demog", "Out_teae"))
  expect_true(all(is.na(out_rows$listItem_analysisId)))
  expect_equal(nrow(ana_rows), 10)
  expect_equal(nrow(ars$OtherListsOfContents), 2)
})

test_that("build_ars expands methods from the vendored catalog", {
  ars <- build_ars(.demo_params())
  expect_setequal(unique(ars$AnalysisMethods$id),
                  c("Mth_total_n", "Mth_continuous_summary",
                    "Mth_categorical_summary"))
  # continuous summary has 8 operations (N..max)
  cont <- ars$AnalysisMethods[ars$AnalysisMethods$id == "Mth_continuous_summary", ]
  expect_equal(nrow(cont), 8)
  tpl <- ars$AnalysisMethodCodeTemplate
  expect_equal(nrow(tpl), 3)
  expect_false(any(grepl("\r", tpl$templateCode, fixed = TRUE)))
  expect_true(all(tpl$specifiedAs == "Code"))
  # the ydisctools overlay replaces the legacy dummy-variable categorical
  # template with the modern by=/variables= pattern
  cat_tpl <- tpl$templateCode[tpl$method_id == "Mth_categorical_summary"]
  expect_false(grepl("dummy", cat_tpl, fixed = TRUE))
  expect_true(grepl("byvarshere", cat_tpl, fixed = TRUE) ||
                grepl("variables", cat_tpl, fixed = TRUE))
  pars <- ars$AnalysisMethodCodeParameters
  expect_true(all(nzchar(pars$parameter_valueSource)))
})

test_that("build_ars validates its inputs", {
  base <- .demo_params()

  p <- base; p$analyses$method[1] <- "no_such_method"
  expect_error(build_ars(p), "unknown method")

  p <- base; p$outputs <- rbind(p$outputs, p$outputs[1, ])
  expect_error(build_ars(p), "Duplicated `output_id`")

  p <- base; p$analyses <- p$analyses[-(2:6), ]   # demog drops to 2 -> too few
  expect_error(build_ars(p), "at least 3 analyses per output")

  p <- base; p$analyses$analysis_id[1] <- "An 01"
  expect_error(build_ars(p), "letters, digits and underscores")

  p <- base; p$analyses$output_id[1] <- "Out_missing"
  expect_error(build_ars(p), "not present in `outputs`")

  p <- base; p$analyses$denominator[3] <- "An_99"
  expect_error(build_ars(p), "unknown analysis_id")

  p <- base; p$analyses$population[2] <- "ITTFL"
  expect_error(build_ars(p), "mixes different populations")

  p <- base; p$analyses$group_by[2] <- "TRT01A, AGEGR1, SEX"
  expect_error(build_ars(p), "at most 2")

  p <- base; p$outputs$population <- NA; p$analyses$population <- NA
  expect_error(build_ars(p), "`population` and `group_by` are required")

  p <- base; p$analyses$population[] <- "SAFFL; ITTFL"
  expect_error(build_ars(p), "single condition")

  p <- base
  p$analyses$denominator[p$analyses$analysis_id == "An_09"] <- "auto"
  # auto works here too (same population + grouping1 as An_08)
  ars <- build_ars(p)
  expect_equal(
    ars$Analyses$referencedAnalysisOperations_analysisId2[
      ars$Analyses$id == "An_09"], "An_08"
  )
})

# -- write_ars_xlsx -----------------------------------------------------------

test_that("write_ars_xlsx writes a workbook with all sheets", {
  ars <- build_ars(.demo_params())
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  write_ars_xlsx(ars, tmp, overwrite = TRUE)
  expect_error(write_ars_xlsx(ars, tmp), "already exists")
  expect_true(all(names(ars) %in% readxl::excel_sheets(tmp)))

  # templateCode survives the xlsx round trip with newlines intact
  tpl <- readxl::read_excel(tmp, sheet = "AnalysisMethodCodeTemplate")
  expect_true(any(grepl("\n", tpl$templateCode, fixed = TRUE)))

  expect_error(write_ars_xlsx(list(Analyses = data.frame()), tmp),
               "missing sheet")
  expect_error(write_ars_xlsx("not a list", tmp), "named list")
})

# -- integration: siera::readARS() runs on the generated workbook -------------

test_that("siera::readARS() generates runnable ARD programmes from our ARS", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_on_cran()

  wd <- withr::local_tempdir()

  # deterministic mini ADaM data
  adsl <- data.frame(
    STUDYID = "STUDY01",
    USUBJID = sprintf("SUBJ-%03d", 1:8),
    SAFFL   = c("Y", "Y", "Y", "Y", "Y", "Y", "Y", "N"),
    TRT01A  = c(rep("Placebo", 4), rep("Active", 4)),
    AGE     = c(50, 60, 70, 55, 65, 75, 45, 80),
    AGEGR1  = c("<65", "<65", ">=65", "<65", ">=65", ">=65", "<65", ">=65"),
    AGEGR2  = c("<75", "<75", "<75", "<75", "<75", ">=75", "<75", ">=75"),
    SEX     = c("F", "M", "F", "M", "F", "M", "F", "M"),
    RACE    = c("WHITE", "WHITE", "ASIAN", "WHITE",
                "WHITE", "ASIAN", "WHITE", "WHITE"),
    ETHNIC  = c("NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO",
                "HISPANIC OR LATINO", "NOT HISPANIC OR LATINO",
                "NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO",
                "HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    STUDYID  = "STUDY01",
    USUBJID  = c("SUBJ-001", "SUBJ-002", "SUBJ-003", "SUBJ-005"),
    TRTEMFL  = c("Y", "Y", "N", "Y"),
    AEBODSYS = c("CARDIAC DISORDERS", "NERVOUS SYSTEM DISORDERS",
                 "CARDIAC DISORDERS", "CARDIAC DISORDERS"),
    AESEV    = c("MILD", "SEVERE", "MILD", "MODERATE"),
    stringsAsFactors = FALSE
  )
  write.csv(adsl, file.path(wd, "ADSL.csv"), row.names = FALSE)
  write.csv(adae, file.path(wd, "ADAE.csv"), row.names = FALSE)

  pfile <- file.path(wd, "params.xlsx")
  ars_param_template(pfile, overwrite = TRUE)
  afile <- file.path(wd, "ARS_STUDY01.xlsx")
  write_ars_xlsx(build_ars(pfile), afile)

  out_dir <- file.path(wd, "scripts")
  dir.create(out_dir)
  siera::readARS(afile, output_path = out_dir, adam_path = wd)
  scripts <- list.files(out_dir, full.names = TRUE)
  expect_setequal(basename(scripts), c("ARD_Out_demog.R", "ARD_Out_teae.R"))

  run_script <- run_ard_script   # helper-ars.R

  # demographics output (N, Age, AGEGR1, AGEGR2, SEX, RACE, ETHNIC)
  ard <- run_script(scripts[basename(scripts) == "ARD_Out_demog.R"])
  expect_setequal(unique(ard$AnalysisId), sprintf("An_%02d", 1:7))
  # An_01 total N per arm (safety only): Placebo 4, Active 3
  n01 <- ard[ard$AnalysisId == "An_01" & ard$stat_name == "n", ]
  stats01 <- vapply(n01$stat, as.numeric, numeric(1))
  expect_setequal(stats01, c(4, 3))
  # An_03 age-group percentages use An_01 as denominator:
  # Placebo <65 -> 3/4.  With the overlay template the category variable
  # lands in variable / variable_level (no dummy variable).
  a03 <- ard[ard$AnalysisId == "An_03" &
               ard$group1_level == "Placebo" &
               ard$variable_level == "<65", ]
  expect_equal(unique(a03$variable), "AGEGR1")
  expect_equal(as.numeric(a03$stat[a03$stat_name == "n"][[1]]), 3)
  expect_equal(as.numeric(a03$stat[a03$stat_name == "p"][[1]]), 0.75)
  expect_false(any(ard$variable == "dummy", na.rm = TRUE))
  # predefined 1st grouping stamps groupingId + groupId
  expect_true(all(a03$group1_groupingId == "AnlsGrp_01_TRT01A"))
  expect_true(all(a03$group1_groupId == "AnlsGrp_01_TRT01A_01"))

  # TEAE output
  ard2 <- run_script(scripts[basename(scripts) == "ARD_Out_teae.R"])
  expect_setequal(unique(ard2$AnalysisId), c("An_08", "An_09", "An_10"))
  # An_08 denominator N per arm: Placebo 4, Active 3 (full safety population,
  # not only AE subjects)
  n08 <- ard2[ard2$AnalysisId == "An_08" & ard2$stat_name == "n", ]
  expect_setequal(vapply(n08$stat, as.numeric, numeric(1)), c(4, 3))
  # An_09: TEAE by SOC, TRTEMFL == Y applied ->
  # (Placebo, CARDIAC) = SUBJ-001 only: n = 1, p = 1/4
  a09 <- ard2[ard2$AnalysisId == "An_09" &
                ard2$group1_level == "Placebo" &
                ard2$variable_level == "CARDIAC DISORDERS", ]
  expect_equal(unique(a09$variable), "AEBODSYS")
  expect_equal(as.numeric(a09$stat[a09$stat_name == "n"][[1]]), 1)
  expect_equal(as.numeric(a09$stat[a09$stat_name == "p"][[1]]), 0.25)
  # data-driven 1st grouping stamps groupValue, not groupId
  expect_equal(unique(a09$group1_groupValue), "Placebo")
})
