# Tests for issue #37: population = "ALL" (no analysis-set filter),
# denominator ordering, and the required-fields error message.

.all_params <- function(pop = "ALL", dataset = "DF") {
  analyses <- data.frame(
    output_id = "Out_ard",
    analysis_id = sprintf("An_%02d", 1:4),
    name = c("Number of subjects", "AGE (continuous)", "AGEGR1, n (%)",
             "SEX, n (%)"),
    method = c("total_n", "continuous_summary", "categorical_summary",
               "categorical_summary"),
    dataset = dataset,
    variable = c("USUBJID", "AGE", "USUBJID", "USUBJID"),
    population = pop,
    group_by = c("TR01AG1", "TR01AG1", "TR01AG1, AGEGR1", "TR01AG1, SEX"),
    groups = "", groups2 = "", where = "",
    denominator = c("", "", "auto", "auto"),
    stringsAsFactors = FALSE
  )
  outputs <- data.frame(output_id = "Out_ard", name = "Recovered display",
                        population = pop, group_by = "TR01AG1", groups = "",
                        stringsAsFactors = FALSE)
  list(outputs = outputs, analyses = analyses)
}

test_that("population = 'ALL' builds an ARS without an analysis set", {
  ars <- build_ars(.all_params())
  expect_equal(nrow(ars$AnalysisSets), 0)
  expect_true(all(ars$Analyses$analysisSetId == ""))
  # denominator wiring still resolves within the ALL population
  expect_equal(ars$Analyses$referencedAnalysisOperations_analysisId2[3:4],
               c("An_01", "An_01"))
})

test_that("an 'ALL' output spanning two datasets is rejected", {
  p <- .all_params()
  p$analyses$dataset <- c("ADSL", "ADSL", "ADAE", "ADAE")
  expect_error(build_ars(p), "same dataset")
})

test_that("'ALL' and a flag population can coexist across outputs", {
  p1 <- .all_params()
  p2 <- .all_params(pop = "SAFFL", dataset = "ADSL")
  p2$outputs$output_id <- "Out_flag"
  p2$analyses$output_id <- "Out_flag"
  p2$analyses$analysis_id <- sprintf("An_%02d", 5:8)
  p <- list(outputs = rbind(p1$outputs, p2$outputs),
            analyses = rbind(p1$analyses, p2$analyses))
  ars <- build_ars(p)
  expect_equal(nrow(ars$AnalysisSets), 1)
  expect_equal(ars$AnalysisSets$condition_variable, "SAFFL")
  expect_equal(unique(ars$Analyses$analysisSetId[1:4]), "")
  expect_equal(unique(ars$Analyses$analysisSetId[5:8]), "AnalysisSet_01")
})

test_that("the required-fields error names only the missing column", {
  p <- .all_params()
  p$analyses$population <- ""
  p$outputs$population <- ""
  err <- tryCatch(build_ars(p), error = conditionMessage)
  expect_match(err, "`population` is required")
  expect_no_match(err, "`group_by`")
  expect_match(err, "ALL", fixed = TRUE)  # points at the escape hatch
})

test_that("a denominator listed after its referent is rejected with a hint", {
  p <- .all_params()
  # move total_n to the end (the order cards ARDs naturally produce)
  p$analyses <- p$analyses[c(2, 3, 4, 1), ]
  err <- tryCatch(build_ars(p), error = conditionMessage)
  expect_match(err, "comes AFTER it")
  expect_match(err, "ars_params_from_ard", fixed = TRUE)
})

test_that("mock shells label an ALL population as All Subjects", {
  toc <- data.frame(
    toc_no = NA_character_, output_id = "Out_dm", title = NA_character_,
    display_type = "dm_summary", section_key = NA_character_,
    population = "ALL", group_by = "TRT01A", groups = NA_character_,
    where = NA_character_, stringsAsFactors = FALSE
  )
  skip_if_not_installed("openxlsx")
  mock <- withr::local_tempfile(fileext = ".xlsx")
  ars_mock(list(toc = toc), mock, overwrite = TRUE)
  titles <- suppressMessages(readxl::read_excel(mock, sheet = "Out_dm",
                                                col_names = FALSE))
  expect_true("All Subjects" %in% titles[[1]])
})

test_that("issue-37 end to end: recovered ALL draft generates a running ARD", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_on_cran()

  # dummy pre-filtered analysis data (the programme read it as `DF`)
  set.seed(37)
  df <- data.frame(
    USUBJID = sprintf("S%02d", 1:30),
    TR01AG1 = rep(c("Placebo", "Low", "High"), each = 10),
    AGE = sample(50:80, 30, TRUE),
    AGEGR1 = sample(c("<65", ">=65"), 30, TRUE),
    SEX = sample(c("F", "M"), 30, TRUE),
    stringsAsFactors = FALSE
  )
  adam <- withr::local_tempdir()
  write.csv(df, file.path(adam, "DF.csv"), row.names = FALSE)

  # siera warns "using the analysis dataset without filtering" - the
  # documented fallback behaviour for a blank analysisSetId
  expect_warning(
    ard <- ars_generate_ard(build_ars(.all_params()), adam_path = adam),
    "without filtering"
  )
  expect_equal(unique(ard$OutputId), "Out_ard")
  # per-group percentages against an independent computation
  n_f <- ard[ard$AnalysisId == "An_04" & ard$stat_name == "n" &
               ard$variable_level == "F", ]
  truth <- table(df$TR01AG1[df$SEX == "F"])
  got <- vapply(n_f$stat, as.numeric, numeric(1))
  names(got) <- n_f$group1_level
  expect_equal(got[names(truth)], c(truth)[names(truth)],
               ignore_attr = TRUE)
})

# --- issue #44: the recovery routes must emit "ALL" themselves -------------
# (#37 taught build_ars() to ACCEPT "ALL" but left the recovery functions
# emitting a blank, so their own drafts still could not build when the data
# were pre-filtered upstream - the most common real-world shape)

# the reported pattern: data pre-filtered upstream, no *FL filter in the code
.prefiltered_src <- function(envir = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".R", .local_envir = envir)
  writeLines('
    library(cards); library(dplyr)
    df <- DF
    ard <- df %>%
      ard_stack(
        ard_summary(variables = AGE),
        ard_tabulate(variables = c(AGEGR1, SEX)),
        .by = TR01AG1, .total_n = TRUE)', f)
  f
}

test_that("a resolved pipeline without a filter recovers population 'ALL' (#44)", {
  rec <- ars_params_from_code(.prefiltered_src(), output_id = "out_dm")
  expect_true(all(rec$analyses$population == "ALL"))
  expect_true(any(grepl("no analysis-set filter in the code", rec$notes)))
  # the draft builds with NO manual edit - the pipeline promised by the docs
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_warning(write_ars_params(rec, tmp, overwrite = TRUE))
  ars <- build_ars(tmp)
  expect_equal(nrow(ars$AnalysisSets), 0)
})

test_that("the issue-44 recover -> write -> build pipeline needs no edit", {
  skip_if_not_installed("cards")
  set.seed(44)
  DF <- data.frame(
    USUBJID = sprintf("S%02d", 1:30),
    TR01AG1 = rep(c("Placebo", "Low", "High"), each = 10),
    AGE = sample(50:80, 30, TRUE),
    AGEGR1 = sample(c("<65", ">=65"), 30, TRUE),
    SEX = sample(c("F", "M"), 30, TRUE),
    stringsAsFactors = FALSE
  )
  ard <- cards::ard_stack(
    DF,
    cards::ard_summary(variables = AGE),
    cards::ard_tabulate(variables = c(AGEGR1, SEX)),
    .by = TR01AG1, .total_n = TRUE
  )
  rec <- ars_params_recover(.prefiltered_src(), ard, output_id = "out_dm")
  expect_true(all(rec$analyses$population == "ALL"))
  expect_true(all(rec$analyses$dataset == "DF"))
  # the code side resolved the dataset, so the UNKNOWN alarm must be gone
  expect_false(any(grepl("'UNKNOWN' on every analysis", rec$notes)))
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_warning(write_ars_params(rec, tmp, overwrite = TRUE))
  ars <- build_ars(tmp)
  expect_equal(nrow(ars$AnalysisSets), 0)
  expect_true(all(ars$Analyses$analysisSetId == ""))
})

test_that("an unconvertible condition keeps population blank + write warns", {
  src <- withr::local_tempfile(fileext = ".R")
  writeLines('
    library(cards); library(dplyr)
    adsl <- ADSL %>% filter(strange_fn(USUBJID))
    ard <- adsl %>%
      ard_stack(
        ard_summary(variables = AGE),
        ard_tabulate(variables = c(AGEGR1, SEX)),
        .by = TRT01A, .total_n = TRUE)', src)
  rec <- ars_params_from_code(src, output_id = "out_dm")
  # "no filter found" is NOT certain here: a condition failed to convert
  expect_true(all(rec$analyses$population == ""))
  expect_true(any(grepl("set `population` manually", rec$notes)))
  # ... and the gap is announced when the draft is written, not at build time
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  expect_warning(write_ars_params(rec, tmp, overwrite = TRUE),
                 "will not build as-is")
})
