# Tests for issue #40: real-world patterns -- cardx::ard_categorical_ci()
# (response-rate CIs) end to end via the new proportion_ci method + options
# column, multi-`by` tabulations, and external denominators.

test_that("snippet 1: imap-wrapped ard_categorical_ci recovers with options", {
  src <- withr::local_tempfile(fileext = ".R")
  writeLines('
    library(cards); library(cardx); library(dplyr); library(purrr)
    adrs_filtered <- ADRS %>% filter(ITTFL == "Y")
    ard <- imap(group_defs, function(values, grpname) {
      imap(values, function(cond, valname) {
        adrs_filtered %>%
          filter(!!parse_expr(cond)) %>%
          ard_categorical_ci(
            variables = "AVALCAT1",
            by = "TR01AG1",
            value = list(AVALCAT1 = "Response"),
            method = "clopper-pearson",
            conf.level = 0.90
          )
      })
    }) %>% list_flatten() %>% bind_ard() %>% unlist_ard_columns()', src)

  rec <- ars_params_from_code(src, output_id = "Out_rsp")
  an <- rec$analyses
  expect_equal(an$method, "proportion_ci")
  expect_equal(an$variable, "AVALCAT1")
  expect_equal(an$group_by, "TR01AG1")
  expect_equal(an$population, "ITTFL")
  expect_equal(an$options,
               "method=clopper-pearson; conf.level=0.9; value=Response")
  # the dynamic subgroup condition stays a human decision
  expect_true(any(grepl("parse_expr", rec$notes)))
  expect_true(any(grepl("MAPPED: ard_categorical_ci", rec$notes)))
})

test_that("snippet 3: multi-by and external denominators are flagged", {
  src <- withr::local_tempfile(fileext = ".R")
  writeLines('
    library(cards); library(dplyr)
    ard <- df %>%
      ard_tabulate(
        variables = WORSTGR,
        by = c(LBTOX_LBL, BASEGR),
        denominator = n_subj,
        statistic = everything() ~ c("n", "p")
      ) %>%
      unlist_ard_columns()', src)

  rec <- ars_params_from_code(src, output_id = "Out_lb")
  # the faithful 3-grouping group_by is kept, with the #6 LIMITATION note
  expect_equal(rec$analyses$group_by, "LBTOX_LBL, BASEGR, WORSTGR")
  expect_true(any(grepl("LIMITATION: ard_tabulate\\(by = c\\(", rec$notes)))
  expect_true(any(grepl("#6", rec$notes)))
  # external denominator dataset -> explicit REVIEW note
  expect_true(any(grepl("external `denominator = n_subj`", rec$notes)))
})

.ci_params <- function(options =
                         "method=clopper-pearson; conf.level=0.9; value=Response") {
  analyses <- data.frame(
    output_id = "Out_rsp",
    analysis_id = sprintf("An_%02d", 1:3),
    name = c("Number of subjects", "Best response, n (%)",
             "Response rate CI"),
    method = c("total_n", "categorical_summary", "proportion_ci"),
    dataset = "ADRS",
    variable = c("USUBJID", "USUBJID", "AVALCAT1"),
    population = "ADRS.ITTFL",
    group_by = c("TR01AG1", "TR01AG1, AVALCAT1", "TR01AG1"),
    groups = "", groups2 = "", where = "",
    denominator = c("", "auto", ""),
    options = c("", "", options),
    stringsAsFactors = FALSE
  )
  outputs <- data.frame(output_id = "Out_rsp",
                        name = "Analysis of Best Response",
                        population = "ADRS.ITTFL", group_by = "TR01AG1",
                        groups = "", stringsAsFactors = FALSE)
  list(outputs = outputs, analyses = analyses)
}

test_that("build_ars bakes CI options into a synthesized method entry", {
  ars <- build_ars(.ci_params())
  expect_true("Mth_proportion_ci_clopper_pearson_90_Response" %in%
                ars$Analyses$method_id)
  tpl <- ars$AnalysisMethodCodeTemplate
  tc <- tpl$templateCode[
    tpl$method_id == "Mth_proportion_ci_clopper_pearson_90_Response"]
  expect_match(tc, "method = 'clopper-pearson'", fixed = TRUE)
  expect_match(tc, "conf.level = 0.9", fixed = TRUE)
  expect_match(tc, "value = list(anavarhere = 'Response')", fixed = TRUE)
  # the dataset-qualified bare-flag population parsed correctly
  expect_equal(ars$AnalysisSets$condition_dataset, "ADRS")
  expect_equal(ars$AnalysisSets$condition_variable, "ITTFL")

  # defaults: no options -> clopper-pearson 95%, no value argument
  ars2 <- build_ars(.ci_params(options = ""))
  expect_true("Mth_proportion_ci_clopper_pearson_95" %in%
                ars2$Analyses$method_id)
  tc2 <- ars2$AnalysisMethodCodeTemplate$templateCode[
    ars2$AnalysisMethodCodeTemplate$method_id ==
      "Mth_proportion_ci_clopper_pearson_95"]
  expect_match(tc2, "conf.level = 0.95", fixed = TRUE)
  expect_no_match(tc2, "value = list", fixed = TRUE)
})

test_that("two CI flavours can coexist; bad options error clearly", {
  p <- .ci_params()
  extra <- p$analyses[3, ]
  extra$analysis_id <- "An_04"
  extra$options <- "method=wilson; conf.level=0.95"
  p$analyses <- rbind(p$analyses, extra)
  ars <- build_ars(p)
  expect_setequal(
    grep("^Mth_proportion_ci", unique(ars$Analyses$method_id), value = TRUE),
    c("Mth_proportion_ci_clopper_pearson_90_Response",
      "Mth_proportion_ci_wilson_95"))

  p <- .ci_params(options = "conf.level=1.5")
  expect_error(build_ars(p), "conf.level must be a number")
  p <- .ci_params(options = "foo=bar")
  expect_error(build_ars(p), "unknown `options` key")
  p <- .ci_params()
  p$analyses$options[1] <- "conf.level=0.9"   # options on a total_n
  expect_error(build_ars(p), "only supported for method 'proportion_ci'")
})

test_that("proportion_ci runs end to end and the loop converges (#40)", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom")   # cardx::ard_categorical_ci() needs it
  skip_on_cran()

  set.seed(40)
  adrs <- data.frame(
    USUBJID = sprintf("S%02d", 1:40), ITTFL = "Y",
    TR01AG1 = rep(c("Placebo", "Active"), each = 20),
    AVALCAT1 = sample(c("Response", "Non-response"), 40, TRUE),
    stringsAsFactors = FALSE
  )
  adrs$ITTFL[c(3, 25)] <- "N"
  adam <- withr::local_tempdir()
  write.csv(adrs, file.path(adam, "ADRS.csv"), row.names = FALSE)

  prog <- withr::local_tempdir()
  ard <- ars_generate_ard(build_ars(.ci_params()), adam_path = adam,
                          output_path = prog)

  # CI values match an independent cardx computation
  ci <- as.data.frame(ard[ard$AnalysisId == "An_03", ])
  direct <- cardx::ard_categorical_ci(
    adrs[adrs$ITTFL == "Y", ], variables = "AVALCAT1", by = "TR01AG1",
    value = list(AVALCAT1 = "Response"),
    method = "clopper-pearson", conf.level = 0.9)
  d <- as.data.frame(cards::unlist_ard_columns(direct))
  for (statn in c("estimate", "conf.low", "conf.high")) {
    truth <- as.numeric(d$stat[d$stat_name == statn])
    names(truth) <- d$group1_level[d$stat_name == statn]
    g <- ci[ci$stat_name == statn, ]
    got <- vapply(g$stat, as.numeric, numeric(1))
    names(got) <- g$group1_level
    expect_equal(got[names(truth)], truth, tolerance = 1e-8,
                 ignore_attr = TRUE)
  }

  # loop: the generated programme recovers the same analysis incl. options
  rec <- ars_params_from_code(list.files(prog, full.names = TRUE),
                              output_id = "Out_rsp")
  pci <- rec$analyses[rec$analyses$method == "proportion_ci", ]
  expect_equal(pci$variable, "AVALCAT1")
  expect_equal(pci$options,
               "method=clopper-pearson; conf.level=0.9; value=Response")

  # and the executed ARD recovers method + conf.level onto options
  rec2 <- ars_params_from_ard(ard, dataset = "ADRS")
  pci2 <- rec2$analyses[rec2$analyses$method == "proportion_ci", ]
  expect_equal(pci2$options, "method=clopper-pearson; conf.level=0.9")
})
