# Tests for ars_params_from_ard() -- drafting compact parameters from an
# existing ARD (issue #32's fallback route when the code cannot be read).

# a small flattened ARD in the cards layout: per-arm N (.by_stats idiom),
# a continuous Age summary and a categorical Sex tabulation
.mini_ard <- function() {
  rbind(
    data.frame(group1 = NA, group1_level = NA, variable = "TRT01A",
               variable_level = c("Placebo", "Active"),
               context = "tabulate", stat_name = "n", stat = c(10, 10)),
    data.frame(group1 = "TRT01A",
               group1_level = rep(c("Placebo", "Active"), 2),
               variable = "AGE", variable_level = NA, context = "summary",
               stat_name = rep(c("mean", "sd"), each = 2),
               stat = c(70.1, 71.2, 8.1, 7.9)),
    data.frame(group1 = "TRT01A",
               group1_level = rep(c("Placebo", "Active"), 2),
               variable = "SEX", variable_level = c("F", "F", "M", "M"),
               context = "tabulate", stat_name = rep(c("n", "p"), 2),
               stat = c(6, 5, 0.6, 0.5))
  )
}

test_that("a cards-style ARD drafts total_n / continuous / categorical", {
  rec <- ars_params_from_ard(.mini_ard(), output_id = "Out_dm",
                             dataset = "ADSL")
  an <- rec$analyses
  expect_equal(an$method,
               c("total_n", "continuous_summary", "categorical_summary"))
  # the ungrouped tabulation of the grouping variable became the big N
  expect_equal(an$variable, c("USUBJID", "AGE", "USUBJID"))
  expect_equal(an$group_by, c("TRT01A", "TRT01A", "TRT01A, SEX"))
  expect_equal(an$denominator, c("", "", "auto"))
  expect_true(all(an$dataset == "ADSL"))
  # the ARD was computed from the data as-is -> output-level "ALL" default
  # (analysis rows stay blank and inherit it) + ASSUMED note (issue #44)
  expect_true(all(an$population == ""))
  expect_equal(rec$outputs$population, "ALL")
  expect_true(any(grepl("`population` was set to 'ALL'", rec$notes,
                        fixed = TRUE)))
  # observed levels of the first grouping -> pre-defined groups (ASSUMED)
  expect_equal(rec$outputs$groups, "Placebo | Active")
  expect_true(any(grepl("observed levels", rec$notes)))
})

test_that("the result round-trips through write_ars_params and build_ars", {
  rec <- ars_params_from_ard(.mini_ard(), output_id = "Out_dm",
                             dataset = "ADSL")
  rec$analyses$population <- "SAFFL"
  rec$outputs$population <- "SAFFL"
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  write_ars_params(rec, tmp, overwrite = TRUE)
  ars <- build_ars(tmp)
  expect_equal(nrow(ars$Analyses), 3)
  expect_true("Mth_categorical_summary" %in% ars$Analyses$method_id)
})

test_that("a siera-stamped combined ARD splits per OutputId and maps idioms", {
  d <- system.file("sap-pipeline", package = "ydisctools")
  skip_if(identical(d, ""), "sap-pipeline folder not installed")

  rec <- ars_params_from_ard(file.path(d, "05_ard", "ARD_STUDY01.csv"))
  an <- rec$analyses
  expect_length(unique(rec$outputs$output_id), 10)
  # AnalysisId-keyed units: Out_dm = big N + Age + 4 categoricals
  dm <- an[an$output_id == "Out_dm", ]
  expect_equal(dm$method,
               c("total_n", "continuous_summary",
                 rep("categorical_summary", 4)))
  # the '.flag_' flat idiom maps to a per-group subject count
  ov <- an[an$output_id == "Out_ae_ov", ]
  expect_equal(ov$method, c("total_n", rep("categorical_summary", 3)))
  expect_true(all(ov$variable == "USUBJID"))
  # arm levels recovered as groups on every output
  expect_true(all(grepl("Placebo", rec$outputs$groups, fixed = TRUE)))
})

test_that("input validation: bad inputs error clearly", {
  expect_error(ars_params_from_ard(42), "data frame")
  expect_error(ars_params_from_ard(data.frame(x = 1)), "missing required")
  expect_error(ars_params_from_ard("no/such/file.csv"), "not found")
})

# --- issue #46: hierarchical ARDs (cards sentinels must never leak) --------

# a flattened ard_stack_hierarchical(variables = c(AEBODSYS, AEDECOD),
# by = TR01AG1, over_variables = TRUE, denominator = adsl) shape
.hier_ard <- function(by2 = FALSE) {
  g2 <- function(d) {
    if (by2) {
      # a second by-variable rides only the grouped (hierarchical) rows -
      # cards keeps the denominator tabulations ungrouped
      hier <- d$context == "hierarchical"
      d$group2 <- ifelse(hier, "SEROSTAT", NA)
      d$group2_level <- ifelse(hier, "POS", NA)
      d[, c("group1", "group1_level", "group2", "group2_level",
            "group3", "group3_level", "variable", "variable_level",
            "context", "stat_name", "stat")]
    } else {
      d
    }
  }
  base <- rbind(
    # big N (cards derives it from denominator=)
    data.frame(group1 = NA, group1_level = NA, group3 = NA,
               group3_level = NA, variable = "TRT01A",
               variable_level = c("Placebo", "Active"),
               context = "tabulate", stat_name = "n", stat = c(10, 10)),
    # any-event overall rows
    data.frame(group1 = "TRT01A", group1_level = c("Placebo", "Active"),
               group3 = NA, group3_level = NA,
               variable = "..ard_hierarchical_overall..",
               variable_level = TRUE, context = "hierarchical",
               stat_name = "n", stat = c(6, 7)),
    # by-SOC rows
    data.frame(group1 = "TRT01A", group1_level = c("Placebo", "Active"),
               group3 = NA, group3_level = NA, variable = "AEBODSYS",
               variable_level = "Cardiac", context = "hierarchical",
               stat_name = "n", stat = c(3, 4)),
    # by-PT rows (hierarchy parent AEBODSYS as a grouping)
    data.frame(group1 = "TRT01A", group1_level = c("Placebo", "Active"),
               group3 = "AEBODSYS", group3_level = "Cardiac",
               variable = "AEDECOD", variable_level = "Palpitations",
               context = "hierarchical", stat_name = "n", stat = c(2, 2))
  )
  g2(base)
}

test_that("a hierarchical ARD maps sentinels + parents away and builds (#46)", {
  rec <- ars_params_from_ard(.hier_ard(), output_id = "Out_ae",
                             dataset = "ADAE")
  an <- rec$analyses
  # no cards sentinel anywhere in the draft
  expect_false(any(grepl("..ard_", c(an$variable, an$group_by, an$name),
                         fixed = TRUE)))
  # the over_variables rows became the any-event analysis
  expect_true("Subjects with at least one event, n (%)" %in% an$name)
  expect_equal(an$group_by[an$name ==
                             "Subjects with at least one event, n (%)"],
               "TRT01A")
  # the by-PT analysis dropped its hierarchy parent (flattened)
  expect_equal(an$group_by[grepl("AEDECOD", an$name)], "TRT01A, AEDECOD")
  expect_true(any(grepl("FLAT per-level analyses", rec$notes)))
  # ... and the draft builds with no manual edit
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_warning(write_ars_params(rec, tmp, overwrite = TRUE))
  ars <- build_ars(tmp)
  expect_equal(nrow(ars$Analyses), 4)
})

test_that("3+ groupings get a LIMITATION note and a write-time warning (#46)", {
  rec <- ars_params_from_ard(.hier_ard(by2 = TRUE), output_id = "Out_ae",
                             dataset = "ADAE")
  # two real by-vars + the category = a faithful 3-grouping group_by
  expect_true(any(grepl("^TRT01A, SEROSTAT, ", rec$analyses$group_by)))
  expect_true(any(grepl("3+ simultaneous groupings", rec$notes,
                        fixed = TRUE)))
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  expect_warning(write_ars_params(rec, tmp, overwrite = TRUE),
                 "at most 2 are supported")
})

test_that("an unknown cards sentinel is skipped with a note, not emitted", {
  ard <- .mini_ard()
  ard$variable[ard$variable == "SEX"] <- "..ard_mystery.."
  rec <- ars_params_from_ard(ard, output_id = "Out_dm", dataset = "ADSL")
  expect_false(any(grepl("..ard_mystery..", rec$analyses$variable,
                         fixed = TRUE)))
  expect_true(any(grepl("cards internal rows", rec$notes)))
})
