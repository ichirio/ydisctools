# Tests for ars_params_recover() -- merging code-based and ARD-based
# parameter recovery (issue #35).

# programme whose variable lists live in ANOTHER file (unresolvable), but
# whose provenance (dataset, population, arm levels) is fully readable
.recover_src <- function(envir = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".R", .local_envir = envir)
  writeLines('
    library(cards); library(dplyr)
    arm_levels <- c("Placebo", "Active")
    adsl <- ADSL %>% filter(TRT01A %in% arm_levels, SAFFL == "Y")
    ard <- adsl %>%
      ard_stack(
        ard_summary(variables = all_of(vars_cont)),
        ard_tabulate(variables = all_of(vars_cat)),
        .by = TRT01A, .total_n = TRUE)', f)
  f
}

# the flattened ARD that programme produced (cards layout): per-arm big N
# (ungrouped tabulation + ..ard_total_n..), AGE/HTBL summaries, SEX n (%)
.recover_ard <- function() {
  rbind(
    data.frame(group1 = NA, group1_level = NA, variable = "TRT01A",
               variable_level = c("Placebo", "Active"),
               context = "tabulate", stat_name = "n", stat = c(10, 10)),
    data.frame(group1 = NA, group1_level = NA,
               variable = "..ard_total_n..", variable_level = NA,
               context = "total_n", stat_name = "N", stat = 20),
    data.frame(group1 = "TRT01A",
               group1_level = rep(c("Placebo", "Active"), 4),
               variable = rep(c("AGE", "HTBL"), each = 4),
               variable_level = NA, context = "summary",
               stat_name = rep(rep(c("mean", "sd"), each = 2), 2),
               stat = c(70.1, 71.2, 8.1, 7.9, 165.3, 166.0, 8.8, 9.1)),
    data.frame(group1 = "TRT01A",
               group1_level = rep(c("Placebo", "Active"), 2),
               variable = "SEX", variable_level = c("F", "F", "M", "M"),
               context = "tabulate", stat_name = rep(c("n", "p"), 2),
               stat = c(6, 5, 0.6, 0.5))
  )
}

test_that("merging code and ARD beats either source alone", {
  src <- .recover_src()
  rec <- ars_params_recover(src, .recover_ard(), output_id = "Out_dm")
  an <- rec$analyses

  # the ARD decided WHICH analyses exist: real variables, no all_of() junk,
  # and the two total_n idiom rows collapsed into one
  expect_setequal(an$variable[an$method == "continuous_summary"],
                  c("AGE", "HTBL"))
  expect_equal(sum(an$method == "total_n"), 1)
  expect_false(any(grepl("all_of|vars_cont|vars_cat", an$variable)))

  # the code supplied the provenance an ARD cannot carry
  expect_true(all(an$dataset == "ADSL"))
  expect_true(all(an$population == "SAFFL"))
  expect_equal(rec$outputs$population, "SAFFL")
  expect_equal(rec$outputs$groups, "Placebo | Active")

  # both discrepancy directions are reported
  expect_true(any(grepl("found in the ARD but not in the programme code",
                        rec$notes)))
  expect_true(any(grepl("no counterpart in the ARD", rec$notes)))
})

test_that("the merged draft builds into an ARS", {
  src <- .recover_src()
  rec <- ars_params_recover(src, .recover_ard(), output_id = "Out_dm")
  ars <- build_ars(rec)
  expect_gte(nrow(ars$Analyses), 4)
  expect_equal(ars$AnalysisSets$condition_variable, "SAFFL")
})

test_that("single-source calls delegate to the matching extractor", {
  src <- .recover_src()
  ard <- .recover_ard()
  expect_equal(ars_params_recover(paths = src, output_id = "Out_dm"),
               ars_params_from_code(src, output_id = "Out_dm"))
  expect_equal(ars_params_recover(ard = ard, output_id = "Out_dm"),
               ars_params_from_ard(ard, output_id = "Out_dm"))
  expect_error(ars_params_recover(), "Supply")
  expect_error(ars_params_recover(c(src, src), ard), "one programme per")
})

test_that("a fully disjoint code/ARD pairing errors, not drafts (#46)", {
  # the dm programme from .recover_src() vs an AE-shaped ARD: nothing can
  # match - this is the mismatched-`ard`-object mistake, say so loudly
  ae_ard <- data.frame(
    group1 = "TRT01A", group1_level = rep(c("Placebo", "Active"), 2),
    variable = "AEBODSYS",
    variable_level = rep(c("Cardiac", "Gastro"), each = 2),
    context = "hierarchical", stat_name = "n", stat = c(3, 4, 2, 5)
  )
  expect_error(ars_params_recover(.recover_src(), ae_ard,
                                  output_id = "Out_dm"),
               "DIFFERENT displays")
})
