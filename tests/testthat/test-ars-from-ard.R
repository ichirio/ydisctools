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
  # provenance the ARD cannot carry -> blank + REVIEW note
  expect_true(all(an$population == ""))
  expect_true(any(grepl("no population or subset conditions", rec$notes)))
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
