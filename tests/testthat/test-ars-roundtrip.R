# Round-trip loop tests (issue #39):
#
#   code0 (human-style) -> params1 -> ARS1 -> programmes1 (+ ARD1, executed)
#   programmes1         -> params2 -> ARS2 -> programmes2 (+ ARD2, executed)
#   programmes2         -> params3
#
# The generated programmes are ultimately finished and maintained by people,
# so the loop must hold ON the generated code: recovering parameters from it
# must converge in one step (params2 == params1), stay stable (params3 ==
# params2, ARS2 == ARS1), and the executed results must match the human
# programme's own ARD.

.rt_cols <- c("method", "dataset", "variable", "population", "group_by",
              "groups", "where", "denominator")

# the human-style study programme: variable lists, %>%, population and
# pre-defined arm levels in a filter() pipeline
.rt_human_src <- function(envir = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".R", .local_envir = envir)
  writeLines('
    library(cards); library(dplyr)

    arm_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
    vars_cont  <- c("AGE")
    vars_cat   <- c("AGEGR1", "SEX")

    adsl <- ADSL %>%
      filter(TRT01A %in% arm_levels, SAFFL == "Y")

    ard <- adsl %>%
      ard_stack(
        ard_summary(variables = all_of(vars_cont)),
        ard_tabulate(variables = all_of(vars_cat)),
        .by = TRT01A,
        .total_n = TRUE
      )', f)
  f
}

# read the STUDY01 dummy ADSL the same way the generated programmes do
.rt_adam <- function() {
  d <- system.file("sap-pipeline", "adam", package = "ydisctools")
  skip_if(identical(d, ""), "sap-pipeline adam data not installed")
  d
}

# flatten an executed ARD to comparable (key, value) rows
.rt_stats <- function(ard) {
  d <- as.data.frame(ard)
  keep <- d$stat_name %in% c("n", "p", "N", "mean", "sd", "median")
  d <- d[keep, ]
  key <- paste(
    ifelse(is.na(d$variable), "", as.character(d$variable)),
    if ("group1_level" %in% names(d)) {
      ifelse(is.na(d$group1_level), "", as.character(d$group1_level))
    } else "",
    ifelse(is.na(d$variable_level), "", as.character(d$variable_level)),
    d$stat_name, sep = "|")
  val <- vapply(d$stat, function(v) {
    suppressWarnings(as.numeric(if (is.list(v)) v[[1]] else v))
  }, numeric(1))
  out <- tapply(val, key, function(x) x[1])   # de-dup identical rows
  out[order(names(out))]
}

test_that("the code -> params -> ARS -> code loop converges and holds", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_on_cran()

  adam <- .rt_adam()
  src0 <- .rt_human_src()

  # ---- iteration 1: human code -> params -> ARS -> programmes -> ARD ------
  params1 <- ars_params_from_code(src0, output_id = "Out_dm",
                                  output_name = "Summary of Demographic Data")
  ars1 <- build_ars(params1)
  prog1 <- withr::local_tempdir()
  ard1 <- ars_generate_ard(ars1, adam_path = adam, output_path = prog1)

  # ---- iteration 2: generated code -> params -> ARS -> programmes -> ARD --
  gen1 <- list.files(prog1, pattern = "[.]R$", full.names = TRUE)
  expect_length(gen1, 1)
  params2 <- ars_params_from_code(gen1, output_id = "Out_dm",
                                  output_name = "Summary of Demographic Data")

  # convergence in ONE step: every semantic column identical, incl. the
  # pre-defined group levels (recovered from the groupId stamping) and the
  # population (recovered from the generated df_pop filter)
  expect_equal(params2$analyses[, .rt_cols], params1$analyses[, .rt_cols],
               ignore_attr = TRUE)

  ars2 <- build_ars(params2)
  # the rebuilt ARS is the same metadata, sheet by sheet (About carries
  # build provenance and is excluded)
  for (sheet in setdiff(names(ars1), "About")) {
    expect_equal(ars2[[sheet]], ars1[[sheet]], info = sheet)
  }

  prog2 <- withr::local_tempdir()
  ard2 <- ars_generate_ard(ars2, adam_path = adam, output_path = prog2)

  # ---- iteration 3: idempotence on the regenerated code -------------------
  gen2 <- list.files(prog2, pattern = "[.]R$", full.names = TRUE)
  params3 <- ars_params_from_code(gen2, output_id = "Out_dm",
                                  output_name = "Summary of Demographic Data")
  expect_equal(params3$analyses[, .rt_cols], params2$analyses[, .rt_cols],
               ignore_attr = TRUE)

  # ---- semantic equivalence: both generated ARDs agree --------------------
  expect_equal(.rt_stats(ard2), .rt_stats(ard1), tolerance = 1e-8)

  # ---- ground truth: the human programme's own ARD agrees -----------------
  # run the human code against the same data (ADSL loaded like the
  # generated programmes load it)
  suppressWarnings({   # "built under R x.y.z" noise on older local R
    withr::local_package("dplyr")
    withr::local_package("cards")
  })
  env <- new.env(parent = globalenv())
  env$ADSL <- utils::read.csv(file.path(adam, "ADSL.csv"),
                              stringsAsFactors = FALSE)
  code <- gsub("^\\s*library\\(", "# library(", readLines(src0))
  tf <- withr::local_tempfile(fileext = ".R")
  writeLines(code, tf)
  source(tf, local = env)
  ard0 <- env$ard

  s0 <- .rt_stats(ard0)
  s1 <- .rt_stats(ard1)
  # compare on the keys the human ARD carries (it has no siera stamping,
  # and cards emits a few extra stats the ARS methods do not)
  common <- intersect(names(s0), names(s1))
  expect_gt(length(common), 20)
  expect_equal(s1[common], s0[common], tolerance = 1e-8)
})

test_that("params recovered from the executed ARD agree with the code's", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_on_cran()

  adam <- .rt_adam()
  src0 <- .rt_human_src()
  params1 <- ars_params_from_code(src0, output_id = "Out_dm")
  ard1 <- ars_generate_ard(build_ars(params1), adam_path = adam)

  rec <- ars_params_from_ard(ard1, output_id = "Out_dm")
  a_code <- params1$analyses
  a_ard <- rec$analyses
  # same analyses, same order, same shape (provenance apart)
  expect_equal(a_ard$method, a_code$method)
  expect_equal(a_ard$variable, a_code$variable)
  expect_equal(a_ard$group_by, a_code$group_by)
  expect_equal(a_ard$denominator, a_code$denominator)
})
