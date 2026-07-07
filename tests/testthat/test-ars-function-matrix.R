# Verification matrix (issue #48, after the #37 -> #44 -> #46 sequence):
# the OFFICIAL cards / cardx reference examples (the pkgdown reference
# pages render exactly these Rd examples; sample data cards::ADSL /
# cards::ADAE) are pushed through the whole chain
#
#   code -> ars_params_from_code()/ars_params_recover() ->
#   write_ars_params() -> build_ars() -> siera programme -> executed ARD
#
# and the executed ARD is compared per variable / per statistic against a
# DIRECT run of the same official code on the same data.

.fm_stat_names <- c("n", "N", "p", "mean", "sd", "median", "p25", "p75",
                    "min", "max", "estimate", "conf.low", "conf.high",
                    "p.value")

# flatten an ARD to a named numeric vector keyed by
# variable|group1_level|group2_level|variable_level|stat_name
.fm_stats <- function(ard, drop_group2 = FALSE) {
  d <- as.data.frame(ard)
  d <- d[d$stat_name %in% .fm_stat_names, , drop = FALSE]
  # a flattened per-level analysis has no hierarchy parent: fold the
  # direct hierarchical ARD the same way before comparing
  if (drop_group2 && "group2_level" %in% names(d)) d$group2_level <- NULL
  # levels come as list columns (NULL = no level) from cards, as plain
  # NA-holed character columns from the flattened generated ARDs
  gl <- function(col) {
    if (!col %in% names(d)) return(rep("", nrow(d)))
    v <- d[[col]]
    if (is.list(v)) {
      vapply(v, function(x) {
        if (is.null(x) || length(x) == 0 || all(is.na(x))) "" else
          as.character(x[[1]])
      }, character(1))
    } else {
      ifelse(is.na(v), "", as.character(v))
    }
  }
  key <- paste(gl("variable"), gl("group1_level"), gl("group2_level"),
               gl("variable_level"), d$stat_name, sep = "|")
  val <- vapply(d$stat, function(v) {
    v <- if (is.list(v)) v[[1]] else v
    if (is.null(v) || length(v) == 0) return(NA_real_)
    tryCatch(suppressWarnings(as.numeric(v[[1]])),
             error = function(e) NA_real_)
  }, numeric(1))
  ok <- !is.na(val)
  out <- tapply(val[ok], key[ok], function(x) x[1])
  out[order(names(out))]
}

# the cards sample data, written the way the generated programmes read it
.fm_adam <- function(envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  adsl <- as.data.frame(cards::ADSL[, c("USUBJID", "ARM", "TRT01A", "TRTA",
                                        "AGE", "AGEGR1", "SEX", "RACE",
                                        "ETHNIC", "BMIBL", "SAFFL",
                                        "ITTFL")])
  adae <- as.data.frame(cards::ADAE[, c("USUBJID", "TRTA", "SAFFL",
                                        "AESOC", "AEDECOD")])
  utils::write.csv(adsl, file.path(d, "ADSL.csv"), row.names = FALSE,
                   na = "")
  utils::write.csv(adae, file.path(d, "ADAE.csv"), row.names = FALSE,
                   na = "")
  utils::write.csv(datasets::mtcars, file.path(d, "MTCARS.csv"),
                   row.names = FALSE, na = "")
  d
}

# run the programme directly on the same CSVs the generated code reads;
# returns every ARD object it created, bound into one. library() lines are
# neutralised for --as-cran; the packages are attached here instead.
.fm_direct <- function(src, adam, envir = parent.frame()) {
  suppressWarnings({
    withr::local_package("dplyr", .local_envir = envir)
    withr::local_package("cards", .local_envir = envir)
    if (requireNamespace("cardx", quietly = TRUE)) {
      withr::local_package("cardx", .local_envir = envir)
    }
  })
  env <- new.env(parent = globalenv())
  env$ADSL <- utils::read.csv(file.path(adam, "ADSL.csv"),
                              stringsAsFactors = FALSE)
  env$ADAE <- utils::read.csv(file.path(adam, "ADAE.csv"),
                              stringsAsFactors = FALSE)
  env$MTCARS <- utils::read.csv(file.path(adam, "MTCARS.csv"),
                                stringsAsFactors = FALSE)
  code <- gsub("^\\s*library\\(", "# library(", readLines(src))
  tf <- withr::local_tempfile(fileext = ".R")
  writeLines(code, tf)
  suppressMessages(suppressWarnings(source(tf, local = env)))
  cards_objs <- Filter(function(o) inherits(o, "card"),
                       mget(ls(env), envir = env))
  if (length(cards_objs) == 1) {
    cards_objs[[1]]
  } else {
    # different ard_* calls can share (variable, stat_name) keys
    do.call(cards::bind_ard,
            c(unname(cards_objs), list(.update = TRUE, .quiet = TRUE)))
  }
}

# the whole chain; `via` picks the recovery route
.fm_chain <- function(src, adam, output_id = "Out_x",
                      via = c("recover", "code"),
                      envir = parent.frame()) {
  via <- match.arg(via)
  direct <- .fm_direct(src, adam)
  rec <- if (via == "recover") {
    ars_params_recover(src, direct, output_id = output_id)
  } else {
    ars_params_from_code(src, output_id = output_id)
  }
  tmp <- withr::local_tempfile(fileext = ".xlsx", .local_envir = envir)
  expect_no_warning(write_ars_params(rec, tmp, overwrite = TRUE))
  ars <- build_ars(tmp)
  gen <- suppressWarnings(suppressMessages(
    ars_generate_ard(ars, adam_path = adam)
  ))
  list(direct = direct, rec = rec, ars = ars,
       gen = gen, s_gen = .fm_stats(gen), s_dir = .fm_stats(direct))
}

# per-variable check: the generated ARD must carry the variable's
# statistics AND agree with the direct run on every shared key
.fm_check_var <- function(ch, var, min_common = 1) {
  pref <- paste0(var, "|")
  gk <- names(ch$s_gen)[startsWith(names(ch$s_gen), pref)]
  dk <- names(ch$s_dir)[startsWith(names(ch$s_dir), pref)]
  common <- intersect(gk, dk)
  expect_gte(length(common), min_common,
             label = paste0("shared '", var, "' statistics (",
                            length(common), ")"))
  expect_equal(ch$s_gen[common], ch$s_dir[common], tolerance = 1e-8,
               info = paste("variable", var))
}

.fm_skip <- function() {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_on_cran()
}

# ---------------------------------------------------------------------------
# cards::ard_stack + ard_summary + ard_tabulate (reference examples)
# ---------------------------------------------------------------------------

test_that("matrix: ard_stack(ard_tabulate, ard_summary, .by, .total_n)", {
  .fm_skip()
  adam <- .fm_adam()
  src <- withr::local_tempfile(fileext = ".R")
  writeLines('
    library(cards)
    ard <- ard_stack(
      data = ADSL,
      ard_tabulate(variables = "AGEGR1"),
      ard_summary(variables = "AGE"),
      .by = "ARM",
      .total_n = TRUE
    )', src)
  ch <- .fm_chain(src, adam, output_id = "Out_dm")
  expect_true(all(ch$rec$analyses$population == "ALL"))
  .fm_check_var(ch, "AGE", min_common = 9)      # 3 arms x mean/sd/median +
  .fm_check_var(ch, "AGEGR1", min_common = 12)  # 3 arms x >=2 levels x n/p
})

# ---------------------------------------------------------------------------
# the maintainer's focus case: a mixed DM display, checked PER VARIABLE
# ---------------------------------------------------------------------------

test_that("matrix: mixed DM display - every variable, both routes", {
  .fm_skip()
  adam <- .fm_adam()
  src <- withr::local_tempfile(fileext = ".R")
  writeLines('
    library(cards); library(dplyr)
    vars_cont <- c("AGE", "BMIBL")
    vars_cat  <- c("AGEGR1", "SEX", "RACE")
    adsl <- ADSL %>% filter(SAFFL == "Y")
    ard <- adsl %>%
      ard_stack(
        ard_summary(variables = all_of(vars_cont)),
        ard_tabulate(variables = all_of(vars_cat)),
        .by = ARM,
        .total_n = TRUE
      )', src)

  for (via in c("recover", "code")) {
    ch <- .fm_chain(src, adam, output_id = "Out_dm", via = via)
    # the SAFFL filter must survive as the population on every analysis
    expect_true(all(ch$rec$analyses$population == "SAFFL"),
                info = via)
    # continuous variables: every summary statistic agrees
    for (v in c("AGE", "BMIBL")) .fm_check_var(ch, v, min_common = 9)
    # categorical variables: every level's n and % agree per arm
    for (v in c("AGEGR1", "SEX", "RACE")) .fm_check_var(ch, v,
                                                        min_common = 12)
    # the big N per arm is present in the generated ARD
    expect_true(any(startsWith(names(ch$s_gen), "ARM|")) ||
                  any(grepl("\\|N$", names(ch$s_gen))),
                info = via)
  }
})

# ---------------------------------------------------------------------------
# cards::ard_stack_hierarchical (reference example + over_variables)
# ---------------------------------------------------------------------------

test_that("matrix: ard_stack_hierarchical official example (safety AE)", {
  .fm_skip()
  adam <- .fm_adam()
  src <- withr::local_tempfile(fileext = ".R")
  # the reference example plus the SAFFL analysis-set filter of a real AE
  # display (population="ALL" cannot span the ADAE/ADSL dataset pair -
  # siera's no-filter fallback is single-dataset, see #37)
  writeLines('
    library(cards); library(dplyr)
    adae <- ADAE %>% filter(SAFFL == "Y")
    adsl <- ADSL %>% filter(SAFFL == "Y")
    ard <- ard_stack_hierarchical(
      adae,
      variables = c(AESOC, AEDECOD),
      by = TRTA,
      denominator = adsl,
      id = USUBJID,
      over_variables = TRUE
    )', src)
  ch <- .fm_chain(src, adam, output_id = "Out_ae")
  an <- ch$rec$analyses
  expect_true(all(an$population == "SAFFL"))
  expect_false(any(grepl("..ard_", an$group_by, fixed = TRUE)))
  # the code route recovered the whole display: any-event + both levels +
  # the denominator= total_n (the over_variables early-return bug, #48)
  expect_setequal(an$method, c("total_n", rep("categorical_summary", 3)))
  # subject counts per SOC / per PT / any-event agree with the direct run
  # (the by-PT analysis is FLAT, so fold the direct ARD's SOC grouping)
  ch$s_dir <- .fm_stats(ch$direct, drop_group2 = TRUE)
  .fm_check_var(ch, "AESOC", min_common = 3)
  .fm_check_var(ch, "AEDECOD", min_common = 3)
})

# ---------------------------------------------------------------------------
# cardx::ard_categorical_ci (reference example methods)
# ---------------------------------------------------------------------------

test_that("matrix: ard_categorical_ci methods survive the chain", {
  .fm_skip()
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom")
  adam <- .fm_adam()
  src <- withr::local_tempfile(fileext = ".R")
  writeLines('
    library(cards); library(cardx)
    ard <- ard_stack(
      data = ADSL,
      ard_tabulate(variables = "AGEGR1"),
      ard_summary(variables = "AGE"),
      .by = "ARM",
      .total_n = TRUE
    )
    ard_ci_w <- ard_categorical_ci(ADSL, by = ARM, variables = SEX,
                                   method = "wilson")
    ard_ci_j <- ard_categorical_ci(ADSL, by = ARM, variables = AGEGR1,
                                   method = "jeffreys")', src)
  ch <- .fm_chain(src, adam, output_id = "Out_ci", via = "code")
  an <- ch$rec$analyses
  expect_setequal(
    an$method,
    c("total_n", "categorical_summary", "continuous_summary",
      rep("proportion_ci", 2))
  )
  expect_true(any(grepl("method=wilson", an$options, fixed = TRUE)))
  expect_true(any(grepl("method=jeffreys", an$options, fixed = TRUE)))
  # per-level estimate + CI bounds agree with the direct cardx run
  .fm_check_var(ch, "SEX", min_common = 6)
  .fm_check_var(ch, "AGEGR1", min_common = 6)
})

# ---------------------------------------------------------------------------
# cardx::ard_stats_* tests (reference examples; formula + variables= forms)
# ---------------------------------------------------------------------------

test_that("matrix: chisq (variables=) reaches the ARD with the right
           p-value; fisher/prop_test say what they need", {
  .fm_skip()
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom")
  adam <- .fm_adam()
  src <- withr::local_tempfile(fileext = ".R")
  writeLines('
    library(cards); library(cardx)
    ard <- ard_stack(
      data = ADSL,
      ard_tabulate(variables = "AGEGR1"),
      ard_summary(variables = "AGE"),
      .by = "ARM",
      .total_n = TRUE
    )
    ard_chisq <- ard_stats_chisq_test(ADSL, by = "ARM",
                                      variables = "AGEGR1")
    ard_fisher <- ard_stats_fisher_test(ADSL, by = "ARM",
                                        variables = "SEX")', src)
  rec <- ars_params_from_code(src, output_id = "Out_tests")
  an <- rec$analyses
  # chisq rides the "category as 2nd grouping" idiom the template tests
  expect_equal(an$group_by[an$method == "chisq"], "ARM, AGEGR1")
  expect_equal(an$variable[an$method == "chisq"], "USUBJID")
  # fisher's variables= form cannot be generated faithfully (subject-flag
  # idiom) - the note says exactly what to set
  expect_true(any(grepl("subject-flag idiom", rec$notes)))

  # the chisq p-value survives the chain (fisher is excluded: its row
  # needs the manual variable/where completion the note asks for)
  src2 <- withr::local_tempfile(fileext = ".R")
  writeLines('
    library(cards); library(cardx)
    ard <- ard_stack(
      data = ADSL,
      ard_tabulate(variables = "AGEGR1"),
      ard_summary(variables = "AGE"),
      .by = "ARM",
      .total_n = TRUE
    )
    ard_chisq <- ard_stats_chisq_test(ADSL, by = "ARM",
                                      variables = "AGEGR1")', src2)
  ch <- .fm_chain(src2, adam, output_id = "Out_tests", via = "code")
  # direct chisq keys sit on variable AGEGR1; the generated one is stamped
  # on the 2nd grouping - compare the p.value values themselves
  p_dir <- ch$s_dir[grepl("\\|p\\.value$", names(ch$s_dir))]
  p_gen <- ch$s_gen[grepl("\\|p\\.value$", names(ch$s_gen))]
  expect_gte(length(p_gen), 1)
  expect_equal(sort(unname(p_gen))[1], sort(unname(p_dir))[1],
               tolerance = 1e-8)
})

test_that("matrix: aov / oneway_test formula interface is recovered (#48)", {
  .fm_skip()
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom")
  adam <- .fm_adam()
  src <- withr::local_tempfile(fileext = ".R")
  # the OFFICIAL examples use the formula flavour
  writeLines('
    library(cards); library(cardx)
    ard <- ard_stack(
      data = ADSL,
      ard_tabulate(variables = "AGEGR1"),
      ard_summary(variables = "AGE"),
      .by = "ARM",
      .total_n = TRUE
    )
    ard_aov <- ard_stats_oneway_test(AGE ~ ARM, data = ADSL)', src)
  rec <- ars_params_from_code(src, output_id = "Out_aov")
  an <- rec$analyses
  expect_true("anova" %in% an$method)
  expect_equal(an$variable[an$method == "anova"], "AGE")
  expect_equal(an$group_by[an$method == "anova"], "ARM")
  expect_equal(an$dataset[an$method == "anova"], "ADSL")
  # ... and the chain reproduces the p-value
  ch <- .fm_chain(src, adam, output_id = "Out_aov", via = "code")
  .fm_check_var(ch, "AGE", min_common = 9)
})

test_that("matrix: prop_test official example (mtcars) is recovered with
           the subject-flag guidance", {
  skip_if_not_installed("cards")
  src <- withr::local_tempfile(fileext = ".R")
  writeLines('
    library(cardx)
    ard_pt <- ard_stats_prop_test(MTCARS, by = vs, variables = am)', src)
  rec <- ars_params_from_code(src, output_id = "Out_pt")
  an <- rec$analyses
  expect_equal(an$method, "risk_difference")
  expect_equal(an$variable, "am")
  expect_equal(an$group_by, "vs")
  # ... and the note explains the manual completion the catalog needs
  expect_true(any(grepl("subject-flag idiom", rec$notes)))
})

# ---------------------------------------------------------------------------
# unsupported functions must degrade to notes, never to broken params
# ---------------------------------------------------------------------------

test_that("matrix: unsupported ard_* calls degrade to notes and the rest
           still builds", {
  .fm_skip()
  adam <- .fm_adam()
  src <- withr::local_tempfile(fileext = ".R")
  writeLines('
    library(cards); library(cardx)
    ard <- ard_stack(
      data = ADSL,
      ard_tabulate(variables = "AGEGR1"),
      ard_summary(variables = "AGE"),
      ard_missing(variables = "BMIBL"),
      ard_dichotomous(variables = "SEX", value = list(SEX = "F")),
      .by = "ARM",
      .total_n = TRUE
    )
    ard_t <- ard_stats_t_test(ADSL, by = "SEX", variables = "AGE")', src)
  rec <- ars_params_from_code(src, output_id = "Out_mix")
  # the unsupported calls left notes, not analyses
  expect_true(any(grepl("ard_missing", rec$notes)))
  expect_true(any(grepl("ard_dichotomous", rec$notes)))
  expect_false(any(grepl("ard_missing|ard_dichotomous",
                         rec$analyses$method)))
  # the supported remainder still builds and runs
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  expect_no_warning(write_ars_params(rec, tmp, overwrite = TRUE))
  ars <- build_ars(tmp)
  gen <- suppressWarnings(suppressMessages(
    ars_generate_ard(ars, adam_path = adam)
  ))
  expect_gt(nrow(gen), 0)
})
