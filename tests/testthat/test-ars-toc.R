# Tests for the TOC workbook layer (ars_toc_template / ars_from_toc).

.toc_params <- function(envir = parent.frame()) {
  tmp <- file.path(withr::local_tempdir(.local_envir = envir), "toc.xlsx")
  ars_toc_template(tmp, overwrite = TRUE)
  tmp
}

test_that("ars_toc_template writes Study/TOC/Analyses sheets", {
  tmp <- .toc_params()
  expect_error(ars_toc_template(tmp), "already exists")
  expect_true(all(c("Study", "TOC", "Analyses") %in%
                    readxl::excel_sheets(tmp)))
  toc <- readxl::read_excel(tmp, sheet = "TOC")
  expect_true("custom" %in% toc$display_type)
})

test_that("ars_from_toc auto-numbers with the ICH E3-style default map", {
  p <- ars_from_toc(.toc_params())
  toc <- p$toc
  expect_equal(toc$toc_no[toc$output_id == "Out_dm"], "14.1.1")
  expect_equal(toc$toc_no[toc$output_id == "Out_ex"], "14.1.3")
  expect_equal(toc$toc_no[toc$output_id == "Out_ae_ov"], "14.3.1.1")
  expect_equal(toc$toc_no[toc$output_id == "Out_ae_pt"], "14.3.1.4")
  # custom rows default to the efficacy section
  expect_equal(toc$toc_no[toc$output_id == "Out_eff"], "14.2.1")
})

test_that("explicit toc_no wins and later numbers continue after it", {
  tmp <- .toc_params()
  toc <- as.data.frame(readxl::read_excel(tmp, sheet = "TOC"))
  ana <- as.data.frame(readxl::read_excel(tmp, sheet = "Analyses"))
  toc$toc_no[toc$output_id == "Out_dm"] <- "14.1.5"
  p <- ars_from_toc(list(toc = toc, analyses = ana))
  expect_equal(p$toc$toc_no[p$toc$output_id == "Out_dm"], "14.1.5")
  expect_equal(p$toc$toc_no[p$toc$output_id == "Out_ds"], "14.1.6")
})

test_that("a sponsor section map overrides the defaults", {
  tmp <- .toc_params()
  p <- ars_from_toc(tmp, section_map = c(safety_ae = "15.1"))
  expect_equal(p$toc$toc_no[p$toc$output_id == "Out_ae_ov"], "15.1.1")
  # unmapped keys keep their ICH default
  expect_equal(p$toc$toc_no[p$toc$output_id == "Out_dm"], "14.1.1")
})

test_that("recipes expand with the TOC row's study specifics", {
  p <- ars_from_toc(.toc_params())
  an <- p$analyses
  # 6+3+3+4+3+3+3 recipe rows + 3 custom rows
  expect_equal(nrow(an), 28)
  dm <- an[an$output_id == "Out_dm", ]
  expect_equal(dm$method[1:2], c("total_n", "continuous_summary"))
  expect_equal(dm$group_by[3], "TRT01A, AGEGR1")
  # recipe where + qualified 2nd grouping
  soc <- an[an$output_id == "Out_ae_soc", ]
  expect_equal(soc$group_by[3], "TRT01A, ADAE.AEBODSYS")
  expect_true(all(soc$where[2:3] == "TRTEMFL EQ Y"))
  # ae_overview combines multiple recipe conditions
  ov <- an[an$output_id == "Out_ae_ov", ]
  expect_true("TRTEMFL EQ Y; AESER EQ Y" %in% ov$where)
  # custom rows pass through untouched (apart from id assignment)
  eff <- an[an$output_id == "Out_eff", ]
  expect_equal(eff$dataset, c("ADSL", "ADEFF", "ADEFF"))
  expect_false(anyDuplicated(an$analysis_id) > 0)
})

test_that("extra TOC where conditions are ANDed onto recipe conditions", {
  tmp <- .toc_params()
  toc <- as.data.frame(readxl::read_excel(tmp, sheet = "TOC"))
  ana <- as.data.frame(readxl::read_excel(tmp, sheet = "Analyses"))
  toc$where[toc$output_id == "Out_ae_soc"] <- "AESER EQ Y"
  p <- ars_from_toc(list(toc = toc, analyses = ana))
  soc <- p$analyses[p$analyses$output_id == "Out_ae_soc", ]
  expect_equal(soc$where[3], "TRTEMFL EQ Y; AESER EQ Y")
})

test_that("toc_no and title become Displays Title subsections", {
  p <- ars_from_toc(.toc_params())
  d <- p$displays[p$displays$output_id == "Out_dm", ]
  expect_equal(d$text, c("Table 14.1.1", "Summary of Demographic Data"))
  ars <- build_ars(p)
  expect_true("Displays" %in% names(ars))
  expect_true("Table 14.1.1" %in% ars$Displays$displaySection_subSection_text)
})

test_that("ars_from_toc validates its input", {
  tmp <- .toc_params()
  toc <- as.data.frame(readxl::read_excel(tmp, sheet = "TOC"))
  ana <- as.data.frame(readxl::read_excel(tmp, sheet = "Analyses"))

  expect_error(ars_from_toc(file.path(tempdir(), "nope.xlsx")), "not found")
  expect_error(ars_from_toc(list()), "`toc` element")

  t2 <- toc; t2$display_type[1] <- "no_such_recipe"
  expect_error(ars_from_toc(list(toc = t2, analyses = ana)),
               "Unknown `display_type`")
  t2 <- toc; t2$output_id[2] <- t2$output_id[1]
  expect_error(ars_from_toc(list(toc = t2, analyses = ana)),
               "unique `output_id`")
  t2 <- toc; t2$population[1] <- ""
  expect_error(ars_from_toc(list(toc = t2, analyses = ana)),
               "`population` and `group_by`")
  t2 <- toc; t2$title[t2$display_type == "custom"] <- ""
  expect_error(ars_from_toc(list(toc = t2, analyses = ana)),
               "needs a `title`")
  # custom without Analyses rows
  expect_error(ars_from_toc(list(toc = toc)), "no rows for it")
  # bad section key
  t2 <- toc; t2$section_key[1] <- "banana"
  expect_error(ars_from_toc(list(toc = t2, analyses = ana)),
               "Unknown section_key")
})

test_that("a TOC-driven ARS runs end-to-end through siera", {
  skip_if_not_installed("siera")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")
  skip_on_cran()

  ex <- system.file("ars-examples", package = "ydisctools")
  skip_if(identical(ex, ""), "ars-examples folder not installed")

  # displays limited to what the shipped dummy ADaM supports
  toc <- data.frame(
    toc_no = NA_character_,
    output_id = c("Out_dm", "Out_ae_soc", "Out_ae_sev"),
    title = NA_character_,
    display_type = c("dm_summary", "ae_soc", "ae_severity"),
    section_key = NA_character_,
    population = "SAFFL", group_by = "TRT01A", groups = NA_character_,
    where = NA_character_,
    stringsAsFactors = FALSE
  )
  # drop the serious-TEAE-free recipes' unsupported vars: dm_summary needs
  # AGEGR1/SEX/RACE/ETHNIC and ae recipes need TRTEMFL/AEBODSYS/AESEV - all
  # present in the shipped adam data.
  ard <- ars_from_toc(list(toc = toc)) |>
    build_ars() |>
    ars_generate_ard(adam_path = file.path(ex, "adam"))
  expect_setequal(unique(ard$OutputId), c("Out_dm", "Out_ae_soc", "Out_ae_sev"))
  # spot check: SOC counts keyed by real variable names
  expect_true("AEBODSYS" %in% ard$variable)
  adsl <- read.csv(file.path(ex, "adam", "ADSL.csv"), stringsAsFactors = FALSE)
  saf <- adsl[adsl$SAFFL == "Y", ]
  truth <- vapply(split(saf$USUBJID, saf$TRT01A), length, numeric(1))
  n01 <- ard[ard$AnalysisId == "An_01" & ard$stat_name == "n", ]
  got <- vapply(n01$stat, as.numeric, numeric(1))
  names(got) <- n01$variable_level
  expect_equal(got[names(truth)], truth)
})
