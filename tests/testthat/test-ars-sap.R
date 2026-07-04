# Tests for read_csr_map() / read_sap_toc() (draft extraction from Word).

.docx_with <- function(build, envir = parent.frame()) {
  skip_if_not_installed("officer")
  f <- withr::local_tempfile(fileext = ".docx", .local_envir = envir)
  doc <- officer::read_docx()
  doc <- build(doc)
  print(doc, target = f)
  f
}

test_that("read_csr_map extracts literal section numbers under the anchor", {
  f <- .docx_with(function(doc) {
    doc <- officer::body_add_par(doc, "12 Safety Evaluation",
                                 style = "heading 1")
    doc <- officer::body_add_par(doc, "12.2 Adverse Events",
                                 style = "heading 2")
    doc <- officer::body_add_par(doc, "14 Tables, Figures and Graphs",
                                 style = "heading 1")
    doc <- officer::body_add_par(doc, "14.1 Demographic Data",
                                 style = "heading 2")
    doc <- officer::body_add_par(doc, "14.2 Efficacy Data",
                                 style = "heading 2")
    doc <- officer::body_add_par(doc, "14.3.1 Displays of Adverse Events",
                                 style = "heading 2")
    officer::body_add_par(doc, "14.3.4 Laboratory Values",
                          style = "heading 2")
  })
  m <- read_csr_map(f)
  expect_equal(m$section_map[["baseline"]], "14.1")
  expect_equal(m$section_map[["efficacy"]], "14.2")
  # the anchor keeps the body section 12.2 from capturing the AE key
  expect_equal(m$section_map[["safety_ae"]], "14.3.1")
  expect_equal(m$section_map[["safety_lab"]], "14.3.4")
  expect_false(any(m$headings$reconstructed))
})

test_that("read_csr_map reconstructs Word auto-numbering from heading levels", {
  f <- .docx_with(function(doc) {
    for (t in c("Title Page", "Synopsis", "Ethics")) {
      doc <- officer::body_add_par(doc, t, style = "heading 1")
    }
    doc <- officer::body_add_par(doc, "Tables, Figures and Graphs",
                                 style = "heading 1")
    doc <- officer::body_add_par(doc, "Demographic and Baseline Data",
                                 style = "heading 2")
    doc <- officer::body_add_par(doc, "Efficacy Data", style = "heading 2")
    officer::body_add_par(doc, "Adverse Event Displays", style = "heading 2")
  })
  m <- read_csr_map(f)
  expect_equal(m$section_map[["baseline"]], "4.1")
  expect_equal(m$section_map[["safety_ae"]], "4.3")
  expect_true(any(m$headings$reconstructed))
  expect_true(any(grepl("reconstructed", m$notes)))
  # the derived map plugs straight into ars_from_toc numbering
  tmp <- file.path(withr::local_tempdir(), "toc.xlsx")
  ars_toc_template(tmp, overwrite = TRUE)
  p <- ars_from_toc(tmp, section_map = m$section_map)
  expect_equal(p$toc$toc_no[p$toc$output_id == "Out_dm"], "4.1.1")
})

test_that("read_csr_map flags a missing anchor and missing keys", {
  f <- .docx_with(function(doc) {
    officer::body_add_par(doc, "3 Efficacy Evaluation", style = "heading 1")
  })
  m <- read_csr_map(f)
  expect_true(any(grepl("no tables-section anchor", m$notes)))
  expect_true(any(grepl("section key 'safety_ae'", m$notes)))
  expect_equal(m$section_map[["efficacy"]], "3")
  expect_error(read_csr_map(file.path(tempdir(), "nope.docx")), "not found")
})

test_that("read_sap_toc drafts TOC rows from a planned-display table", {
  f <- .docx_with(function(doc) {
    displays <- data.frame(
      `Table No.` = c("Table 14.1.1", "Table 14.3.1.1", "Table 14.2.1"),
      Title = c("Summary of Demographic Data",
                paste("Summary of Treatment-Emergent Adverse Events by",
                      "System Organ Class"),
                "Primary Endpoint Analysis"),
      Population = c("Safety", "Safety", "ITT"),
      check.names = FALSE
    )
    doc <- officer::body_add_par(doc, "Planned Analyses and Displays",
                                 style = "heading 1")
    officer::body_add_table(doc, displays)
  })
  r <- read_sap_toc(f)
  expect_equal(nrow(r$toc), 3)
  # "Table " prefix cleaned off the numbers
  expect_equal(r$toc$toc_no, c("14.1.1", "14.3.1.1", "14.2.1"))
  # titles keyword-matched onto recipes, unmatched -> custom
  expect_equal(r$toc$display_type, c("dm_summary", "ae_soc", "custom"))
  expect_equal(r$toc$population, c("Safety", "Safety", "ITT"))
  expect_true(any(grepl("fill in `population`", r$notes)))
  expect_true(any(grepl("1 display\\(s\\) did not match", r$notes)))
})

test_that("read_sap_toc reports when no display table exists", {
  f <- .docx_with(function(doc) {
    officer::body_add_par(doc, "Statistical Methods", style = "heading 1")
  })
  r <- read_sap_toc(f)
  expect_null(r$toc)
  expect_true(any(grepl("no planned-display table", r$notes)))
})
