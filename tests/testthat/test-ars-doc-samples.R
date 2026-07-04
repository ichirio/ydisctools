# Tests for the bundled synthetic SAP / CSR template samples.

test_that("the sample documents ship with their generator and README", {
  d <- system.file("ars-doc-samples", package = "ydisctools")
  skip_if(identical(d, ""), "ars-doc-samples folder not installed")
  expect_true(file.exists(file.path(d, "sample_csr_template.docx")))
  expect_true(file.exists(file.path(d, "sample_sap.docx")))
  expect_true(file.exists(file.path(d, "make_doc_samples.R")))
  expect_true(file.exists(file.path(d, "README.md")))
})

test_that("the sample CSR template yields the company (15.x) section map", {
  skip_if_not_installed("officer")
  d <- system.file("ars-doc-samples", package = "ydisctools")
  skip_if(identical(d, ""), "ars-doc-samples folder not installed")

  m <- read_csr_map(file.path(d, "sample_csr_template.docx"))
  expect_equal(
    m$section_map[c("baseline", "efficacy", "safety_ae", "safety_lab")],
    c(baseline = "15.1", efficacy = "15.2",
      safety_ae = "15.3.1", safety_lab = "15.3.4")
  )
  # literal numbers: nothing reconstructed, anchored to section 15
  expect_false(any(m$headings$reconstructed))
  expect_true(any(grepl("Anchored to section 15", m$notes)))
})

test_that("the sample SAP drafts the expected TOC rows", {
  skip_if_not_installed("officer")
  d <- system.file("ars-doc-samples", package = "ydisctools")
  skip_if(identical(d, ""), "ars-doc-samples folder not installed")

  r <- read_sap_toc(file.path(d, "sample_sap.docx"))
  expect_equal(nrow(r$toc), 7)
  expect_true(all(is.na(r$toc$toc_no)))          # numbers left to the chain
  expect_equal(r$toc$display_type,
               c("dm_summary", "disposition", "exposure", "ae_overview",
                 "ae_soc", "ae_severity", "custom"))
  expect_equal(unique(r$toc$population), c("Safety", "ITT"))
})

test_that("the reviewed sample TOC numbers with the company map end-to-end", {
  skip_if_not_installed("officer")
  d <- system.file("ars-doc-samples", package = "ydisctools")
  skip_if(identical(d, ""), "ars-doc-samples folder not installed")

  m <- read_csr_map(file.path(d, "sample_csr_template.docx"))
  r <- read_sap_toc(file.path(d, "sample_sap.docx"))
  toc <- r$toc
  toc$population <- ifelse(toc$population == "ITT", "ITTFL", "SAFFL")
  toc$group_by <- "TRT01A"
  # drop the custom display: this test exercises the recipes + numbering
  toc <- toc[toc$display_type != "custom", ]

  p <- ars_from_toc(list(toc = toc), section_map = m$section_map)
  expect_equal(p$toc$toc_no,
               c("15.1.1", "15.1.2", "15.1.3",
                 "15.3.1.1", "15.3.1.2", "15.3.1.3"))
  ars <- build_ars(p)
  expect_true("Table 15.1.1" %in% ars$Displays$displaySection_subSection_text)
})
