# Tests for ars_mock() (mock shells from the TOC workbook).

.mock_toc <- function(envir = parent.frame()) {
  tmp <- file.path(withr::local_tempdir(.local_envir = envir), "toc.xlsx")
  ars_toc_template(tmp, overwrite = TRUE)
  tmp
}

test_that("shell construction lays out titles, arms and stat blocks", {
  p <- ars_from_toc(.mock_toc())
  sh <- ydisctools:::.ars_mock_shell(p$toc[p$toc$output_id == "Out_dm", ],
                                     p$analyses, p$outputs)
  expect_equal(sh$titles,
               c("Table 14.1.1", "Summary of Demographic Data",
                 "Safety Population"))
  # generic arm placeholders when groups are data-driven
  expect_equal(names(sh$body)[2], "<TRT01A level 1>\n(N=XXX)")
  expect_equal(names(sh$body)[1], "Characteristic")
  # continuous block
  expect_true(all(c("  Mean (SD)", "  Min, Max") %in% sh$body[[1]]))
  expect_equal(sh$body[[2]][sh$body[[1]] == "  Mean (SD)"], "XX.X (XX.XX)")
  # categorical placeholder rows
  expect_true("  <AGEGR1 level 1>" %in% sh$body[[1]])
  # total_n does not appear as a body row
  expect_false("Number of subjects" %in% sh$body[[1]])
})

test_that("pre-defined groups become the arm columns", {
  tmp <- .mock_toc()
  toc <- as.data.frame(readxl::read_excel(tmp, sheet = "TOC"))
  ana <- as.data.frame(readxl::read_excel(tmp, sheet = "Analyses"))
  toc$groups[toc$output_id == "Out_dm"] <- "Placebo | Active"
  p <- ars_from_toc(list(toc = toc, analyses = ana))
  sh <- ydisctools:::.ars_mock_shell(p$toc[p$toc$output_id == "Out_dm", ],
                                     p$analyses, p$outputs)
  expect_equal(names(sh$body)[-1],
               c("Placebo\n(N=XXX)", "Active\n(N=XXX)"))
})

test_that("ars_mock writes an xlsx shell workbook", {
  skip_if_not_installed("openxlsx")
  tmp <- .mock_toc()
  out <- withr::local_tempfile(fileext = ".xlsx")
  ars_mock(tmp, out, overwrite = TRUE)
  expect_error(ars_mock(tmp, out), "already exists")

  sheets <- readxl::excel_sheets(out)
  expect_true(all(c("Out_dm", "Out_eff") %in% sheets))
  dm <- suppressMessages(
    readxl::read_excel(out, sheet = "Out_dm", col_names = FALSE,
                       trim_ws = FALSE))
  expect_equal(as.character(dm[1, 1]), "Table 14.1.1")
  expect_true(any(dm[[1]] == "  Mean (SD)", na.rm = TRUE))
  expect_true(any(dm[[2]] == "XX (XX.X)", na.rm = TRUE))
})

test_that("ars_mock writes a one-page-per-display rtf document", {
  skip_if_not_installed("rtfreporter")
  tmp <- .mock_toc()
  out <- withr::local_tempfile(fileext = ".rtf")
  ars_mock(tmp, out, format = "rtf", overwrite = TRUE)
  txt <- readLines(out, warn = FALSE)
  expect_true(any(grepl("Table 14.1.1", txt, fixed = TRUE)))
  expect_true(any(grepl("Table 14.2.1", txt, fixed = TRUE)))
  expect_true(any(grepl("Mean (SD)", txt, fixed = TRUE)))
})
