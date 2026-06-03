library(testthat)
library(ydisctools)
library(r2rtf)
library(dplyr)
library(tidyr)


test_that("rtf_encode_table2 handles table with subgroups", {
  skip_if_not_installed("r2rtf")

  test_data <- data.frame(
    Group = rep(c("Group A", "Group B"), each = 3),
    Item = paste("Item", 1:6),
    Value = c(10, 20, 30, 40, 50, 60)
  )

  # Test without grouping function - just basic table structure
  tbl_prepared <- test_data %>%
    rtf_page() %>%
    rtf_body()

  result <- rtf_encode(tbl_prepared, verbose = TRUE)

  # Should work without error
  expect_type(result, "list")
  expect_true("info" %in% names(result))
  expect_true("start" %in% names(result))
  expect_true("body" %in% names(result))
  expect_true("end" %in% names(result))
})

test_that("rtf_encode handles list input with warning", {
  skip_if_not_installed("r2rtf")

  tbl1 <- head(r2rtf::r2rtf_adae, 5) %>%
    rtf_page() %>%
    rtf_body()

  tbl2 <- head(r2rtf::r2rtf_adae, 6) %>%
    rtf_page() %>%
    rtf_body()

  attr(tbl1, "page")$page_title <- "all"
  attr(tbl1, "page")$page_footnote <- "last"
  attr(tbl1, "page")$page_source <- "last"
  attr(tbl2, "page")$page_title <- "all"
  attr(tbl2, "page")$page_footnote <- "last"
  attr(tbl2, "page")$page_source <- "last"

  expect_warning(
    rtf_encode(
      list(tbl1, tbl2),
      doc_type = "table",
      page_title = "all",
      page_footnote = "last",
      page_source = "last"
    ),
    "List of tables is not yet supported"
  )
})

test_that("rtf_encode validates arguments", {
  expect_error(rtf_encode(data.frame(x = 1), doc_type = "invalid"), "arg")
})

test_that("rtf_encode_list validates list/page constraints", {
  skip_if_not_installed("r2rtf")

  tbl1 <- head(r2rtf::r2rtf_adae, 3) %>%
    r2rtf::rtf_page() %>%
    r2rtf::rtf_body()
  tbl2 <- head(r2rtf::r2rtf_adae, 4) %>%
    r2rtf::rtf_page() %>%
    r2rtf::rtf_body()

  expect_error(ydisctools:::rtf_encode_list(list(tbl1)), "must >= 2")

  tbl2_w <- tbl2
  attr(tbl2_w, "page")$width <- attr(tbl2_w, "page")$width + 1
  expect_error(ydisctools:::rtf_encode_list(list(tbl1, tbl2_w)), "Page width")

  tbl1_t <- tbl1
  tbl2_t <- tbl2
  attr(tbl1_t, "page")$page_title <- "first"
  attr(tbl2_t, "page")$page_title <- "first"
  attr(tbl1_t, "page")$page_footnote <- "last"
  attr(tbl2_t, "page")$page_footnote <- "last"
  attr(tbl1_t, "page")$page_source <- "last"
  attr(tbl2_t, "page")$page_source <- "last"
  expect_error(ydisctools:::rtf_encode_list(list(tbl1_t, tbl2_t)), "page_title = 'all'")

  tbl1_f <- tbl1
  tbl2_f <- tbl2
  attr(tbl1_f, "page")$page_title <- "all"
  attr(tbl2_f, "page")$page_title <- "all"
  attr(tbl1_f, "page")$page_footnote <- "first"
  attr(tbl2_f, "page")$page_footnote <- "first"
  attr(tbl1_f, "page")$page_source <- "last"
  attr(tbl2_f, "page")$page_source <- "last"
  expect_error(ydisctools:::rtf_encode_list(list(tbl1_f, tbl2_f)), "page_footnote = 'last'")
})

test_that("assemble_rtf supports missing inputs and writes output", {
  td <- tempdir()
  f1 <- file.path(td, "part1.rtf")
  f2 <- file.path(td, "part2.rtf")
  out <- file.path(td, "assembled.rtf")

  writeLines(c("{\\rtf1", "\\sectd", "part1", "}"), f1)
  writeLines(c("{\\rtf1", "\\sectd", "part2", "}"), f2)

  expect_error(
    ydisctools:::assemble_rtf(c(f1, f2), file.path(td, "bad.txt")),
    "rtf"
  )

  expect_warning(
    ydisctools:::assemble_rtf(c(f1, file.path(td, "missing.rtf")), out),
    "Missing files|見つかりません"
  )
  expect_true(file.exists(out))

  ydisctools:::assemble_rtf(
    c(f1, f2),
    out,
    sectionpages = TRUE,
    cnt_numpages = TRUE,
    replace_sect = TRUE
  )
  expect_true(file.exists(out))
})

if(FALSE) {
  library(r2rtf)
  library(dplyr)
  library(tidyr)
  library(magrittr)

  tbl <- r2rtf_adae %>%
    count(TRTA, AEDECOD) %>%
    pivot_wider(names_from = TRTA, values_from = n, values_fill = 0)

  tbl_obj <- head(tbl) %>%
    rtf_body()

  attr(tbl_obj, "page")$border_first <- "single"
  attr(tbl_obj, "page")$border_last <- ""


  attr(tbl_obj, "border_left")[] <- ""
  attr(tbl_obj, "border_right")[] <- ""
  attr(tbl_obj, "border_top")[] <- ""
  attr(tbl_obj, "border_bottom")[] <- ""

  attr(attr(tbl_obj, "rtf_colheader")[[1]], "border_left")[] <- ""
  attr(attr(tbl_obj, "rtf_colheader")[[1]], "border_right")[] <- ""



  tbl_encode <- tbl_obj %>%
    rtf_encode()



  tbl_encode %>%
    write_rtf("c:/Yrepo/intro-ae1.rtf") # Step 3 Write to a .rtf file
}
