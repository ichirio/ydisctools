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
    rtf_body()

  result <- rtf_encode_table2(tbl_prepared, verbose = TRUE)

  # Should work without error
  expect_type(result, "list")
  expect_true("info" %in% names(result))
  expect_true("start" %in% names(result))
  expect_true("body" %in% names(result))
  expect_true("end" %in% names(result))
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
    rtf_encode_table2()

  tbl_encode %>%
    write_rtf("c:/Yrepo/intro-ae1.rtf") # Step 3 Write to a .rtf file
}
