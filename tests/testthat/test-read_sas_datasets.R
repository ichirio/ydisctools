## Test 1: read_sas_datasets: Read Multiple SAS Datasets from a Folder ----
test_that("read_sas_datasets Test 1: Read Multiple SAS Datasets from a Folder", {
  edc_data <- read_sas_datasets("data")
  data_list <- c("dm", "vs", "VS2")

  expect_equal(sort(names(edc_data)), sort(data_list))
})

test_that("read_sas_datasets Test 2: dataset filter works", {
  edc_data <- read_sas_datasets("data", datasets = c("dm", "vs"))
  expect_equal(sort(names(edc_data)), c("dm", "vs"))
})

test_that("read_xpts Test 1: read written xpt from temp folder", {
  tmp <- tempdir()
  xdf <- data.frame(USUBJID = c("01", "02"), AVAL = c(10, 20), stringsAsFactors = FALSE)
  haven::write_xpt(xdf, file.path(tmp, "adsl.xpt"), version = 5)

  lst <- read_xpts(tmp)
  expect_true("adsl" %in% names(lst))
  expect_equal(nrow(lst$adsl), 2)
})

test_that("pickup_subjects Test 1: filter only datasets with target column", {
  d1 <- data.frame(USUBJID = c("01", "02", "03"), AVAL = c(1, 2, 3), stringsAsFactors = FALSE)
  d2 <- data.frame(SUBJ = c("01", "02"), BVAL = c(5, 6), stringsAsFactors = FALSE)
  lst <- list(dm = d1, xx = d2)

  out <- pickup_subjects(lst, "USUBJID", c("01", "03"))
  expect_equal(out$dm$USUBJID, c("01", "03"))
  expect_equal(nrow(out$xx), 2)
})
