## Test 1: read_sas_datasets: Read Multiple SAS Datasets from a Folder ----
test_that("read_sas_datasets Test 1: Read Multiple SAS Datasets from a Folder", {
  edc_data <- read_sas_datasets("data")
  data_list <- c("dm", "vs", "VS2")

  expect_equal(sort(names(edc_data)), sort(data_list))
})
