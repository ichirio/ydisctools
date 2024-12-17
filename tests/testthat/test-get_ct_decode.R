sdtm_meta <- read_sdtm_metadata_p21("test_spec.xls")

## Test 1: get_ct_decode: Get Decode Value from CT Metadata ----
test_that("get_ct_decode Test 1: Get CT decode values", {
  expect_equal(get_ct_decode(sdtm_meta$codelists, "LBTESTCD", c("RBC", "WBC")), c("Erythrocyte Count", "Leukocyte Count"))
})
