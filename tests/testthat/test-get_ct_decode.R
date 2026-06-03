sdtm_meta <- read_sdtm_metadata_p21("test_spec.xls")

## Test 1: get_ct_decode: Get Decode Value from CT Metadata ----
test_that("get_ct_decode Test 1: Get CT decode values", {
  expect_equal(get_ct_decode(sdtm_meta$codelists, "LBTESTCD", c("RBC", "WBC")), c("Erythrocyte Count", "Leukocyte Count"))
})

test_that("get_ct_decode Test 2: reverse lookup and get_ct_term", {
  dec <- c("Erythrocyte Count", "Leukocyte Count")
  expect_equal(get_ct_decode(sdtm_meta$codelists, "LBTESTCD", dec, reverse = TRUE), c("RBC", "WBC"))
  expect_equal(get_ct_term(sdtm_meta$codelists, "LBTESTCD", dec), c("RBC", "WBC"))
})

test_that("get_ct_decode Test 3: empty term returns empty vector", {
  expect_equal(get_ct_decode(sdtm_meta$codelists, "LBTESTCD", character(0)), character(0))
})

test_that("get_ct_decode Test 4: input validation errors", {
  bad_meta <- data.frame(id = "A", term = "T", stringsAsFactors = FALSE)
  expect_error(get_ct_decode(bad_meta, "A", "T"), "ct_meta")
  expect_error(get_ct_decode(sdtm_meta$codelists, c(1, 2), "T"), "id")
  expect_error(get_ct_decode(sdtm_meta$codelists, "A", 1), "term")
})
