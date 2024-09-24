result1 <- tibble::tibble(
  RDOMAIN = c("DM", "DM", "DM", "DM", "DM", "EC", "NV", "OE", "APQS", "APMH", "FA", "FA"),
  QNAM = c("RACE1", "RACE2", "RACE3", "RACE4", "RACE5", "ECREASOC", "NVCLSIG", "OECLSIG", "HOGE01", "HOGE01", "HOGE01", "HOGE01"),
  QLABEL = c("Race 1", "Race 2", "Race 3", "Race 4", "Race 5", "Reason for Occur Value", "Clinically Significant", "Clinically Significant", "Hoge 01", "Hoge 01", "Hoge 01", "Hoge 01"),
  QEVAL = NA,
  QORIG = "CRF",
  IDVAR = ""
)

result2 <- tibble::tibble(
  RDOMAIN = c("DM", "DM", "DM", "DM", "DM", "EC", "NV", "OE", "APQS", "APMH", "FA", "FA"),
  QNAM = c("RACE1", "RACE2", "RACE3", "RACE4", "RACE5", "ECREASOC", "NVCLSIG", "OECLSIG", "HOGE01", "HOGE01", "HOGE01", "HOGE01"),
  QLABEL = c("Race 1", "Race 2", "Race 3", "Race 4", "Race 5", "Reason for Occur Value", "Clinically Significant", "Clinically Significant", "Hoge 01", "Hoge 01", "Hoge 01", "Hoge 01"),
  QEVAL = NA,
  QORIG = "CRF",
  IDVAR = c(NA, NA, NA, NA, NA, "ECSPID", "NVSEQ", "OESEQ", "QSSEQ", "MHSPID", "FASEQ", "FASEQ")
)


## Test 1: read_sdtm_metadata_p21: Read SDTM Metadata from P21 Spec File ----
test_that("read_sdtm_metadata_p21 Test 1: Read SDTM Metadata from P21 Spec File", {
  sdtm_meta <- read_sdtm_metadata_p21("test_spec.xls")

  expect_equal(nrow(sdtm_meta$variables), 439)
})

## Test 2: read_sdtm_meta_supp: Read SDTM Supplemental Qualifiers Metadata from Excel File ----
test_that("read_sdtm_meta_supp Test 2: Read SDTM Supplemental Qualifiers Metadata from Excel File", {
  supp_meta1 <- read_sdtm_meta_supp("test_spec.xls")


  expect_equal(supp_meta1, result2)
})


## Test 3: read_sdtm_meta_supp: Read SDTM Supplemental Qualifiers Metadata from Excel File ----
test_that("read_sdtm_meta_supp Test 3: Read SDTM Supplemental Qualifiers Metadata from Excel File(No IDVAR)", {
  supp_meta2 <- read_sdtm_meta_supp("test_spec.xls", supp_sheet = "Suppqual2", idvar_col = NULL)


  expect_equal(supp_meta2, result1)
})

## Test 4: read_sdtm_meta_supp: Read SDTM Supplemental Qualifiers Metadata from Excel File ----
test_that("read_sdtm_meta_supp Test 4: Read SDTM Supplemental Qualifiers Metadata from Excel File(RDOMAIN)", {
  supp_meta3 <- read_sdtm_meta_supp("test_spec.xls", supp_sheet = "Suppqual3", dataset_col = "RDOMAIN")

  expect_equal(supp_meta3, result2)
})


