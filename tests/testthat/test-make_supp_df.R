## Test 1: make_supp_df: Create a suppqual DataFrame ----
test_that("make_supp_df Test 1: Create a suppqual DataFrame(suppdm)", {
  dm <- data.frame(
    STUDYID = c("HOGE-01", "HOGE-01", "HOGE-01"),
    DOMAIN = c("DM", "DM", "DM"),
    USUBJID = c("HOGE-01-001", "HOGE-01-002", "HOGE-01-003"),
    SUBJID = c("001", "002", "003"),
    SUPP1 = c("ASIAN", "ASIAN", ""),
    SUPP2 = c("WHITE", "", ""),
    SUPP3 = c("", "", "BLACK OR AFRICAN AMERICAN")
  )

  suppmeta <- data.frame(
    RDOMAIN = c("DM", "DM", "DM", "AE", "AE", "AE", "CM", "CM", "CM"),
    IDVAR = c("", "", "", "AESEQ", "AESEQ", "AESEQ", "CMSPID", "CMSPID", "CMSPID"),
    QNAM = c("SUPP1", "SUPP2", "SUPP3", "SUPP1", "SUPP2", "SUPP3", "SUPP1", "SUPP2", "SUPP3"),
    QLABEL = c("Supp Label 1", "Supp Label 2", "Supp Label 3", "Supp Label 1", "Supp Label 2", "Supp Label 3", "Supp Label 1", "Supp Label 2", "Supp Label 3"),
    QORIG = c("CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF"),
    QEVAL = c("", "", "", "", "", "", "", "", "")
  )

  suppdm <- tibble::tibble(
    STUDYID = c("HOGE-01", "HOGE-01", "HOGE-01", "HOGE-01"),
    RDOMAIN = c("DM", "DM", "DM", "DM"),
    USUBJID = c("HOGE-01-001", "HOGE-01-001", "HOGE-01-002", "HOGE-01-003"),
    IDVAR = c("", "", "", ""),
    IDVARVAL = c("", "", "", ""),
    QNAM = c("SUPP1", "SUPP2", "SUPP1", "SUPP3"),
    QLABEL = c("Supp Label 1", "Supp Label 2", "Supp Label 1", "Supp Label 3"),
    QVAL = c("ASIAN", "WHITE", "ASIAN", "BLACK OR AFRICAN AMERICAN"),
    QORIG = c("CRF", "CRF", "CRF", "CRF"),
    QEVAL = c("", "", "", "")
  )

  expect_equal(make_supp_df(dm, suppmeta), suppdm)
})

## Test 2: make_supp_df: Create a suppqual DataFrame ----
test_that("make_supp_df Test 2: Create a suppqual DataFrame(suppae)", {
  ae <- data.frame(
    STUDYID = c("HOGE-01", "HOGE-01", "HOGE-01"),
    DOMAIN = c("AE", "AE", "AE"),
    USUBJID = c("HOGE-01-001", "HOGE-01-002", "HOGE-01-003"),
    AESEQ = c(1, 20, 30),
    AETERM = c("term1", "term2", "term3"),
    SUPP1 = c("supp1a", "supp1b", ""),
    SUPP2 = c("supp2a", "", ""),
    SUPP3 = c("supp3a", "supp3b", NA)
  )

  suppmeta <- data.frame(
    RDOMAIN = c("DM", "DM", "DM", "AE", "AE", "AE", "CM", "CM", "CM"),
    IDVAR = c("", "", "", "AESEQ", "AESEQ", "AESEQ", "CMSPID", "CMSPID", "CMSPID"),
    QNAM = c("SUPP1", "SUPP2", "SUPP3", "SUPP1", "SUPP2", "SUPP3", "SUPP1", "SUPP2", "SUPP3"),
    QLABEL = c("Supp Label 1", "Supp Label 2", "Supp Label 3", "Supp Label 1", "Supp Label 2", "Supp Label 3", "Supp Label 1", "Supp Label 2", "Supp Label 3"),
    QORIG = c("CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF"),
    QEVAL = c("", "", "", "", "", "", "", "", "")
  )

  suppae <- tibble::tibble(
    STUDYID = c("HOGE-01", "HOGE-01", "HOGE-01", "HOGE-01", "HOGE-01"),
    RDOMAIN = c("AE", "AE", "AE", "AE", "AE"),
    USUBJID = c("HOGE-01-001", "HOGE-01-001", "HOGE-01-001", "HOGE-01-002", "HOGE-01-002"),
    IDVAR = c("AESEQ", "AESEQ", "AESEQ", "AESEQ", "AESEQ"),
    IDVARVAL = c("1", "1", "1", "20", "20"),
    QNAM = c("SUPP1", "SUPP2", "SUPP3", "SUPP1", "SUPP3"),
    QLABEL = c("Supp Label 1", "Supp Label 2", "Supp Label 3", "Supp Label 1", "Supp Label 3"),
    QVAL = c("supp1a", "supp2a", "supp3a", "supp1b", "supp3b"),
    QORIG = c("CRF", "CRF", "CRF", "CRF", "CRF"),
    QEVAL = c("", "", "", "", "")
  )

  expect_equal(make_supp_df(ae, suppmeta), suppae)
})

## Test 3: make_supp_df: Create a suppqual DataFrame ----
test_that("make_supp_df Test 2: Create a suppqual DataFrame(suppcm)", {
  apcm <- data.frame(
    STUDYID = c("HOGE-01", "HOGE-01", "HOGE-01"),
    DOMAIN = c("APCM", "APCM", "APCM"),
    APID = c("HOGE-01-001-P", "HOGE-01-002-P", "HOGE-01-003-P"),
    CMSPID = c("CM-001", "CM-002", "CM-003"),
    CMTRT = c("trt1", "trt2", "trt3"),
    SUPP1 = c("supp1a", "supp1b", "supp1c"),
    SUPP2 = c("supp2a", "supp2b", ""),
    SUPP3 = c("", "", "")
  )

  suppmeta <- data.frame(
    RDOMAIN = c("DM", "DM", "DM", "AE", "AE", "AE", "APCM", "APCM", "APCM"),
    IDVAR = c("", "", "", "AESEQ", "AESEQ", "AESEQ", "CMSPID", "CMSPID", "CMSPID"),
    QNAM = c("SUPP1", "SUPP2", "SUPP3", "SUPP1", "SUPP2", "SUPP3", "SUPP1", "SUPP2", "SUPP3"),
    QLABEL = c("Supp Label 1", "Supp Label 2", "Supp Label 3", "Supp Label 1", "Supp Label 2", "Supp Label 3", "Supp Label 1", "Supp Label 2", "Supp Label 3"),
    QORIG = c("CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF", "CRF"),
    QEVAL = c("", "", "", "", "", "", "", "", "")
  )

  sqapcm <- tibble::tibble(
    STUDYID = c("HOGE-01", "HOGE-01", "HOGE-01", "HOGE-01", "HOGE-01"),
    RDOMAIN = c("APCM", "APCM", "APCM", "APCM", "APCM"),
    APID = c("HOGE-01-001-P", "HOGE-01-001-P", "HOGE-01-002-P", "HOGE-01-002-P", "HOGE-01-003-P"),
    IDVAR = c("CMSPID", "CMSPID", "CMSPID", "CMSPID", "CMSPID"),
    IDVARVAL = c("CM-001", "CM-001", "CM-002", "CM-002", "CM-003"),
    QNAM = c("SUPP1", "SUPP2", "SUPP1", "SUPP2", "SUPP1"),
    QLABEL = c("Supp Label 1", "Supp Label 2", "Supp Label 1", "Supp Label 2", "Supp Label 1"),
    QVAL = c("supp1a", "supp2a", "supp1b", "supp2b", "supp1c"),
    QORIG = c("CRF", "CRF", "CRF", "CRF", "CRF"),
    QEVAL = c("", "", "", "", "")
  )

  expect_equal(make_supp_df(apcm, suppmeta), sqapcm)
})
