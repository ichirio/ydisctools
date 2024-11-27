library(haven)
library(dplyr)
library(xportr)

df <- data.frame(
  STUDYID = rep("STUDY1", 3),
  DOMAIN = rep("DM", 3),
  USUBJID = c("SUBJ1", "SUBJ2", "SUBJ3"),
  SUBJID = c("001", "002", "003"),
  RFSTDTC = c("2021-01-01", "2021-01-02", "2021-01-03"),
  RFENDTC = c("2021-12-31", "2021-12-31", "2021-12-31"),
  RFXSTDTC = c("2021-01-01", "2021-01-02", "2021-01-03"),
  RFXENDTC = c("2021-12-31", "2021-12-31", "2021-12-31"),
  RFICDTC = c("2021-01-01", "2021-01-02", "2021-01-03"),
  RFPENDTC = c("2021-12-31", "2021-12-31", "2021-12-31"),
  DTHDTC = c(NA, NA, NA),
  DTHFL = c("N", "N", "N"),
  SITEID = c("SITE1", "SITE2", "SITE3"),
  BRTHDTC = c("1990-01-01", "1985-02-02", "1970-03-03"),
  AGE = c(31, 36, 51),
  AGEU = rep("YEARS", 3),
  SEX = c("M", "F", "M"),
  RACE = c("WHITE", "BLACK", "ASIAN"),
  ETHNIC = c("HISPANIC", "NOT HISPANIC", "NOT HISPANIC"),
  ARMCD = c("ARM1", "ARM2", "ARM3"),
  ARM = c("Treatment A", "Treatment B", "Treatment C"),
  ACTARMCD = c("ARM1", "ARM2", "ARM3"),
  ACTARM = c("Treatment A", "Treatment B", "Treatment C"),
  ARMNRS = c(NA, NA, NA),
  ACTARMUD = c(NA, NA, NA),
  COUNTRY = c("USA", "CAN", "MEX"),
  RACE1 = c("WHITE", "BLACK", "ASIAN"),
  RACE2 = c("WHITE", "BLACK", "ASIAN")
)

sdtm_meta <- read_sdtm_metadata_p21("test_spec.xls")
sdtm_meta$suppqual <- read_sdtm_meta_supp("test_spec.xls")

# dm_all <- make_sdtm_all_dataset(df, "DM", sdtm_meta)
# dm <- make_sdtm_dataset(df, "DM", sdtm_meta)

## Test 1: make_sdtm_supp_dataset: Create SDTM Supplemental Dataset with metadata ----
test_that("make_sdtm_supp_dataset Test 1: Create SDTM Supplemental Dataset with metadata", {
  suppdm <- make_sdtm_supp_dataset(df, "SUPPDM", sdtm_meta)

  expect_equal(nrow(suppdm), 6)
})





