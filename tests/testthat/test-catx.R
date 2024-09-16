## Test 1: catx: combine characters with specified separator ----
test_that("catx Test 1: combine characters with specified separator", {
  race1 <- c("ASIAN", "ASIAN", NA, "", NA)
  race2 <- c("WHITE", "", "WHITE", NA, NA)
  race3 <- c("BLACK", NA, "", "BLACK", "")

  RACE <- c("ASIAN:WHITE:BLACK", "ASIAN", "WHITE", "BLACK", "")

  expect_equal(catx(":", race1, race2, race3), RACE)
})
