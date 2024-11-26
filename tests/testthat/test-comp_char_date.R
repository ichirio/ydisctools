## Test 1: comp_char_date: compares two character dates ----
test_that("comp_char_date Test 1: compares two character dates", {
  expect_equal(comp_char_date("2024-01-01T10:01", "2024-01-01"), TRUE)
})
