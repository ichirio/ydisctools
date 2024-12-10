## Test 1: compute_dy: Compute Day Difference Between Two Dates ----
test_that("compute_dy Test 1: Compute Day Difference Between Two Dates", {
  refdate   <- c("2024-01-01", "2024-01-01", "", "2024-01-01", "2024-01-01T01-01", "2024-01-01")
  eventdate <- c("2024-01", "2023-12-31", "", "2024-01-00", "2024-01-01T01-01", "2023-12-32")

  result <- c(NA, -1, NA, NA, 1, NA)

  expect_equal(compute_dy(refdate, eventdate), result)
})
