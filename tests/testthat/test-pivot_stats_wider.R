make_wide_input <- function() {
  data.frame(
    column1 = rep(c("cohort1", "cohort2"), each = 4),
    column2 = rep(rep(c("trt1", "trt2"), each = 2), times = 2),
    group1 = "Hoge",
    group2 = "Sex",
    label = rep(c("Male", "Female"), times = 4),
    fmt_value = c("11 (55.0)", "9 (45.0)", "12 (60.0)", "8 (40.0)",
                  "13 (65.0)", "7 (35.0)", "14 (70.0)", "6 (30.0)"),
    stringsAsFactors = FALSE
  )
}

## Test 1: pivot_stats_wider: spreads multiple columns into joined headers ----
test_that("pivot_stats_wider Test 1: combines expand columns with '____'", {
  out <- pivot_stats_wider(make_wide_input(), expand_cols = c("column1", "column2"))

  expect_equal(
    names(out),
    c("group1", "group2", "label",
      "cohort1____trt1", "cohort1____trt2",
      "cohort2____trt1", "cohort2____trt2")
  )
  expect_equal(nrow(out), 2L)
  expect_equal(out$label, c("Male", "Female"))
  expect_equal(out[["cohort1____trt1"]], c("11 (55.0)", "9 (45.0)"))
  expect_equal(out[["cohort2____trt2"]], c("14 (70.0)", "6 (30.0)"))
})

## Test 2: pivot_stats_wider: single expand column, custom separator ----
test_that("pivot_stats_wider Test 2: single column and custom sep", {
  df <- data.frame(
    label = c("Male", "Female", "Male", "Female"),
    arm = c("A", "A", "B", "B"),
    fmt_value = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  )
  out <- pivot_stats_wider(df, expand_cols = "arm", sep = "::")
  expect_equal(names(out), c("label", "A", "B"))
  expect_equal(out[["B"]], c("3", "4"))

  out2 <- pivot_stats_wider(df, expand_cols = "arm", sep = "::",
                            value_col = "fmt_value")
  expect_identical(out, out2)
})

## Test 3: pivot_stats_wider: input validation ----
test_that("pivot_stats_wider Test 3: invalid input raises informative errors", {
  df <- make_wide_input()
  expect_error(pivot_stats_wider(list()), "must be a data frame")
  expect_error(pivot_stats_wider(df), "at least one column")
  expect_error(pivot_stats_wider(df, expand_cols = "nope"),
               "not found in `data`")
  expect_error(
    pivot_stats_wider(df, expand_cols = "column1", value_col = "missing"),
    "not found in `data`"
  )
})
