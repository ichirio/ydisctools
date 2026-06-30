## Shared fixtures -------------------------------------------------------------

make_long <- function() {
  data.frame(
    column1 = "cohort1",
    column2 = "trt1",
    group1 = "Hoge",
    group2 = c("Age", "Age", "Age", "Age", "Age", "Age",
               "Sex", "Sex", "Sex", "Sex"),
    label = c("n", "Mean (SD)", "Mean (SD)", "Median",
              "Min, Max", "Min, Max", "Male", "Male", "Female", "Female"),
    stat = c("N", "mean", "sd", "median", "min", "max",
             "n", "p", "n", "p"),
    value = c(20, 50.345340, 8.435340, 48.453600, 40.234340, 70.445240,
              10, 50, 10, 50),
    stringsAsFactors = FALSE
  )
}

make_specs <- function() {
  data.frame(
    group2 = c("Age", "Age", "Age", "Age", "Sex", "Sex"),
    label = c("n", "Mean (SD)", "Median", "Min, Max", "Male", "Female"),
    template = c("{N:0}", "{mean:2} ({sd:3})", "{median:2}",
                 "{min:2}, {max:2}", "{n:0} ({p:1})", "{n:0} ({p:1})"),
    stringsAsFactors = FALSE
  )
}

## Test 1: format_stats: collapses long stats per the issue example ----
test_that("format_stats Test 1: collapses long stats to formatted rows", {
  out <- format_stats(make_long(), make_specs())

  expect_equal(
    names(out),
    c("column1", "column2", "group1", "group2", "label", "fmt_value")
  )
  expect_equal(nrow(out), 6L)
  expect_equal(
    out$fmt_value,
    c("20", "50.35 (8.435)", "48.45", "40.23, 70.45",
      "10 (50.0)", "10 (50.0)")
  )
  # grouping-key combinations keep first-appearance order
  expect_equal(
    out$label,
    c("n", "Mean (SD)", "Median", "Min, Max", "Male", "Female")
  )
})

## Test 2: format_stats: digits are per-token (variable-specific) ----
test_that("format_stats Test 2: per-token digits differ across variables", {
  long <- data.frame(
    param = c("Age", "Age", "Lab", "Lab"),
    label = "Mean (SD)",
    stat = c("mean", "sd", "mean", "sd"),
    value = c(50.345340, 8.435340, 50.345340, 8.435340),
    stringsAsFactors = FALSE
  )
  specs <- data.frame(
    param = c("Age", "Lab"),
    label = "Mean (SD)",
    template = c("{mean:1} ({sd:2})", "{mean:2} ({sd:3})"),
    stringsAsFactors = FALSE
  )
  out <- format_stats(long, specs)
  expect_equal(out$fmt_value, c("50.3 (8.44)", "50.35 (8.435)"))
})

## Test 3: format_stats: bare token inserts pre-formatted values verbatim ----
test_that("format_stats Test 3: bare {stat} token passes character through", {
  long <- data.frame(
    label = "Range",
    stat = c("min", "max"),
    value = c("40.2", "70.4"),  # already pre-formatted character
    stringsAsFactors = FALSE
  )
  specs <- data.frame(
    label = "Range",
    template = "{min}, {max}",
    stringsAsFactors = FALSE
  )
  out <- format_stats(long, specs)
  expect_equal(out$fmt_value, "40.2, 70.4")
})

## Test 4: format_stats: non-numeric value under {stat:n} kept as-is ----
test_that("format_stats Test 4: non-coercible value left unchanged", {
  long <- data.frame(
    label = c("n", "n"),
    stat = c("n", "p"),
    value = c("10", "ND"),  # ND cannot be coerced
    stringsAsFactors = FALSE
  )
  specs <- data.frame(
    label = "n",
    template = "{n:0} ({p:1})",
    stringsAsFactors = FALSE
  )
  out <- format_stats(long, specs)
  expect_equal(out$fmt_value, "10 (ND)")
})

## Test 5: format_stats: missing statistic renders as empty string ----
test_that("format_stats Test 5: token with no matching stat is blank", {
  long <- data.frame(
    label = "Mean (SD)",
    stat = "mean",   # sd is absent
    value = 50.3,
    stringsAsFactors = FALSE
  )
  specs <- data.frame(
    label = "Mean (SD)",
    template = "{mean:1} ({sd:2})",
    stringsAsFactors = FALSE
  )
  out <- format_stats(long, specs)
  expect_equal(out$fmt_value, "50.3 ()")
})

## Test 6: format_stats: digits fallback applies to bare tokens ----
test_that("format_stats Test 6: digits argument is the bare-token fallback", {
  long <- data.frame(
    label = "Mean",
    stat = "mean",
    value = 50.345340,
    stringsAsFactors = FALSE
  )
  specs <- data.frame(
    label = "Mean",
    template = "{mean}",
    stringsAsFactors = FALSE
  )
  expect_equal(format_stats(long, specs, digits = 2)$fmt_value, "50.35")
  expect_equal(format_stats(long, specs)$fmt_value, "50.34534")
})

## Test 7: format_stats: input validation ----
test_that("format_stats Test 7: invalid input raises informative errors", {
  long <- make_long()
  specs <- make_specs()

  expect_error(format_stats(list(), specs), "must be a data frame")
  expect_error(format_stats(long, specs, stat_col = "nope"),
               "not a column of `data`")
  expect_error(format_stats(long, specs[, "template", drop = FALSE]),
               "at least one key column")
  expect_error(
    format_stats(long, data.frame(other = "x", template = "{N:0}")),
    "not found in `data`"
  )
  expect_error(format_stats(long, specs, digits = -1), "non-negative")
})
