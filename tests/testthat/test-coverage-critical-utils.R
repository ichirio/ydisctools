test_that("r2rtf utility helpers handle core branches", {
  expect_equal(ydisctools:::convert_units(10, from = "mm", to = "mm"), 10)
  expect_true(ydisctools:::convert_units(25.4, from = "mm", to = "in") > 0.99)
  expect_error(ydisctools:::convert_units(1, from = "bad", to = "mm"), "Unsupported source unit")
  expect_error(ydisctools:::convert_units(1, from = "mm", to = "bad"), "Unsupported target unit")

  expect_equal(length(ydisctools:::get_paper_dimension("a4", unit = "mm")), 2)
  expect_error(ydisctools:::get_paper_dimension("unknown"), "Unsupported paper size")

  x <- c("a", "b", "c")
  expect_equal(ydisctools:::insert_text(x, pos = c(0), text = "X")[1], "X")
  expect_equal(tail(ydisctools:::insert_text(x, pos = c(99), text = "X"), 1), "X")

  p <- c("p1", "p2", "p3")
  out <- ydisctools:::insert_elements(p, pos = c(1, 3))
  expect_true(length(out) >= 5)

  rtf <- c("{\\rtf1", "Table 14.1.1", "text", "\\page", "}")
  expect_true(ydisctools:::rtf_page_count(rtf) >= 2)
  expect_true(nchar(ydisctools:::find_rtf_text(rtf, "^Table", 2)) > 0)
  expect_true(nchar(ydisctools:::rpad("abc", 5)) == 5)

  toc_df <- ydisctools:::rtf_toc_list(list(rtf))
  expect_true(is.data.frame(toc_df))
  expect_true(nrow(toc_df) == 1)

  toc <- ydisctools:::generate_toc(list(rtf), toc_title = "Contents")
  expect_true(length(toc) > 0)
})

test_that("split and label helpers cover important branches", {
  expect_equal(split_text_by_max_bytes(NA_character_), NA_character_)
  expect_equal(split_text_by_max_bytes("", max_bytes = 5), "")
  expect_true(length(split_text_by_max_bytes("AAAA-BBBB-CCCC", max_bytes = 5)) >= 2)

  expect_equal(get_split_var_names("AETERM", 3), c("AETERM", "AETERM1", "AETERM2"))
  expect_equal(get_split_var_names("AETERM1", 3)[1], "AETERM1")

  df <- data.frame(id = 1, text = "alpha beta gamma", stringsAsFactors = FALSE)
  out <- split_column_by_max_bytes(df, "text", max_bytes = 6, label = "Test Label", keep_org = TRUE)
  expect_true(any(grepl("^text", names(out))))
  expect_true("text_ORG" %in% names(out))
  expect_equal(attr(out[["text"]], "label"), "Test Label")

  expect_error(split_column_by_max_bytes(df, "none"), "var_name")
  expect_error(set_label_on_sep_columns(df, "text", label = 1), "label")
})

test_that("text width and date compare branches", {
  expect_error(text_width("ABC", font = "Times"), "Unsupported font")
  expect_true(text_width_arial("A", points = 12) > 0)
  expect_warning(text_width_arial("😀", points = 12), "not found in width table")
  expect_error(text_width_courier_new(c("A", "B")), "single character")

  expect_false(comp_char_date("2025-01", "2025-01-30", "<=", mode = "IMAX"))
  expect_true(comp_char_date("2025-01", "2025-01-30", "<=", mode = "IMIN"))
  expect_true(is.na(comp_char_date("2025", "2025", mark = "BAD")))

  expect_error(comp_char_dates(c("2024-01"), c("2024-01", "2024-02")), "same length")
  expect_equal(comp_char_dates(c("2024-01", "2024-02"), c("2024-01", "2024-03"), mark = "<="), c(TRUE, TRUE))
})
