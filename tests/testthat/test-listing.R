# Listing preparation: listing_col() / rtf_listing() / suggest_listing_widths()
# / listing_to_rtftables().

adsl <- data.frame(
  USUBJID = c("63016-204", "63016-205", "63016-206"),
  DISPTPD = c("Ovarian cancer", "Fallopian tube cancer", "Primary peritoneal cancer"),
  BRCA    = c("Negative", "Positive", NA),
  STAGE   = c("IIIC", "IV", "IIIB"),
  stringsAsFactors = FALSE
)

# ── listing_col() ────────────────────────────────────────────────────────────

test_that("listing_col() accepts bare names and strings, and defaults sensibly", {
  a <- listing_col(USUBJID, width = 15)
  b <- listing_col("USUBJID", width = 15)
  expect_s3_class(a, "listing_col")
  expect_identical(a$vars, "USUBJID")
  expect_identical(a$vars, b$vars)
  expect_identical(a$header, "USUBJID")     # header defaults to the first var
  expect_identical(a$sep, "/")

  m <- listing_col(DISPTPD, BRCA, header = "Dx/BRCA")
  expect_identical(m$vars, c("DISPTPD", "BRCA"))
  expect_identical(m$name, "DISPTPD_BRCA")
  expect_null(m$width)
})

test_that("listing_col() validates its arguments", {
  expect_error(listing_col(), "at least one source column")
  expect_error(listing_col(USUBJID, width = 0), "positive integer")
  expect_error(listing_col(USUBJID, sep = c("a", "b")), "single string")
  expect_error(listing_col(USUBJID, header = 1), "single string")
})

# ── wrapping engine ──────────────────────────────────────────────────────────

test_that("cells break after the separator first", {
  expect_identical(.listing_wrap("Ovarian cancer/Negative/Serous", 18),
                   c("Ovarian cancer/", "Negative/", "Serous"))
})

test_that("an over-long first word does not emit a leading empty fragment", {
  # Regression: the original implementation appended trimws("") first.
  out <- .listing_wrap("Supercalifragilistic tail", 10)
  expect_false(any(!nzchar(out)))
  expect_true(all(.listing_disp_width(out) <= 10))
})

test_that("a single over-long token is hard-split", {
  out <- .listing_wrap(strrep("A", 24), 8)
  expect_identical(out, c(strrep("A", 8), strrep("A", 8), strrep("A", 8)))
})

test_that("wrapping honours display width, not character count", {
  out <- .listing_wrap("卵巣がん漿液性腺癌", 8)
  expect_true(all(.listing_disp_width(out) <= 8))
  expect_gt(length(out), 1L)             # 2-wide glyphs force a break
})

test_that("width = NULL leaves the cell unwrapped", {
  long <- strrep("x", 50)
  expect_identical(.listing_wrap(long, NULL), long)
})

# ── rtf_listing() ────────────────────────────────────────────────────────────

test_that("rtf_listing() expands records and aligns their lines", {
  lst <- rtf_listing(
    adsl,
    listing_col(USUBJID, width = 12),
    listing_col(DISPTPD, BRCA, width = 18),
    listing_col(STAGE, width = 8)
  )
  expect_s3_class(lst, "rtflisting")
  expect_identical(lst$n_records, 3L)
  # Every record contributes as many rows as its tallest cell.
  expect_gt(nrow(lst$data), 3L)
  # The record key numbers the source rows.
  expect_identical(sort(unique(lst$data[[lst$record_id]])), 1:3)
  # A record's first row carries the subject; continuation rows are blank.
  first <- lst$data[[1L]][lst$data[[lst$record_id]] == 1L]
  expect_identical(first[[1L]], "63016-204")
  expect_true(all(!nzchar(first[-1L])))
})

test_that("NA / empty source values are skipped when composing", {
  lst <- rtf_listing(adsl, listing_col(DISPTPD, BRCA, width = 40))
  # Record 3 has BRCA = NA -> no trailing separator.
  r3 <- lst$data[[1L]][lst$data[[lst$record_id]] == 3L]
  expect_false(any(grepl("//", r3, fixed = TRUE)))
  expect_true(any(grepl("Primary peritoneal cancer", r3, fixed = TRUE)))
})

test_that("spacer columns are inserted and reflected in the metadata", {
  lst <- rtf_listing(adsl,
                     listing_col(USUBJID, width = 12),
                     listing_col(STAGE, width = 8))
  # 2 content + 1 spacer + 1 hidden key
  expect_identical(ncol(lst$data), 4L)
  expect_true(any(grepl("^\\.spacer", names(lst$data))))
  expect_identical(length(lst$col_header), ncol(lst$data))
  expect_identical(length(lst$col_rel_width), ncol(lst$data))
  expect_identical(lst$col_rel_width[[2L]], 1)      # the spacer
})

test_that("spacer_width = 0 inserts no spacer columns", {
  lst <- rtf_listing(adsl,
                     listing_col(USUBJID, width = 12),
                     listing_col(STAGE, width = 8),
                     spacer_width = 0)
  expect_false(any(grepl("^\\.spacer", names(lst$data))))
  expect_identical(ncol(lst$data), 3L)              # 2 content + hidden key
  expect_identical(length(lst$col_header), 3L)
})

test_that("all columns are left-aligned and the header carries through", {
  lst <- rtf_listing(adsl, listing_col(USUBJID, header = "Unique\nSubject ID",
                                       width = 12))
  expect_true(all(vapply(lst$col_spec, function(s) s$align, character(1L)) == "left"))
  expect_identical(lst$col_header[[1L]], "Unique\nSubject ID")
})

test_that("rtf_listing() validates its arguments", {
  expect_error(rtf_listing(list(a = 1), listing_col(a)), "must be a data.frame")
  expect_error(rtf_listing(adsl), "listing_col")
  expect_error(rtf_listing(adsl, listing_col(USUBJID), template = "nope"),
               "type1")
  expect_error(rtf_listing(adsl, listing_col(USUBJID), spacer_width = -1),
               "non-negative")
  expect_error(rtf_listing(adsl, listing_col(NOPE)), "not found")
})

test_that("a zero-row input yields a zero-row listing", {
  lst <- rtf_listing(adsl[0L, ], listing_col(USUBJID, width = 12),
                     listing_col(STAGE, width = 8))
  expect_identical(nrow(lst$data), 0L)
  expect_identical(lst$n_records, 0L)
  expect_identical(length(lst$col_header), ncol(lst$data))
})

# ── suggest_listing_widths() ─────────────────────────────────────────────────

test_that("suggest_listing_widths() proposes widths that fit the budget", {
  w <- suggest_listing_widths(
    adsl,
    listing_col(USUBJID, header = "Unique\nSubject ID"),
    listing_col(DISPTPD, header = "Primary Diagnosis"),
    listing_col(STAGE,   header = "Stage"),
    total_width = 60
  )
  expect_named(w, c("USUBJID", "DISPTPD", "STAGE"))
  expect_true(all(w >= 6L))                      # min_width respected
  expect_lte(abs(sum(w) - 60L), 3L)              # scaled to the budget
  # The widest content gets the widest column.
  expect_identical(names(w)[which.max(w)], "DISPTPD")
})

test_that("suggest_listing_widths() validates its arguments", {
  expect_error(suggest_listing_widths(adsl), "listing_col")
  expect_error(suggest_listing_widths(adsl, listing_col(USUBJID),
                                      total_width = 0), "positive number")
  expect_error(suggest_listing_widths(adsl, listing_col(USUBJID), probs = 2),
               "\\[0, 1\\]")
})

# ── listing_to_rtftables() ───────────────────────────────────────────────────

test_that("listing_to_rtftables() paginates without splitting a record", {
  skip_if_not_installed("rtfreporter")

  many <- adsl[rep(seq_len(3L), 2L), ]
  many$USUBJID <- sprintf("63016-%03d", 201:206)
  lst <- rtf_listing(many,
                     listing_col(USUBJID, width = 12),
                     listing_col(DISPTPD, BRCA, width = 16),
                     listing_col(STAGE, width = 8))

  pages <- listing_to_rtftables(lst, max_rows = 8)
  expect_gt(length(pages), 1L)

  # The hidden key never reaches a rendered page, and the position-indexed
  # metadata is reindexed to the remaining columns.
  for (p in pages) {
    expect_false(lst$record_id %in% names(p$data))
    expect_identical(length(p$col_rel_width), ncol(p$data))
  }
})

test_that("listing_to_rtftables() records blank rows around and between records", {
  skip_if_not_installed("rtfreporter")

  lst <- rtf_listing(adsl, listing_col(USUBJID, width = 12),
                     listing_col(STAGE, width = 8))
  p <- listing_to_rtftables(lst)[[1L]]
  # Blank rows are carried as positions (materialised by the renderer):
  # 0 = above the first row, then one after each record, then the page end.
  expect_true(0L %in% p$blank_rows)
  expect_true(nrow(p$data) %in% p$blank_rows)
})

test_that("listing_to_rtftables() rejects a non-rtflisting", {
  expect_error(listing_to_rtftables(adsl), "must be an rtflisting")
})
