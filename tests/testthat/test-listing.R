# Listing preparation: listing_col() / rtf_listing() / auto_listing_widths()
# / listing_wrap() / listing_to_rtftables().

adsl <- data.frame(
  USUBJID = c("63016-204", "63016-205", "63016-206"),
  DISPTPD = c("Ovarian cancer", "Fallopian tube cancer", "Primary peritoneal cancer"),
  BRCA    = c("Negative", "Positive", NA),
  STAGE   = c("IIIC", "IV", "IIIB"),
  stringsAsFactors = FALSE
)

labelled <- local({
  d <- adsl
  attr(d$DISPTPD, "label") <- "Primary Diagnosis"
  attr(d$BRCA,    "label") <- "Any (BRCA) Mutations"
  d
})

meta_of <- function(x) attr(x, "rtf_listing", exact = TRUE)

# -- listing_col() -----------------------------------------------------------

test_that("listing_col() accepts bare names and strings, and defaults sensibly", {
  a <- listing_col(USUBJID, width = 15)
  b <- listing_col("USUBJID", width = 15)
  expect_s3_class(a, "rtf_listing_col")
  expect_identical(a$vars, "USUBJID")
  expect_identical(a$vars, b$vars)
  expect_null(a$header)                     # resolved later, from the data
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

# -- listing_wrap() ----------------------------------------------------------

test_that("listing_wrap() breaks after the separator first", {
  expect_identical(listing_wrap("Ovarian cancer/Negative/Serous", 18),
                   c("Ovarian cancer/", "Negative/", "Serous"))
})

test_that("listing_wrap() falls back to word boundaries", {
  expect_identical(listing_wrap("Partial response", 10),
                   c("Partial", "response"))
})

test_that("listing_wrap() keeps the space between two words it rejoins", {
  # "Progressive" alone is wider than the column, so it is hard-split; the
  # remainder must not be glued to the next word.
  expect_identical(listing_wrap("Progressive disease", 10),
                   c("Progressiv", "e disease"))
})

test_that("listing_wrap() hard-splits a single over-long token", {
  out <- listing_wrap("Immunohistochemistry", 8)
  expect_identical(out, c("Immunohi", "stochemi", "stry"))
  expect_true(all(.listing_disp_width(out) <= 8))
})

test_that("listing_wrap() does not emit a leading empty fragment", {
  expect_identical(listing_wrap("Supercalifragilistic word", 6)[[1L]], "Superc")
})

test_that("listing_wrap() honours display width, not character count", {
  # Five full-width glyphs = display width 10, so a width of 6 must split them.
  cjk <- intToUtf8(c(0x6F3F, 0x6DB2, 0x6027, 0x817A, 0x7648))
  out <- listing_wrap(cjk, 6)
  expect_true(all(.listing_disp_width(out) <= 6))
  expect_gt(length(out), 1L)
  expect_identical(paste(out, collapse = ""), cjk)
})

test_that("listing_wrap() leaves the cell alone when width is unset", {
  expect_identical(listing_wrap("Ovarian cancer/Negative", NULL),
                   "Ovarian cancer/Negative")
  expect_identical(listing_wrap("Ovarian cancer/Negative", NA),
                   "Ovarian cancer/Negative")
})

test_that("listing_wrap() vectorises over text and validates sep", {
  out <- listing_wrap(c("Partial response", "Progressive disease"), 10)
  expect_type(out, "list")
  expect_length(out, 2L)
  expect_identical(out[[1L]], c("Partial", "response"))
  expect_error(listing_wrap("x", 5, sep = c("a", "b")), "single string")
})

# -- rtf_listing(): shaping --------------------------------------------------

test_that("rtf_listing() returns a plain data.frame carrying its layout", {
  lst <- rtf_listing(adsl,
                     listing_col(USUBJID, width = 12),
                     listing_col(STAGE, width = 8))
  expect_s3_class(lst, "data.frame")
  expect_false(inherits(lst, "rtflisting"))
  m <- meta_of(lst)
  expect_identical(m$type, "type1")
  expect_identical(m$n_records, 3L)
  expect_identical(m$record_id, ".record_id")
  # Every column is a row heading -- rtfreporter's way of saying "left-align".
  expect_identical(m$row_title, names(lst))
})

test_that("rtf_listing() expands records and aligns their lines", {
  lst <- rtf_listing(adsl,
                     listing_col(USUBJID, width = 12),
                     listing_col(DISPTPD, BRCA, width = 12),
                     spacer_rel_width = 0)
  # Record 3 has no BRCA, so it composes to one long value; each record's
  # columns must occupy the same number of physical rows.
  n <- table(lst$.record_id)
  expect_identical(nrow(lst), sum(as.integer(n)))
  expect_true(all(n >= 1L))
  for (r in unique(lst$.record_id)) {
    blk <- lst[lst$.record_id == r, , drop = FALSE]
    expect_identical(nrow(blk), as.integer(n[[as.character(r)]]))
  }
  expect_identical(lst$USUBJID[lst$.record_id == 1L][[1L]], "63016-204")
})

test_that("rtf_listing() skips NA / empty source values when composing", {
  lst <- rtf_listing(adsl, listing_col(DISPTPD, BRCA, width = 40),
                     spacer_rel_width = 0)
  third <- lst$DISPTPD_BRCA[lst$.record_id == 3L]
  expect_false(any(grepl("/$", third)))          # no dangling separator
  expect_true(any(grepl("Primary peritoneal", third)))
})

test_that("rtf_listing() accepts type = 1 as well as \"type1\"", {
  a <- rtf_listing(adsl, listing_col(USUBJID, width = 12), type = "type1")
  b <- rtf_listing(adsl, listing_col(USUBJID, width = 12), type = 1)
  expect_identical(meta_of(a)$type, meta_of(b)$type)
  expect_error(rtf_listing(adsl, listing_col(USUBJID), type = "type9"),
               "only layout implemented")
})

test_that("rtf_listing() validates its arguments", {
  expect_error(rtf_listing(1, listing_col(USUBJID)), "must be a data.frame")
  expect_error(rtf_listing(adsl), "listing_col\\(\\) specifications")
  expect_error(rtf_listing(adsl, listing_col(USUBJID), spacer_rel_width = -1),
               "non-negative")
  expect_error(rtf_listing(adsl, listing_col(USUBJID), record_id = "USUBJID"),
               "already names a column")
  expect_error(rtf_listing(adsl, listing_col(USUBJID), listing_col(USUBJID)),
               "names must be unique")
  expect_error(rtf_listing(adsl, listing_col(NOPE)), "not found in `data`")
  expect_error(rtf_listing(adsl, listing_col(USUBJID), header = NA),
               "TRUE or FALSE")
})

test_that("a zero-row input yields a zero-row listing", {
  lst <- rtf_listing(adsl[0, ], listing_col(USUBJID, width = 12),
                     listing_col(STAGE, width = 8))
  expect_identical(nrow(lst), 0L)
  expect_identical(meta_of(lst)$n_records, 0L)
  expect_true(".record_id" %in% names(lst))
})

# -- rtf_listing(): headers --------------------------------------------------

test_that("an automatic header uses labels, falling back to names", {
  lst <- rtf_listing(labelled,
                     listing_col(USUBJID, width = 12),
                     listing_col(DISPTPD, BRCA, width = 30),
                     spacer_rel_width = 0)
  hdr <- meta_of(lst)$col_header
  expect_identical(hdr[[1L]], "USUBJID")                  # no label -> name
  expect_identical(hdr[[2L]], "Primary Diagnosis/\nAny (BRCA) Mutations")
})

test_that("an automatic header is wrapped to the column width", {
  lst <- rtf_listing(labelled, listing_col(DISPTPD, BRCA, width = 12))
  lines <- strsplit(meta_of(lst)$col_header[[1L]], "\n", fixed = TRUE)[[1L]]
  expect_true(all(.listing_disp_width(lines) <= 12))
  expect_gt(length(lines), 2L)
})

test_that("an explicit header is used exactly as written", {
  hand <- "Primary Diagnosis/\nBRCA"
  lst  <- rtf_listing(labelled,
                      listing_col(DISPTPD, BRCA, header = hand, width = 5))
  expect_identical(meta_of(lst)$col_header[[1L]], hand)
})

test_that("header = FALSE suppresses the header with an empty row list", {
  lst <- rtf_listing(adsl, listing_col(USUBJID, width = 12), header = FALSE)
  expect_identical(meta_of(lst)$col_header, list())
})

# -- rtf_listing(): widths and spacers ---------------------------------------

test_that("spacer columns are inserted and reflected in the metadata", {
  lst <- rtf_listing(adsl,
                     listing_col(USUBJID, width = 12),
                     listing_col(STAGE, width = 8),
                     spacer_rel_width = 1)
  expect_identical(names(lst),
                   c("USUBJID", ".spacer01", "STAGE", ".record_id"))
  expect_identical(meta_of(lst)$col_rel_width, c(12, 1, 8, 1))
  expect_true(all(lst$.spacer01 == ""))
})

test_that("spacer_rel_width = 0 inserts no spacer columns", {
  lst <- rtf_listing(adsl,
                     listing_col(USUBJID, width = 12),
                     listing_col(STAGE, width = 8),
                     spacer_rel_width = 0)
  expect_identical(names(lst), c("USUBJID", "STAGE", ".record_id"))
  expect_identical(meta_of(lst)$col_rel_width, c(12, 8, 1))
})

test_that("an unset width is measured from the data", {
  lst <- rtf_listing(adsl, listing_col(STAGE), spacer_rel_width = 0)
  # "IIIC" is 4 wide, the header "STAGE" is 5.
  expect_identical(meta_of(lst)$col_rel_width[[1L]], 5)
})

test_that("total_width fits the unset widths into what is left", {
  lst <- rtf_listing(adsl,
                     listing_col(USUBJID, width = 12),   # pinned
                     listing_col(DISPTPD, BRCA),         # fitted
                     listing_col(STAGE),                 # fitted
                     total_width = 60, spacer_rel_width = 0)
  w <- meta_of(lst)$col_rel_width
  expect_identical(w[[1L]], 12)                          # pinned, untouched
  expect_equal(sum(w[1:3]), 60, tolerance = 1)
  expect_error(
    rtf_listing(adsl, listing_col(USUBJID, width = 80), listing_col(STAGE),
                total_width = 60),
    "already used up")
})

test_that("spacer_twips promotes the whole table to absolute widths", {
  skip_if_not_installed("rtfreporter")
  lst <- rtf_listing(adsl,
                     listing_col(USUBJID, width = 12),
                     listing_col(STAGE, width = 8),
                     spacer_twips = 1)
  m <- meta_of(lst)
  expect_null(m$col_rel_width)
  expect_type(m$column_widths_twips, "integer")
  expect_identical(m$column_widths_twips[[2L]], 1L)      # the divider
  expect_gt(m$column_widths_twips[[1L]], 100L)           # 12 characters
})

test_that("table_width_twips scales the content columns only", {
  skip_if_not_installed("rtfreporter")
  lst <- rtf_listing(adsl,
                     listing_col(USUBJID, width = 12),
                     listing_col(DISPTPD, BRCA, width = 24),
                     listing_col(STAGE, width = 8),
                     spacer_twips = 1, table_width_twips = 12960)
  tw <- meta_of(lst)$column_widths_twips
  # The last entry is the hidden record key, which is dropped before render.
  expect_identical(sum(head(tw, -1L)), 12960L)
  # Both dividers stayed at exactly one twip.
  expect_identical(tw[c(2L, 4L)], c(1L, 1L))
})

test_that("table_width_twips needs spacer_twips, and must leave room", {
  skip_if_not_installed("rtfreporter")
  expect_error(
    rtf_listing(adsl, listing_col(USUBJID, width = 12),
                table_width_twips = 12960),
    "needs `spacer_twips`")
  expect_error(
    rtf_listing(adsl, listing_col(USUBJID, width = 12),
                listing_col(STAGE, width = 8),
                spacer_twips = 5000, table_width_twips = 100),
    "leaves no room")
})

# -- listing_to_rtftables() --------------------------------------------------

test_that("listing_to_rtftables() paginates without splitting a record", {
  skip_if_not_installed("rtfreporter")
  lst   <- rtf_listing(adsl,
                       listing_col(USUBJID, width = 12),
                       listing_col(DISPTPD, BRCA, width = 12))
  pages <- listing_to_rtftables(lst, max_rows = 6)
  expect_gt(length(pages), 1L)
  for (p in pages) expect_s3_class(p, "rtftable")
  # The record key is dropped, so no page carries it.
  expect_false(".record_id" %in% names(pages[[1L]]$data))
})

test_that("listing_to_rtftables() renders a single page and an RTF", {
  skip_if_not_installed("rtfreporter")
  lst   <- rtf_listing(adsl, listing_col(USUBJID, width = 12),
                       listing_col(STAGE, width = 8))
  pages <- listing_to_rtftables(lst)
  expect_length(pages, 1L)

  f <- withr::local_tempfile(fileext = ".rtf")
  rtfreporter::generate_rtfreport(
    rtfreporter::rtf_tables(rtfreporter::rtf_document(), pages),
    f, overwrite = TRUE)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
})

test_that("listing_to_rtftables() passes absolute widths through", {
  skip_if_not_installed("rtfreporter")
  lst <- rtf_listing(adsl,
                     listing_col(USUBJID, width = 12),
                     listing_col(STAGE, width = 8),
                     spacer_twips = 1, table_width_twips = 9000)
  p <- listing_to_rtftables(lst)[[1L]]
  expect_identical(sum(p$column_widths_twips), 9000L)
})

test_that("listing_to_rtftables() lets the caller override a default", {
  skip_if_not_installed("rtfreporter")
  lst <- rtf_listing(adsl, listing_col(USUBJID, width = 12))
  p   <- listing_to_rtftables(lst, blank_row_first = FALSE)
  expect_s3_class(p[[1L]], "rtftable")
})

test_that("listing_to_rtftables() rejects input without the attribute", {
  skip_if_not_installed("rtfreporter")
  lst <- rtf_listing(adsl, listing_col(USUBJID, width = 12))
  expect_error(listing_to_rtftables(lst[, 1, drop = FALSE]),
               "\"rtf_listing\" attribute")
  expect_error(listing_to_rtftables(adsl), "\"rtf_listing\" attribute")
})

# -- auto_listing_widths() ---------------------------------------------------

test_that("auto_listing_widths() proposes widths that fit the budget", {
  w <- auto_listing_widths(adsl,
                           listing_col(USUBJID),
                           listing_col(DISPTPD, BRCA),
                           listing_col(STAGE),
                           total_width = 60)
  expect_named(w, c("USUBJID", "DISPTPD_BRCA", "STAGE"))
  expect_true(all(w >= 6))
  expect_lte(abs(sum(w) - 60), 6)
})

test_that("auto_listing_widths() floors a column by its automatic header", {
  w <- auto_listing_widths(labelled, listing_col(DISPTPD, BRCA),
                           total_width = 5, min_width = 1)
  # The header line "Any (BRCA) Mutations" is 20 wide and cannot be ignored.
  expect_gte(attr(w, "demand")[["DISPTPD_BRCA"]], 20)
})

test_that("auto_listing_widths() validates its arguments", {
  expect_error(auto_listing_widths(1, listing_col(USUBJID)), "data.frame")
  expect_error(auto_listing_widths(adsl), "listing_col\\(\\) specifications")
  expect_error(auto_listing_widths(adsl, listing_col(USUBJID), probs = 2),
               "in \\[0, 1\\]")
})
