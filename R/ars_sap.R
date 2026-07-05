# ============================================================================
#  ars_sap.R -- draft extraction from SAP / CSR Word documents
#  (part 3 of the SAP -> TOC -> mock -> ARS chain, ydisctools issue #18)
# ============================================================================
#
#  Both readers follow the ars_params_from_code() model: extract what is
#  statically safe from the document, and surface everything that needs a
#  human decision as an explicit note.  Nothing here is authoritative -- the
#  TOC workbook remains the single source of truth; these functions only
#  DRAFT it (or its numbering) from existing documents.
# ============================================================================

# -- shared: document summary --------------------------------------------------

.ars_docx_summary <- function(docx) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Reading Word documents needs the officer package.", call. = FALSE)
  }
  if (!file.exists(docx)) {
    stop("Document not found: '", docx, "'.", call. = FALSE)
  }
  officer::docx_summary(officer::read_docx(docx))
}

# headings with level, literal number (if present in the text) and title
.ars_docx_headings <- function(summary_df) {
  is_heading <- !is.na(summary_df$style_name) &
    grepl("^heading [0-9]+$", tolower(summary_df$style_name))
  h <- summary_df[is_heading & nzchar(trimws(summary_df$text)), , drop = FALSE]
  if (nrow(h) == 0) {
    return(data.frame(level = integer(0), number = character(0),
                      title = character(0), reconstructed = logical(0)))
  }
  level <- as.integer(sub("^heading ", "", tolower(h$style_name)))
  number <- rep(NA_character_, nrow(h))
  title <- trimws(h$text)
  rx <- "^\\s*([0-9]+(?:\\.[0-9]+)*)[\\.\\s]+(\\S.*)$"
  for (i in seq_len(nrow(h))) {
    m <- regmatches(title[i], regexec(rx, title[i], perl = TRUE))[[1]]
    if (length(m) == 3) {
      number[i] <- m[2]
      title[i] <- trimws(m[3])
    }
  }
  reconstructed <- is.na(number)
  # Word auto-numbering leaves no number in the text: reconstruct rule-based
  # from the heading levels in document order.
  if (any(reconstructed)) {
    counters <- integer(9)
    for (i in seq_len(nrow(h))) {
      if (!is.na(number[i])) {
        # re-seed the counters from an explicit number so reconstruction
        # continues consistently after it
        parts <- as.integer(strsplit(number[i], ".", fixed = TRUE)[[1]])
        counters[seq_along(parts)] <- parts
        if (length(parts) < length(counters)) {
          counters[(length(parts) + 1L):length(counters)] <- 0L
        }
        next
      }
      lv <- level[i]
      counters[lv] <- counters[lv] + 1L
      if (lv < length(counters)) counters[(lv + 1L):length(counters)] <- 0L
      number[i] <- paste(counters[seq_len(lv)], collapse = ".")
    }
  }
  data.frame(level = level, number = number, title = title,
             reconstructed = reconstructed, stringsAsFactors = FALSE)
}

# -- read_csr_map ----------------------------------------------------------------

#' Derive a TOC numbering section map from a CSR template
#'
#' Reads a (company) CSR template or shell document, walks its heading
#' structure, and derives the \code{section_map} that [ars_from_toc()] /
#' [ars_mock()] use for rule-based \code{toc_no} numbering -- so display
#' numbers follow the company's CSR section structure instead of the ICH E3
#' defaults.
#'
#' Literal section numbers are taken from the heading text when present.
#' When the template uses Word automatic numbering (no number in the text),
#' numbers are \emph{reconstructed} rule-based from the heading levels in
#' document order -- review the \code{headings} element in that case.  When a
#' heading matching \code{anchor_keyword} (default: the ICH E3 "tables,
#' figures and graphs" section) exists, keyword matching is restricted to its
#' subtree, so body sections (e.g. a section 12 "Adverse Events" discussion)
#' do not capture the map.
#'
#' @param docx Path to the CSR template (\code{.docx}).
#' @param keywords Named list of regular expressions (matched
#'   case-insensitively against heading titles) defining the section keys;
#'   default maps \code{baseline} / \code{efficacy} / \code{safety_ae} /
#'   \code{safety_lab}.
#' @param anchor_keyword Regular expression locating the tables-section
#'   anchor; \code{NULL} disables anchoring.
#'
#' @return A list with \code{section_map} (named character vector for
#'   [ars_from_toc()]), \code{headings} (the parsed heading table with a
#'   \code{reconstructed} flag) and \code{notes} (review notes).
#'
#' @seealso [ars_from_toc()], [read_sap_toc()]
#'
#' @examples
#' if (requireNamespace("officer", quietly = TRUE)) {
#' # build a tiny CSR-like template
#' doc <- officer::read_docx()
#' doc <- officer::body_add_par(doc, "12 Safety Evaluation", style = "heading 1")
#' doc <- officer::body_add_par(doc, "14 Tables, Figures and Graphs",
#'                              style = "heading 1")
#' doc <- officer::body_add_par(doc, "14.1 Demographic Data", style = "heading 2")
#' doc <- officer::body_add_par(doc, "14.2 Efficacy Data", style = "heading 2")
#' doc <- officer::body_add_par(doc, "14.3 Adverse Events", style = "heading 2")
#' tmp <- file.path(tempdir(), "csr.docx")
#' print(doc, target = tmp)
#' read_csr_map(tmp)$section_map
#' }
#'
#' @export
read_csr_map <- function(docx, keywords = NULL, anchor_keyword =
                           "tables?, figures?|figures? and graphs|listing") {
  if (is.null(keywords)) {
    keywords <- list(
      baseline   = "demograph|baseline|disposition|exposure",
      efficacy   = "efficacy",
      safety_ae  = "adverse event",
      safety_lab = "laborator"
    )
  }
  h <- .ars_docx_headings(.ars_docx_summary(docx))
  notes <- character(0)
  if (nrow(h) == 0) {
    stop("No heading paragraphs found in '", docx,
         "' (styles 'heading 1..9').", call. = FALSE)
  }
  if (any(h$reconstructed)) {
    notes <- c(notes, paste0(
      "REVIEW: ", sum(h$reconstructed), " heading number(s) were ",
      "reconstructed from heading levels (the template uses Word ",
      "auto-numbering); verify them against the rendered template."))
  }

  scope <- h
  if (!is.null(anchor_keyword)) {
    hit <- grepl(anchor_keyword, tolower(h$title), perl = TRUE)
    if (any(hit)) {
      anchor <- h[which(hit)[1], ]
      in_tree <- startsWith(h$number, paste0(anchor$number, ".")) |
        h$number == anchor$number
      scope <- h[in_tree, , drop = FALSE]
      notes <- c(notes, paste0(
        "Anchored to section ", anchor$number, " '", anchor$title, "'."))
    } else {
      notes <- c(notes, paste0(
        "REVIEW: no tables-section anchor matched '", anchor_keyword,
        "'; keywords were matched against the WHOLE document."))
    }
  }

  section_map <- character(0)
  for (key in names(keywords)) {
    hit <- grepl(keywords[[key]], tolower(scope$title), perl = TRUE)
    if (any(hit)) {
      section_map[[key]] <- scope$number[which(hit)[1]]
    } else {
      notes <- c(notes, paste0(
        "REVIEW: no heading matched section key '", key,
        "' (pattern: ", keywords[[key]], "); the ICH E3 default will be ",
        "used for it."))
    }
  }
  list(section_map = section_map, headings = h, notes = notes)
}

# -- read_sap_toc -----------------------------------------------------------------

# map a display title onto a bundled recipe key (else "custom")
.ars_guess_display_type <- function(title) {
  t <- tolower(title)
  if (grepl("demograph", t)) return("dm_summary")
  if (grepl("disposition", t)) return("disposition")
  if (grepl("exposure", t)) return("exposure")
  if (grepl("adverse", t) || grepl("teae", t)) {
    if (grepl("organ class|soc", t)) return("ae_soc")
    if (grepl("preferred term", t)) return("ae_pt")
    if (grepl("severity", t)) return("ae_severity")
    if (grepl("overall|overview|summary", t)) return("ae_overview")
  }
  "custom"
}

#' Draft a TFL TOC from a SAP Word document
#'
#' Statically reads a SAP (\code{.docx}, e.g. based on the TransCelerate
#' template) and drafts TOC workbook rows from it via two routes:
#'
#' \describe{
#'   \item{\strong{table route}}{a table that looks like a planned-display list
#'     (a header row with number- and title-like columns) becomes TOC rows;
#'     display numbers are cleaned (\code{"Table 14.1.1"} -> \code{"14.1.1"}).}
#'   \item{\strong{prose route}}{when no such table exists, the
#'     \dQuote{Statistical Methods / Analyses} section is scanned for display
#'     declarations -- sentences whose \emph{subject} names a clinical data
#'     domain and that carry a display verb (\dQuote{Demographic data
#'     \strong{will be summarized} by treatment group}).  Statistics-detail
#'     lines (\dQuote{95\% CIs will be provided}) name no domain and are
#'     dropped; the scan is anchored to that section's subtree so out-of-scope
#'     prose does not leak in.  The verbatim sentence is kept in the
#'     \code{source} column for review.}
#' }
#'
#' Each title/sentence is keyword-matched against the bundled display-recipe
#' catalog (unmatched displays stay \code{"custom"}).  Real SAPs rarely carry a
#' planned-display table, so \code{"auto"} usually resolves to the prose route.
#'
#' This is a DRAFT generator: review the \code{notes} and each row's
#' \code{source}, fill in \code{population} / \code{group_by} (and
#' \code{Analyses} rows for custom displays), then feed the result to
#' [ars_from_toc()].  Numbering conventions normally live in the TFL
#' specification rather than the SAP, which is why the TOC workbook (not the
#' SAP) remains the chain's source of truth.
#'
#' @param docx Path to the SAP (\code{.docx}).
#' @param mode Which extraction route to use: \code{"auto"} (default; try the
#'   table route, fall through to the prose route), \code{"table"} or
#'   \code{"prose"}.
#'
#' @return A list with \code{toc} (draft TOC rows: \code{toc_no},
#'   \code{output_id}, \code{title}, \code{display_type}, \code{section_key},
#'   \code{population}, \code{group_by}, \code{groups}, \code{where}, and
#'   \code{source} -- the SAP sentence a prose row came from, \code{NA} for
#'   table rows) and \code{notes}.
#'
#' @seealso [ars_from_toc()], [read_csr_map()]
#'
#' @examples
#' if (requireNamespace("officer", quietly = TRUE)) {
#' # a SAP whose display list lives in Statistical Methods prose (no table)
#' doc <- officer::read_docx()
#' doc <- officer::body_add_par(doc, "Statistical Methods", style = "heading 1")
#' doc <- officer::body_add_par(doc,
#'   "Demographic and baseline characteristics will be summarized by treatment group.")
#' doc <- officer::body_add_par(doc,
#'   "Treatment-emergent adverse events will be summarized by system organ class.")
#' tmp <- file.path(tempdir(), "sap.docx")
#' print(doc, target = tmp)
#' read_sap_toc(tmp)$toc
#' }
#'
#' @export
read_sap_toc <- function(docx, mode = c("auto", "table", "prose")) {
  mode <- match.arg(mode)
  s <- .ars_docx_summary(docx)
  notes <- character(0)
  toc <- NULL
  cols <- c("toc_no", "output_id", "title", "display_type", "section_key",
            "population", "group_by", "groups", "where", "source")

  if (mode %in% c("auto", "table")) {
    tbl <- .ars_sap_table_rows(s)
    notes <- c(notes, tbl$notes)
    if (!is.null(tbl$toc)) toc <- tbl$toc
  }
  if (is.null(toc) && mode %in% c("auto", "prose")) {
    pr <- .ars_sap_prose_rows(s, basename(docx))
    notes <- c(notes, pr$notes)
    if (!is.null(pr$toc)) toc <- pr$toc
  }

  if (!is.null(toc)) {
    toc <- .ars_ensure_cols(toc, cols)[, cols, drop = FALSE]
    toc$output_id <- sprintf("Out_%02d", seq_len(nrow(toc)))
    n_custom <- sum(toc$display_type == "custom")
    notes <- c(notes,
      "REVIEW: fill in `population` and `group_by` for every row (and check any extracted populations - free-text values like 'Safety' must become flag conditions such as 'SAFFL').",
      if (n_custom > 0) paste0(
        "REVIEW: ", n_custom, " display(s) did not match a recipe and are ",
        "'custom' - author their Analyses rows before ars_from_toc()."))
  }
  list(toc = toc, notes = notes)
}

# -- table route: a planned-display table (number + title columns) ------------

.ars_sap_table_rows <- function(s) {
  notes <- character(0)
  cells <- s[s$content_type == "table cell" & !is.na(s$text), , drop = FALSE]
  # officer identifies the table via table_index (doc_index is per CELL)
  if (nrow(cells) > 0 && !"table_index" %in% names(cells)) {
    cells$table_index <- 1L
  }
  toc_rows <- list()
  if (nrow(cells) > 0) {
    for (tbl_idx in unique(cells$table_index)) {
      tc <- cells[cells$table_index == tbl_idx, , drop = FALSE]
      header <- tc[tc$row_id == min(tc$row_id), , drop = FALSE]
      header <- header[order(header$cell_id), , drop = FALSE]
      hl <- tolower(trimws(header$text))
      no_col <- header$cell_id[grepl("^(table\\s*)?(no|number|id)\\b|^tfl|^output",
                                     hl, perl = TRUE)]
      ti_col <- header$cell_id[grepl("title|name|display", hl, perl = TRUE)]
      pop_col <- header$cell_id[grepl("population|analysis set", hl,
                                      perl = TRUE)]
      if (length(no_col) == 0 || length(ti_col) == 0) next

      body_rows <- setdiff(unique(tc$row_id), min(tc$row_id))
      for (r in sort(body_rows)) {
        cell <- function(col) {
          v <- tc$text[tc$row_id == r & tc$cell_id == col[1]]
          if (length(v) == 0 || !nzchar(trimws(v[1]))) NA_character_ else
            trimws(v[1])
        }
        no <- cell(no_col)
        ti <- cell(ti_col)
        if (is.na(ti)) next
        if (!is.na(no)) {
          no <- sub("^\\s*(table|figure|listing)\\s*", "", no,
                    ignore.case = TRUE)
        }
        pop <- if (length(pop_col) > 0) cell(pop_col) else NA_character_
        toc_rows[[length(toc_rows) + 1L]] <- data.frame(
          toc_no = no,
          output_id = NA_character_,
          title = ti,
          display_type = .ars_guess_display_type(ti),
          section_key = NA_character_,
          population = pop,
          group_by = NA_character_,
          groups = NA_character_,
          where = NA_character_,
          source = NA_character_,
          stringsAsFactors = FALSE
        )
      }
      notes <- c(notes, paste0(
        "Extracted ", length(toc_rows), " display row(s) from a table with ",
        "header: ", paste(header$text, collapse = " | "), "."))
    }
  }
  if (length(toc_rows) == 0) {
    return(list(toc = NULL, notes = notes))
  }
  list(toc = do.call(rbind, toc_rows), notes = notes)
}

# -- prose route: display declarations in Statistical-Methods prose -----------
#
#  Real SAPs seldom carry a planned-display table; the display list lives in
#  the "Statistical Methods" prose as declarative sentences ("Demographic data
#  will be summarized by treatment group.").  We (1) anchor the scan to that
#  section's subtree, (2) keep only sentences whose SUBJECT names a clinical
#  data domain and that carry a display verb -- the subject, not the verb,
#  separates a display from a statistics-detail line ("95% CIs will be
#  provided") -- and (3) classify each via .ars_guess_display_type().  A draft,
#  never authoritative: the verbatim sentence rides along in `source`.

# display-announcing verbs: passive future/modal + simple present
.ARS_DISPLAY_VERB <- paste0(
  "(?:will be|shall be|are|is)\\s+",
  "(?:summari[sz]ed|presented|tabulated|listed|displayed|provided|",
  "analy[sz]ed|reported|shown|plotted|graphed)")

# subject must name a clinical data domain (broad gate; classification is left
# to .ars_guess_display_type(), which maps the recognised ones to recipes)
.ars_prose_domain_rx <- function() {
  paste(c(
    "demograph", "baseline characteristic", "disposition", "discontinuation",
    "subject accountability", "extent of exposure", "\\bexposure\\b",
    "treatment compliance", "adverse event", "\\bteaes?\\b",
    "treatment[- ]emergent", "\\bdeaths?\\b", "medical histor",
    "concomitant medication", "prior medication", "laborator", "haematolog",
    "hematolog", "clinical chemistr", "urinalys", "vital sign",
    "electrocardiogram", "\\becgs?\\b", "physical exam", "protocol deviation",
    "pharmacokinetic", "immunogenicit", "\\befficacy\\b", "primary endpoint",
    "secondary endpoint"
  ), collapse = "|")
}

# paragraphs under the "Statistical Methods/Analyses" heading subtree (in doc
# order), or all body paragraphs when no such anchor exists.
.ars_sap_methods_paragraphs <- function(s) {
  s <- s[order(s$doc_index), , drop = FALSE]
  is_para <- s$content_type == "paragraph"
  s <- s[is_para, , drop = FALSE]
  style <- tolower(ifelse(is.na(s$style_name), "", s$style_name))
  is_head <- grepl("^heading [0-9]+$", style)
  level <- ifelse(is_head, suppressWarnings(as.integer(sub("^heading ", "",
                                                           style))), NA_integer_)
  txt <- ifelse(is.na(s$text), "", trimws(s$text))
  anchor_rx <- paste0("statistical (methods?|analys[ei]s|consideration)",
                      "|methods? of (statistical )?analys[ei]s",
                      "|analysis methods")
  anchor <- which(is_head & nzchar(txt) &
                    grepl(anchor_rx, tolower(txt), perl = TRUE))
  if (length(anchor) > 0) {
    a <- anchor[1]
    alv <- level[a]
    after <- which(is_head & !is.na(level) & level <= alv &
                     seq_along(level) > a)
    end <- if (length(after) > 0) after[1] - 1L else length(txt)
    idx <- seq.int(a + 1L, end)
    idx <- idx[!is_head[idx] & nzchar(txt[idx])]
    return(list(paragraphs = txt[idx], anchored = TRUE))
  }
  idx <- which(!is_head & nzchar(txt))
  list(paragraphs = txt[idx], anchored = FALSE)
}

# split a paragraph block into sentences (draft-grade: on . ; before a capital)
.ars_split_sentences <- function(paragraphs) {
  txt <- gsub("\\s+", " ", paste(paragraphs, collapse = " "))
  parts <- unlist(strsplit(txt, "(?<=[.;])\\s+(?=[A-Z(])", perl = TRUE))
  trimws(parts[nzchar(trimws(parts))])
}

.ars_prose_title <- function(subject) {
  t <- sub("^(the|a|an|all|any|each)\\s+", "", subject, ignore.case = TRUE)
  t <- trimws(gsub("\\s+", " ", t))
  if (nchar(t) > 120) t <- paste0(substr(t, 1, 117), "...")
  if (nchar(t) > 0) t <- paste0(toupper(substr(t, 1, 1)), substr(t, 2, nchar(t)))
  t
}

.ars_sap_prose_rows <- function(s, docname) {
  notes <- character(0)
  mp <- .ars_sap_methods_paragraphs(s)
  if (!mp$anchored) {
    notes <- c(notes, paste0(
      "REVIEW: no 'Statistical Methods/Analyses' heading found in '", docname,
      "'; prose was scanned across the whole document (expect more noise)."))
  }
  sents <- .ars_split_sentences(mp$paragraphs)
  domrx <- .ars_prose_domain_rx()
  rows <- list()
  seen <- character(0)
  for (sen in sents) {
    m <- regexpr(.ARS_DISPLAY_VERB, sen, ignore.case = TRUE, perl = TRUE)
    if (m < 0) next
    subject <- trimws(substr(sen, 1, m - 1))
    if (!nzchar(subject) || nchar(subject) > 200) next
    # gate: the SUBJECT must name a data domain (rejects stats-detail lines)
    if (!grepl(domrx, subject, ignore.case = TRUE, perl = TRUE)) next
    # reject a subject LED by a statistic ("LS mean difference ... will be ..."):
    # the display is the statistic, not the domain mentioned downstream
    if (grepl(paste0("^(?:the|a|an)?\\s*(?:ls |least[- ]squares |geometric |",
                     "arithmetic )?(?:mean|median|proportion|percentage|",
                     "number|incidence|rate|standard deviation|correlation|",
                     "hazard ratio|odds ratio|p[- ]?values?|",
                     "confidence intervals?)\\b"),
             subject, ignore.case = TRUE, perl = TRUE)) next
    dtype <- .ars_guess_display_type(sen)
    key <- paste(dtype, gsub("[^a-z]+", "", tolower(subject)))
    if (key %in% seen) next
    seen <- c(seen, key)
    rows[[length(rows) + 1L]] <- data.frame(
      toc_no = NA_character_, output_id = NA_character_,
      title = .ars_prose_title(subject), display_type = dtype,
      section_key = NA_character_, population = NA_character_,
      group_by = NA_character_, groups = NA_character_, where = NA_character_,
      source = sen, stringsAsFactors = FALSE)
  }
  if (length(rows) == 0) {
    notes <- c(notes, paste0(
      "REVIEW: no planned-display prose found in '", docname, "'. No sentence ",
      "named a data domain with a display verb (e.g. '... will be summarized ",
      "by treatment group'). If the display list lives in a TFL specification ",
      "rather than the SAP, start from ars_toc_template() instead."))
    return(list(toc = NULL, notes = notes))
  }
  toc <- do.call(rbind, rows)
  notes <- c(notes, paste0(
    "Drafted ", nrow(toc), " display row(s) from SAP prose",
    if (mp$anchored) " (Statistical Methods section)" else "",
    "; check each row's `source` sentence."))
  list(toc = toc, notes = notes)
}
