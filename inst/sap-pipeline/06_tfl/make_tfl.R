# ============================================================================
#  make_tfl.R -- the STUDY01 table programme: ARD -> formatted tables -> RTF
# ============================================================================
#
#  Run FROM THE PIPELINE ROOT (inst/sap-pipeline), e.g.
#
#      Rscript 06_tfl/make_tfl.R
#
#  Inputs (earlier pipeline stages):
#      01_toc/TOC_STUDY01.xlsx    the TFL TOC workbook -- display order,
#                                 titles, populations, analysis metadata
#      05_ard/ARD_STUDY01.csv     the combined study-level ARD
#
#  Output:
#      06_tfl/TFL_STUDY01.rtf     every display, one per landscape page
#
#  The programme is deliberately generic: it never mentions an individual
#  display.  The TOC (expanded through the same recipes the ARS came from)
#  says WHAT each display contains, the ARD holds the numbers, and three
#  small renderers -- big-N header, continuous block, categorical block --
#  turn every analysis into rows via ydisctools::format_stats() +
#  pivot_stats_wider().  rtfreporter writes the final document.
# ============================================================================

library(ydisctools)

# the sponsor numbering convention, derived from the company CSR template at
# TFL-specification time (stage 1: read_csr_map() on sample_csr_template.docx)
acme_map <- c(baseline = "15.1", efficacy = "15.2",
              safety_ae = "15.3.1", safety_lab = "15.3.4")

params <- ars_from_toc("01_toc/TOC_STUDY01.xlsx", section_map = acme_map)
ard <- utils::read.csv("05_ard/ARD_STUDY01.csv", stringsAsFactors = FALSE)
ard[ard == ""] <- NA   # blank csv cells back to NA

arms <- trimws(strsplit(params$toc$groups[1], "|", fixed = TRUE)[[1]])
pop_label <- c(SAFFL = "Safety Analysis Set",
               ITTFL = "Intent-to-Treat Analysis Set")
stub_label <- c(dm_summary = "Characteristic", disposition = "Disposition",
                exposure = "Exposure", ae_overview = "Category",
                ae_soc = "System Organ Class", ae_pt = "Preferred Term",
                ae_severity = "Severity", custom = "Parameter")

# -- renderers: one ARD analysis -> formatted display rows ---------------------

# wide (label + one column per arm) -> stub/value rows in fixed arm order
finish_block <- function(wide, indent = "  ") {
  for (a in arms) if (!a %in% names(wide)) wide[[a]] <- NA_character_
  wide <- as.data.frame(wide)[, c("label", arms)]
  wide$label <- paste0(indent, wide$label)
  wide
}

# n / Mean (SD) / Median / Q1, Q3 / Min, Max block of a continuous analysis
render_continuous <- function(a) {
  lbl <- c(N = "n", mean = "Mean (SD)", sd = "Mean (SD)", median = "Median",
           p25 = "Q1, Q3", p75 = "Q1, Q3", min = "Min, Max", max = "Min, Max")
  d <- a[a$stat_name %in% names(lbl),
         c("group1_level", "stat_name", "stat")]
  d <- data.frame(arm = d$group1_level, label = unname(lbl[d$stat_name]),
                  stat = d$stat_name, value = d$stat)
  specs <- data.frame(
    label    = c("n", "Mean (SD)", "Median", "Q1, Q3", "Min, Max"),
    template = c("{N:0}", "{mean:1} ({sd:2})", "{median:1}",
                 "{p25:1}, {p75:1}", "{min:0}, {max:0}"))
  wide <- format_stats(d, specs, stat_col = "stat", value_col = "value") |>
    pivot_stats_wider(expand_cols = "arm")
  finish_block(wide)
}

# n (%) rows of a categorical analysis; `flat` analyses (no category
# variable -- the ARD carries the constant '.flag_') count subjects per arm
# and render as a single row
render_categorical <- function(a, name) {
  d <- a[a$stat_name %in% c("n", "p"), , drop = FALSE]
  flat <- all(d$variable == ".flag_")
  d <- data.frame(
    arm   = d$group1_level,
    label = if (flat) name else d$variable_level,
    stat  = d$stat_name,
    value = ifelse(d$stat_name == "p", d$stat * 100, d$stat))
  specs <- data.frame(label = unique(d$label), template = "{n:0} ({p:1})")
  wide <- format_stats(d, specs, stat_col = "stat", value_col = "value") |>
    pivot_stats_wider(expand_cols = "arm")
  wide <- finish_block(wide, indent = if (flat) "" else "  ")
  wide[is.na(wide)] <- "0"
  wide
}

# -- assemble one display ------------------------------------------------------

render_display <- function(toc_row) {
  an <- params$analyses[params$analyses$output_id == toc_row$output_id, ]
  out_ard <- ard[ard$OutputId == toc_row$output_id, ]

  # big N (the total_n analysis) feeds the column headers
  big_n <- setNames(rep(NA_integer_, length(arms)), arms)
  blocks <- list()
  for (j in seq_len(nrow(an))) {
    a <- out_ard[out_ard$AnalysisId == an$analysis_id[j], , drop = FALSE]
    if (an$method[j] == "total_n") {
      nn <- a[a$stat_name == "n", ]
      big_n[nn$variable_level] <- nn$stat
      next
    }
    block <- if (an$method[j] == "continuous_summary") {
      render_continuous(a)
    } else {
      render_categorical(a, an$name[j])
    }
    # a label row above indented statistic rows
    if (!all(startsWith(block$label, an$name[j]))) {
      head_row <- block[0, ]
      head_row[1, ] <- c(an$name[j], rep("", length(arms)))
      block <- rbind(head_row, block)
    }
    blocks[[j]] <- block
  }
  body <- do.call(rbind, blocks)
  names(body) <- c(
    stub_label[[toc_row$display_type]],
    paste0(arms, "\n(N=", ifelse(is.na(big_n), "?", big_n), ")"))
  row.names(body) <- NULL

  list(titles = c(paste0("Table ", toc_row$toc_no),
                  toc_row$title,
                  pop_label[[toc_row$population]]),
       body = body)
}

displays <- lapply(seq_len(nrow(params$toc)),
                   function(i) render_display(params$toc[i, ]))

# -- write the RTF (one display per landscape page) ----------------------------

pages <- lapply(displays, function(d) {
  rtfreporter::rtftable(d$body, col_header = names(d$body), row_title = 1L)
})
titles <- lapply(displays, function(d) as.list(d$titles))
doc <- rtfreporter::rtf_document(
  page = list(paper_size = "letter", orientation = "landscape"))
doc <- rtfreporter::rtf_section(doc, page = 1,
                                secinfo = list(header = NULL, footer = NULL))
doc <- rtfreporter::rtf_tables(doc, pages, titles = titles)
rtfreporter::generate_rtfreport(doc, "06_tfl/TFL_STUDY01.rtf",
                                overwrite = TRUE)

cat("Rendered", length(displays), "displays to 06_tfl/TFL_STUDY01.rtf\n")
