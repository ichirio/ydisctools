# ============================================================================
#  ars_toc.R -- the TFL TOC workbook: hub of the SAP -> TOC -> mock -> ARS
#  chain (ydisctools issue #18)
# ============================================================================
#
#  ars_toc_template()  write a TOC workbook pre-filled with the standard
#                      safety display set (recipe keys) + a custom example
#  ars_from_toc()      expand the TOC into compact parameters (recipes from
#                      inst/ars-display-recipes/), auto-numbering toc_no
#                      from a section map (ICH E3 defaults) -- ready for
#                      build_ars() / ars_generate_ard()
# ============================================================================

# -- recipe catalog / section map ---------------------------------------------

.ars_recipe_catalog <- function() {
  path <- system.file("ars-display-recipes", "recipes.json",
                      package = "ydisctools", mustWork = TRUE)
  cat_json <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  recipes <- cat_json$recipes
  names(recipes) <- vapply(recipes, function(r) r$id, character(1))
  recipes
}

# ICH E3 section-14 style defaults.  Sponsor conventions differ, so every
# consumer takes a `section_map` argument to override this.
.ars_default_section_map <- function() {
  c(baseline = "14.1", efficacy = "14.2",
    safety_ae = "14.3.1", safety_lab = "14.3.4")
}

# Assign toc_no where missing: explicit numbers are respected; rows without
# one get "<prefix>.<seq>" from their section key, where <seq> continues
# after the largest "<prefix>.<int>" already present among the explicit
# numbers (deterministic rule-based numbering).
.ars_assign_toc_no <- function(toc_no, section_key, section_map) {
  unknown <- setdiff(unique(section_key), names(section_map))
  if (length(unknown) > 0) {
    stop("Unknown section_key value(s): ", paste(unknown, collapse = ", "),
         ". Known keys: ", paste(names(section_map), collapse = ", "),
         " (extend via the `section_map` argument).", call. = FALSE)
  }
  prefixes <- unname(section_map[section_key])
  counters <- list()
  for (p in unique(prefixes)) {
    rx <- paste0("^", gsub(".", "\\.", p, fixed = TRUE), "\\.([0-9]+)$")
    hits <- regmatches(toc_no[!is.na(toc_no)],
                       regexec(rx, toc_no[!is.na(toc_no)], perl = TRUE))
    nums <- vapply(hits, function(m) {
      if (length(m) == 2) as.integer(m[2]) else NA_integer_
    }, integer(1))
    counters[[p]] <- max(c(0L, nums), na.rm = TRUE)
  }
  for (i in seq_along(toc_no)) {
    if (!is.na(toc_no[i])) next
    p <- prefixes[i]
    counters[[p]] <- counters[[p]] + 1L
    toc_no[i] <- paste0(p, ".", counters[[p]])
  }
  toc_no
}

# -- ars_toc_template ----------------------------------------------------------

#' Write a TFL TOC workbook template
#'
#' Creates the TOC workbook that drives the TOC-centred ARS chain (see
#' [ars_from_toc()]): one row per planned display, referencing a **display
#' recipe** (the bundled catalog of standard safety displays transcribed from
#' the FDA Standard Safety Tables & Figures Integrated Guide and the PHUSE
#' Analyses & Displays white papers) or \code{"custom"} for hand-authored
#' analyses.  The template is pre-filled with the standard safety set and one
#' custom efficacy example.
#'
#' @section Numbering:
#' \code{toc_no} may be left blank: [ars_from_toc()] assigns
#' \code{<section prefix>.<sequence>} numbers from each display's section key
#' using an ICH E3 section-14-style default map (baseline = 14.1,
#' efficacy = 14.2, adverse events = 14.3.1, labs = 14.3.4).  Real numbering
#' conventions are sponsor-specific -- normally assigned by biostatistics
#' when the TFL specification is created, anchored to the company CSR
#' template rather than inherited from the SAP -- so explicit \code{toc_no}
#' values always win and the section map is overridable
#' (\code{section_map} argument of [ars_from_toc()]).
#'
#' @param path File path of the workbook to write (\code{.xlsx}).
#' @param overwrite Logical; overwrite an existing file? Default \code{FALSE}.
#'
#' @return The path, invisibly.
#'
#' @seealso [ars_from_toc()]; the recipe catalog README in
#'   \code{system.file("ars-display-recipes", package = "ydisctools")}.
#'
#' @examples
#' tmp <- file.path(tempdir(), "toc.xlsx")
#' ars_toc_template(tmp, overwrite = TRUE)
#' readxl::read_excel(tmp, sheet = "TOC")
#'
#' @export
ars_toc_template <- function(path, overwrite = FALSE) {
  if (file.exists(path) && !overwrite) {
    stop("File already exists: '", path, "'. Set overwrite = TRUE to replace.",
         call. = FALSE)
  }
  study <- data.frame(
    key   = c("study_id", "study_title"),
    value = c("STUDY01", "Example Study"),
    stringsAsFactors = FALSE
  )
  toc <- data.frame(
    toc_no       = c("", "", "", "", "", "", "", ""),
    output_id    = c("Out_dm", "Out_ds", "Out_ex", "Out_ae_ov",
                     "Out_ae_soc", "Out_ae_sev", "Out_ae_pt", "Out_eff"),
    title        = c("", "", "", "", "", "", "",
                     "Primary Efficacy Analysis"),
    display_type = c("dm_summary", "disposition", "exposure", "ae_overview",
                     "ae_soc", "ae_severity", "ae_pt", "custom"),
    section_key  = c("", "", "", "", "", "", "", "efficacy"),
    population   = "SAFFL",
    group_by     = "TRT01A",
    groups       = "",
    where        = "",
    stringsAsFactors = FALSE
  )
  toc$population[toc$output_id == "Out_eff"] <- "ITTFL"
  # hand-authored analyses for the custom display (>= 3 per output; see
  # ?build_ars "Sizing note")
  analyses <- data.frame(
    output_id   = "Out_eff",
    analysis_id = "",
    name        = c("Number of subjects", "Primary endpoint",
                    "Responders, n (%)"),
    method      = c("total_n", "continuous_summary", "categorical_summary"),
    dataset     = c("ADSL", "ADEFF", "ADEFF"),
    variable    = c("USUBJID", "AVAL", "USUBJID"),
    population  = "",
    group_by    = c("", "", "TRT01A, CRIT1FL"),
    groups      = "",
    groups2     = "",
    where       = c("", "PARAMCD EQ PRIMEP", "PARAMCD EQ PRIMEP"),
    denominator = c("", "", "auto"),
    stringsAsFactors = FALSE
  )
  writexl::write_xlsx(list(Study = study, TOC = toc, Analyses = analyses),
                      path)
  invisible(path)
}

# -- ars_from_toc ---------------------------------------------------------------

#' Expand a TFL TOC workbook into compact ARS parameters
#'
#' The TOC workbook (see [ars_toc_template()]) is the hub of the
#' SAP -> TOC -> mock -> ARS chain: one row per planned display.  Rows with a
#' catalogued \code{display_type} are expanded through the bundled display
#' recipes (the TOC row supplies the study specifics: population, arm
#' grouping, pre-defined arm levels, extra \code{where} conditions); rows
#' with \code{display_type = "custom"} take their hand-authored analyses from
#' the workbook's \code{Analyses} sheet.  Missing \code{toc_no} values are
#' auto-numbered from the section map (see [ars_toc_template()]'s Numbering
#' section), and each display's number and title become ARS \code{Displays}
#' Title subsections (\code{"Table <toc_no>"} + title).
#'
#' @param toc Path to a TOC workbook, or a named list with elements
#'   \code{toc} (the TOC data frame) and optionally \code{study},
#'   \code{analyses} (custom rows) and \code{displays} (extra display
#'   furniture, as in [ars_param_template()]'s \code{Displays} sheet).
#' @param section_map Named character vector mapping section keys to number
#'   prefixes; default [ICH E3-style] \code{c(baseline = "14.1",
#'   efficacy = "14.2", safety_ae = "14.3.1", safety_lab = "14.3.4")}.
#'   Override to follow a sponsor CSR template's numbering.
#'
#' @return A compact parameter list (\code{study}, \code{outputs},
#'   \code{analyses}, \code{displays}, plus the resolved \code{toc}) ready
#'   for [build_ars()] / [ars_generate_ard()] / [write_ars_params()].
#'
#' @seealso [ars_toc_template()], [build_ars()], [ars_generate_ard()]
#'
#' @examples
#' tmp <- file.path(tempdir(), "toc.xlsx")
#' ars_toc_template(tmp, overwrite = TRUE)
#' params <- ars_from_toc(tmp)
#' params$toc[, c("toc_no", "output_id", "display_type")]
#' head(params$analyses[, c("output_id", "analysis_id", "method", "group_by")])
#'
#' @export
ars_from_toc <- function(toc, section_map = NULL) {
  if (is.character(toc) && length(toc) == 1) {
    if (!file.exists(toc)) {
      stop("TOC workbook not found: '", toc, "'.", call. = FALSE)
    }
    sheets <- readxl::excel_sheets(toc)
    if (!"TOC" %in% sheets) {
      stop("TOC workbook is missing the 'TOC' sheet.", call. = FALSE)
    }
    as_chr_df <- function(df) {
      out <- as.data.frame(df, stringsAsFactors = FALSE)
      out[] <- lapply(out, function(col) {
        col <- as.character(col)
        col[!is.na(col) & !nzchar(trimws(col))] <- NA_character_
        col
      })
      out
    }
    toc <- list(
      toc      = as_chr_df(readxl::read_excel(toc, sheet = "TOC")),
      study    = if ("Study" %in% sheets) {
        as_chr_df(readxl::read_excel(toc, sheet = "Study"))
      },
      analyses = if ("Analyses" %in% sheets) {
        as_chr_df(readxl::read_excel(toc, sheet = "Analyses"))
      },
      displays = if ("Displays" %in% sheets) {
        as_chr_df(readxl::read_excel(toc, sheet = "Displays"))
      }
    )
  }
  if (!is.list(toc) || is.null(toc$toc)) {
    stop("`toc` must be a TOC workbook path or a list with a `toc` element.",
         call. = FALSE)
  }

  tt <- .ars_ensure_cols(as.data.frame(toc$toc, stringsAsFactors = FALSE),
                         c("toc_no", "output_id", "title", "display_type",
                           "section_key", "population", "group_by", "groups",
                           "where"))
  tt[] <- lapply(tt, function(col) {
    col <- as.character(col)
    col[!is.na(col) & !nzchar(trimws(col))] <- NA_character_
    col
  })
  if (nrow(tt) == 0) stop("The TOC has no rows.", call. = FALSE)
  if (anyNA(tt$output_id) || anyDuplicated(tt$output_id)) {
    stop("Every TOC row needs a unique `output_id`.", call. = FALSE)
  }

  catalog <- .ars_recipe_catalog()
  bad <- setdiff(tt$display_type, c(names(catalog), "custom"))
  if (anyNA(tt$display_type) || length(bad) > 0) {
    stop("Unknown `display_type`: ",
         paste(unique(bad), collapse = ", "),
         ". Available: ", paste(names(catalog), collapse = ", "),
         ", custom.", call. = FALSE)
  }
  for (i in seq_len(nrow(tt))) {
    if (is.na(tt$population[i]) || is.na(tt$group_by[i])) {
      stop("TOC row ", i, " (", tt$output_id[i],
           "): `population` and `group_by` are required.", call. = FALSE)
    }
  }

  # titles: default from the recipe
  for (i in seq_len(nrow(tt))) {
    if (is.na(tt$title[i])) {
      if (tt$display_type[i] == "custom") {
        stop("TOC row ", i, " (", tt$output_id[i],
             "): a custom display needs a `title`.", call. = FALSE)
      }
      tt$title[i] <- catalog[[tt$display_type[i]]]$title_default
    }
  }

  # numbering: explicit toc_no wins; the rest by section key + section map
  section_map <- if (is.null(section_map)) {
    .ars_default_section_map()
  } else {
    c(section_map, .ars_default_section_map()[
      setdiff(names(.ars_default_section_map()), names(section_map))])
  }
  key <- tt$section_key
  for (i in seq_len(nrow(tt))) {
    if (!is.na(key[i])) next
    key[i] <- if (tt$display_type[i] == "custom") {
      "efficacy"
    } else {
      catalog[[tt$display_type[i]]]$section_key
    }
  }
  tt$section_key <- key
  tt$toc_no <- .ars_assign_toc_no(tt$toc_no, tt$section_key, section_map)

  # outputs sheet: one output per TOC row (analyses inherit the defaults)
  outputs <- data.frame(
    output_id = tt$output_id, name = tt$title,
    population = tt$population, group_by = tt$group_by,
    groups = ifelse(is.na(tt$groups), "", tt$groups),
    stringsAsFactors = FALSE
  )

  # analyses: recipe expansion + custom passthrough
  custom <- if (!is.null(toc$analyses)) {
    as.data.frame(toc$analyses, stringsAsFactors = FALSE)
  }
  rows <- list()
  for (i in seq_len(nrow(tt))) {
    oid <- tt$output_id[i]
    if (tt$display_type[i] == "custom") {
      if (is.null(custom) || !any(custom$output_id == oid)) {
        stop("TOC row ", i, " (", oid, "): display_type is 'custom' but the ",
             "Analyses sheet has no rows for it.", call. = FALSE)
      }
      rows[[oid]] <- custom[custom$output_id == oid, , drop = FALSE]
      next
    }
    recipe <- catalog[[tt$display_type[i]]]
    ana <- lapply(recipe$analyses, function(a) {
      grp2 <- if (!is.null(a$group2)) paste0(", ", a$group2) else ""
      wh <- c(if (!is.null(a$where)) a$where,
              if (!is.na(tt$where[i])) tt$where[i])
      data.frame(
        output_id = oid, analysis_id = NA_character_,
        name = a$name, method = a$method, dataset = a$dataset,
        variable = a$variable,
        population = NA_character_,
        group_by = paste0(tt$group_by[i], grp2),
        groups = NA_character_, groups2 = NA_character_,
        where = if (length(wh)) paste(wh, collapse = "; ") else NA_character_,
        denominator = if (!is.null(a$denominator)) a$denominator else
          NA_character_,
        stringsAsFactors = FALSE
      )
    })
    rows[[oid]] <- do.call(rbind, ana)
  }
  analyses <- do.call(rbind, rows)
  row.names(analyses) <- NULL
  blank_id <- is.na(analyses$analysis_id) |
    !nzchar(trimws(as.character(analyses$analysis_id)))
  analyses$analysis_id[blank_id] <- sprintf("An_%02d", which(blank_id))

  # displays: "Table <toc_no>" + title per output; user-provided rows for an
  # output take precedence (no auto rows for that output)
  user_disp <- if (!is.null(toc$displays)) {
    as.data.frame(toc$displays, stringsAsFactors = FALSE)
  }
  auto_disp <- do.call(rbind, lapply(seq_len(nrow(tt)), function(i) {
    if (!is.null(user_disp) && any(user_disp$output_id == tt$output_id[i])) {
      return(NULL)
    }
    data.frame(
      output_id = tt$output_id[i],
      section_type = "Title", order = 1:2,
      text = c(paste0("Table ", tt$toc_no[i]), tt$title[i]),
      stringsAsFactors = FALSE
    )
  }))
  displays <- rbind(auto_disp, user_disp)

  list(study = toc$study, outputs = outputs, analyses = analyses,
       displays = displays, toc = tt)
}
