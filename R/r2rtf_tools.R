#' Encode Table to RTF Format with Advanced Features
#'
#' This function encodes a table object into Rich Text Format (RTF) with advanced
#' formatting features including pagination, headers, footers, and sublines.
#' Based on the r2rtf package functionality by Wang et al. (2020).
#'
#' @param tbl A table object with RTF attributes. The table should have been
#'   prepared with appropriate RTF formatting attributes using other r2rtf functions.
#' @param verbose A logical value indicating whether to return detailed RTF
#'   components separately. If TRUE, returns a list with individual RTF sections.
#'   If FALSE (default), returns a condensed list with start, body, and end sections.
#'
#' @return If verbose = FALSE, returns a list with three elements:
#'   \describe{
#'     \item{start}{Character string containing RTF initialization code}
#'     \item{body}{Character string containing the main RTF table content}
#'     \item{end}{Character string containing RTF closing code}
#'   }
#'
#'   If verbose = TRUE, returns a detailed list with elements:
#'   \describe{
#'     \item{start}{RTF initialization code}
#'     \item{page}{Page formatting code}
#'     \item{margin}{Margin settings}
#'     \item{header}{Header/title text}
#'     \item{subline}{Subline text}
#'     \item{sublineby}{Subline by grouping variable}
#'     \item{colheader}{Column headers for each page}
#'     \item{body}{Table body content for each page}
#'     \item{footnote}{Footnote text for each page}
#'     \item{source}{Source text for each page}
#'     \item{end}{RTF closing code}
#'     \item{info}{Page and grouping information}
#'   }
#'
#' @details
#' This function performs comprehensive RTF encoding of a table object with the following features:
#' \itemize{
#'   \item Updates first and last border formatting
#'   \item Handles multi-page table layouts with proper pagination
#'   \item Manages page titles, footnotes, and source notes positioning
#'   \item Supports grouping variables with subline headers
#'   \item Applies consistent column headers across pages
#'   \item Integrates fonts, colors, and margin settings
#' }
#'
#' The function expects the input table to have specific RTF attributes set by
#' other r2rtf functions such as page settings, formatting options, and content
#' specifications.
#'
#' Page elements positioning:
#' \itemize{
#'   \item Page titles can be placed on "first", "last", or "all" pages
#'   \item Footnotes can be placed on "first", "last", or "all" pages
#'   \item Source notes can be placed on "first", "last", or "all" pages
#' }
#'
#' @import r2rtf
#'
#' @references
#' Wang, S., Ye, S., Anderson, K., & Zhang, Y. (2020). r2rtf—an R Package to
#' Produce Rich Text Format (RTF) Tables and Figures. PharmaSUG.
#' \url{https://pharmasug.org/proceedings/2020/DV/PharmaSUG-2020-DV-198.pdf}
#'
#' @seealso
#' \code{\link{rtf_encode_table}} for simplified RTF encoding
#'
#' @export
#' @examples
#' \dontrun{
#' # Assume 'tbl' is a properly formatted table object with RTF attributes
#'
#' # Basic encoding
#' rtf_result <- rtf_encode_table2(tbl)
#' writeLines(paste(rtf_result$start, rtf_result$body, rtf_result$end), "output.rtf")
#'
#' # Verbose encoding for debugging
#' rtf_detailed <- rtf_encode_table2(tbl, verbose = TRUE)
#' str(rtf_detailed)
#'
#' # Check page information
#' print(rtf_detailed$info)
#' }
rtf_encode_table2 <- function(tbl, verbose = FALSE) {
  # Update First and Last Border
  tbl_1 <- r2rtf:::update_border_first(tbl)
  tbl_1 <- r2rtf:::update_border_last(tbl_1)

  # Get content
  page <- attr(tbl, "page")
  pageby <- attr(tbl, "rtf_pageby")

  start_rtf <- paste(

    r2rtf:::as_rtf_init(),
    r2rtf:::as_rtf_font(),
    r2rtf:::as_rtf_color(tbl),
    sep = "\n"
  )

  ## get rtf code for page, margin, header, footnote, source, new_page
  page_rtftext <- r2rtf:::as_rtf_page(tbl)
  margin_rtftext <- r2rtf:::as_rtf_margin(tbl)
  header_rtftext <- r2rtf:::as_rtf_title(tbl)
  subline_rtftext <- r2rtf:::as_rtf_subline(tbl)


  new_page_rtftext <- r2rtf:::as_rtf_new_page()

  ## rtf encode for column header
  colheader_rtftext_1 <- paste(unlist(r2rtf:::as_rtf_colheader(tbl_1)), collapse = "\n") # First page
  colheader_rtftext <- paste(unlist(r2rtf:::as_rtf_colheader(tbl)), collapse = "\n") # Rest of page

  ## rtf encode for footnote
  footnote_rtftext_1 <- paste(r2rtf:::as_rtf_footnote(tbl_1), collapse = "\n") # Last page
  footnote_rtftext <- paste(r2rtf:::as_rtf_footnote(tbl), collapse = "\n") # Rest of pages

  ## rtf encode for source
  source_rtftext_1 <- paste(r2rtf:::as_rtf_source(tbl_1), collapse = "\n") # Last page
  source_rtftext <- paste(r2rtf:::as_rtf_source(tbl), collapse = "\n") # Rest of pages

  ## RTF encode for table body
  if (is.null(pageby$by_var)) {
    table_rtftext <- as_rtf_table(tbl_1)
  } else {
    table_rtftext <- r2rtf:::as_rtf_pageby(tbl_1)
  }

  ## RTF encoding for subline_by row
  info <- attr(table_rtftext, "info")

  if (is.null(attr(tbl, "rtf_by_subline")$by_var)) {
    sublineby_rtftext <- NULL
  } else {
    info_dict <- unique(info[, c("subline", "page")])
    sublineby_index <- as.numeric(factor(info_dict$subline, levels = unique(info_dict$subline)))

    sublineby_rtftext <- r2rtf:::as_rtf_paragraph(attr(tbl, "rtf_by_subline_row"), combine = FALSE)

    if (!is.null(dim(sublineby_rtftext))) {
      sublineby_rtftext <- apply(sublineby_rtftext, 1, paste, collapse = "\n")
    }

    sublineby_rtftext <- sublineby_rtftext[sublineby_index]
  }

  # if (pageby$new_page) {
  #   body_rtftext <- tapply(table_rtftext, paste0(info$id, info$page), FUN = function(x) paste(x, collapse = "\n"))
  # } else {
  body_rtftext <- tapply(table_rtftext, info$page, FUN = function(x) paste(x, collapse = "\n"))
  # }

  n_page <- length(body_rtftext)

  # Page Title Display Location
  if (page$page_title == "first") {
    if (!is.null(header_rtftext)) header_rtftext <- c(header_rtftext, rep("", n_page - 1))
    if (!is.null(subline_rtftext)) subline_rtftext <- c(subline_rtftext, rep("", n_page - 1))
  }

  if (page$page_title == "last") {
    if (!is.null(header_rtftext)) header_rtftext <- c(rep("", n_page - 1), header_rtftext)
    if (!is.null(subline_rtftext)) subline_rtftext <- c(rep("", n_page - 1), subline_rtftext)
  }

  # Title RTF encoding by page

  # Footnote RTF encoding by page
  footnote_rtftext <- switch(page$page_footnote,
                             first = c(footnote_rtftext, rep("", n_page - 1)),
                             last  = c(rep("", n_page - 1), footnote_rtftext_1),
                             all   = c(rep(footnote_rtftext, n_page - 1), footnote_rtftext_1)
  )

  # Source RTF encoding by page
  source_rtftext <- switch(page$page_source,
                           first = c(source_rtftext, rep("", n_page - 1)),
                           last  = c(rep("", n_page - 1), source_rtftext_1),
                           all   = c(rep(source_rtftext, n_page - 1), source_rtftext_1)
  )


  # Combine RTF body encoding
  rtf_feature <- paste(
    page_rtftext,
    margin_rtftext,
    header_rtftext,
    subline_rtftext,
    sublineby_rtftext,
    c(colheader_rtftext_1, rep(colheader_rtftext, n_page - 1)),
    body_rtftext,
    footnote_rtftext,
    source_rtftext,
    c(rep(new_page_rtftext, n_page - 1), ""),
    sep = "\n"
  )

  rtf_feature <- paste(unlist(rtf_feature), collapse = "\n")

  ## Post Processing for total page number
  rtf_feature <- gsub("\\totalpage", n_page, rtf_feature, fixed = TRUE) # total page number

  end <- r2rtf:::as_rtf_end()
  if (verbose) {
    rtf <- list(
      start = start_rtf,
      page = page_rtftext,
      margin = margin_rtftext,
      header = header_rtftext,
      subline = subline_rtftext,
      sublineby = sublineby_rtftext,
      colheader = c(colheader_rtftext_1, rep(colheader_rtftext, n_page - 1)),
      body = body_rtftext,
      footnote = footnote_rtftext,
      source = source_rtftext,
      end = end,
      info = info
    )
  } else {
    rtf <- list(start = start_rtf, body = rtf_feature, end = end)
  }

  rtf
}

as_rtf_table <- function(tbl) {
  # Remove subline_by column
  if (!is.null(attr(tbl, "rtf_by_subline")$by_var)) {
    index_subline <- which(names(tbl) %in% attr(tbl, ("rtf_by_subline"))$by_var)
    tbl <- r2rtf:::rtf_subset(tbl, col = -index_subline)
  }

  # Calculate Number of rows for each entry.
  tbl <- r2rtf:::rtf_nrow(tbl)

  # tbl attributes
  page <- attr(tbl, "page")
  group_by <- attr(tbl, "rtf_groupby")
  col_width <- attr(tbl, "page")$col_width

  # Get number of row for each entry
  rtf_nrow <- attr(tbl, "rtf_nrow_meta")

  # Number of rows in cells based on column size
  cell_tbl <- tbl

  # Number of rows in cells based on column size
  index <- 1:nrow(cell_tbl)

  # Number of row in each table entry
  if (is.null(attr(cell_tbl, "cell_nrow"))) {
    table_nrow <- attr(cell_tbl, "rtf_nrow")
  } else {
    table_nrow <- attr(cell_tbl, "cell_nrow")
  }

  rtf_nrow_body <- rtf_nrow
  if (page$page_title != "all") rtf_nrow_body$title <- 0
  if (page$page_footnote != "all") rtf_nrow_body$footnote <- 0
  if (page$page_source != "all") rtf_nrow_body$source <- 0

  # Page Dictionary
  page_dict <- data.frame(
    index = index,
    nrow = table_nrow,
    total = rtf_nrow$page - sum(rtf_nrow_body[-1]),
    stringsAsFactors = FALSE
  )

  if (!is.null(attr(tbl, "rtf_by_subline")$id)) {
    page_dict$id <- attr(tbl, "rtf_by_subline")$id
    page_dict$subline <- attr(tbl, "rtf_by_subline")$id
    page_dict$page <- unlist(lapply(split(page_dict, page_dict$id), page_dict_page))
    page_dict$page <- as.numeric(page_dict$id) * 1e6 + page_dict$page
  } else {
    page_dict$page <- r2rtf:::page_dict_page(page_dict)
  }

  # Move to next page for footnote and data source
  total_all <- rtf_nrow$page - sum(rtf_nrow[-1])
  if (sum(page_dict$nrow[page_dict$page == tail(page_dict$page, 1)]) > total_all) {
    page_dict$page[c(-2:0) + nrow(page_dict)] <- page_dict$page[c(-2:0) + nrow(page_dict)] + 1
  }

  # Remove repeated records if group_by is not null
  if (!is.null(group_by)) {
    cell_tbl <- r2rtf:::rtf_group_by_enhance(cell_tbl,
                                     group_by = group_by,
                                     page_index = page_dict$page
    )
  }

  # Add border type for first and last row
  page_dict_first <- do.call(rbind, lapply(split(page_dict, page_dict$page), function(x) x[1, ]))
  page_dict_last <- do.call(rbind, lapply(split(page_dict, page_dict$page), function(x) x[nrow(x), ]))

  if (!is.null(attr(cell_tbl, "border_first"))) {
    attr(cell_tbl, "border_top")[page_dict_first$index, ] <- attr(cell_tbl, "border_first")[page_dict_first$index, ]
  }

  if (!is.null(attr(cell_tbl, "border_last"))) {
    attr(cell_tbl, "border_bottom")[page_dict_last$index, ] <- attr(cell_tbl, "border_last")[page_dict_last$index, ]
  }

  if (!is.null(attr(cell_tbl, "border_color_first"))) {
    attr(cell_tbl, "border_color_top")[page_dict_first$index, ] <- attr(cell_tbl, "border_color_first")[page_dict_first$index, ]
  }

  if (!is.null(attr(cell_tbl, "border_color_last"))) {
    attr(cell_tbl, "border_color_bottom")[page_dict_last$index, ] <- attr(cell_tbl, "border_color_last")[page_dict_last$index, ]
  }

  # RTF encode
  rtf_table <- rtf_table_content(cell_tbl,
                                 col_total_width = col_width,
                                 use_border_bottom = TRUE
  )

  rtf <- apply(rtf_table, 2, paste, collapse = "\n")

  attr(rtf, "info") <- page_dict
  rtf
}



rtf_table_content <- function(tbl,
                              col_total_width = attr(tbl, "page")$col_width,
                              use_border_bottom = FALSE) {
  border_left <- attr(tbl, "border_left")
  border_right <- attr(tbl, "border_right")
  border_top <- attr(tbl, "border_top")
  border_bottom <- attr(tbl, "border_bottom")

  border_color_left <- attr(tbl, "border_color_left")
  border_color_right <- attr(tbl, "border_color_right")
  border_color_top <- attr(tbl, "border_color_top")
  border_color_bottom <- attr(tbl, "border_color_bottom")

  border_width <- attr(tbl, "border_width")

  col_rel_width <- attr(tbl, "col_rel_width")
  cell_height <- attr(tbl, "cell_height")
  cell_justification <- attr(tbl, "cell_justification")
  cell_vertical_justification <- attr(tbl, "cell_vertical_justification")

  text_font <- attr(tbl, "text_font")
  text_format <- attr(tbl, "text_format")
  text_color <- attr(tbl, "text_color")
  text_background_color <- attr(tbl, "text_background_color")
  text_justification <- attr(tbl, "text_justification")
  text_font_size <- attr(tbl, "text_font_size")
  text_space <- attr(tbl, "text_space")
  text_space_before <- attr(tbl, "text_space_before")
  text_space_after <- attr(tbl, "text_space_after")

  text_indent_first <- attr(tbl, "text_indent_first")
  text_indent_left <- attr(tbl, "text_indent_left")
  text_indent_right <- attr(tbl, "text_indent_right")



  text_convert <- attr(tbl, "text_convert")

  ## get dimension of tbl
  n_row <- nrow(tbl)
  n_col <- ncol(tbl)

  ## Ensure Missing value display nothing
  for (i in 1:n_col) {
    tbl[, i] <- ifelse(is.na(tbl[, i]), "", as.character(tbl[, i]))
  }

  ## Transfer vector to matrix by row
  foo <- function(x) {
    if ((is.null(dim(x))) & (!is.null(x))) {
      x <- matrix(x, nrow = n_row, ncol = n_col, byrow = TRUE)
    }
    x
  }

  # Encoding RTF Cell Justification
  justification <- r2rtf:::justification()
  cell_justification_rtf <- factor(cell_justification, levels = justification$type, labels = justification$rtf_code_row)

  vertical_justification <- r2rtf:::vertical_justification()
  cell_vertical_justification <- factor(cell_vertical_justification, levels = vertical_justification$type, labels = vertical_justification$rtf_code)

  cell_height <- round(r2rtf:::inch_to_twip(cell_height) / 2, 0)

  # rtf code for table begin and end
  row_begin <- paste0("\\trowd\\trgaph", cell_height, "\\trleft0", cell_justification_rtf)
  row_end <- "\\intbl\\row\\pard"

  # Encoding RTF Cell Border type and width
  border_lrtb <- c("\\clbrdrl", "\\clbrdrr", "\\clbrdrt", "\\clbrdrb")
  names(border_lrtb) <- c("left", "right", "top", "bottom")
  border_wid <- paste0("\\brdrw", border_width)

  ## cell border type
  border_type <- r2rtf:::border_type()
  border_left_rtf <- factor(border_left, levels = border_type$name, labels = border_type$rtf_code)
  border_right_rtf <- factor(border_right, levels = border_type$name, labels = border_type$rtf_code)
  border_top_rtf <- factor(border_top, levels = border_type$name, labels = border_type$rtf_code)
  border_bottom_rtf <- factor(border_bottom, levels = border_type$name, labels = border_type$rtf_code)

  border_left_rtf <- paste0(border_lrtb["left"], border_left_rtf, border_wid)
  border_right_rtf <- paste0(border_lrtb["right"], border_right_rtf, border_wid)
  border_top_rtf <- paste0(border_lrtb["top"], border_top_rtf, border_wid)
  border_bottom_rtf <- paste0(border_lrtb["bottom"], border_bottom_rtf, border_wid)

  ## Encoding RTF Cell Border color
  col_tb <- r2rtf:::color_table()

  if (!is.null(border_color_left)) {
    border_color_left_rtf <- factor(border_color_left, levels = col_tb$color, labels = col_tb$type)
    border_color_left_rtf <- paste0("\\brdrcf", border_color_left_rtf)
    border_left_rtf <- paste0(border_left_rtf, border_color_left_rtf)
  }

  if (!is.null(border_color_right)) {
    border_color_right_rtf <- factor(border_color_right, levels = col_tb$color, labels = col_tb$type)
    border_color_right_rtf <- paste0("\\brdrcf", border_color_right_rtf)
    border_right_rtf <- paste0(border_right_rtf, border_color_right_rtf)
  }

  if (!is.null(border_color_top)) {
    border_color_top_rtf <- factor(border_color_top, levels = col_tb$color, labels = col_tb$type)
    border_color_top_rtf <- paste0("\\brdrcf", border_color_top_rtf)
    border_top_rtf <- paste0(border_top_rtf, border_color_top_rtf)
  }

  if (!is.null(border_color_bottom)) {
    border_color_bottom_rtf <- factor(border_color_bottom, levels = col_tb$color)
    levels(border_color_bottom_rtf) <- col_tb$type
    border_color_bottom_rtf <- paste0("\\brdrcf", border_color_bottom_rtf)
    border_bottom_rtf <- paste0(border_bottom_rtf, border_color_bottom_rtf)
  }

  ## Cell Background Color
  if (!is.null(text_background_color)) {
    text_background_color_rtf <- factor(text_background_color, levels = col_tb$color)
    levels(text_background_color_rtf) <- col_tb$type
    text_background_color_rtf <- paste0("\\clcbpat", text_background_color_rtf)
  } else {
    text_background_color_rtf <- NULL
  }

  # Cell Size
  cell_width <- r2rtf:::cell_size(col_rel_width, col_total_width)
  cell_size <- cumsum(cell_width)
  cell_size <- foo(cell_size)

  # Delete borders if they are not set
  border_left_rtf[as.vector(t(border_left)) == ""] <- ""
  border_right_rtf[as.vector(t(border_right)) == ""] <- ""
  border_top_rtf[as.vector(t(border_top)) == ""] <- ""
  border_bottom_rtf[as.vector(t(border_bottom)) == ""] <- ""


  # Combine Cell Attributes of cell justification, cell border type, cell border width, cell border color, cell background color and cell size.
  border_top_left <- matrix(paste0(border_left_rtf, border_top_rtf, text_background_color_rtf, cell_vertical_justification, "\\cellx", cell_size), nrow = n_row, ncol = n_col)
  border_top_left_right <- matrix(paste0(border_left_rtf, border_top_rtf, border_right_rtf, text_background_color_rtf, cell_vertical_justification, "\\cellx", cell_size), nrow = n_row, ncol = n_col)
  border_top_left_bottom <- matrix(paste0(border_left_rtf, border_top_rtf, border_bottom_rtf, text_background_color_rtf, cell_vertical_justification, "\\cellx", cell_size), nrow = n_row, ncol = n_col)
  border_all <- matrix(paste0(border_left_rtf, border_top_rtf, border_right_rtf, border_bottom_rtf, text_background_color_rtf, cell_vertical_justification, "\\cellx", cell_size), nrow = n_row, ncol = n_col)

  if (use_border_bottom) {
    border_rtf <- border_top_left_bottom
    border_rtf[, n_col] <- border_all[, n_col]
  } else {
    border_rtf <- border_top_left
    border_rtf[, n_col] <- border_top_left_right[, n_col]
  }

  border_rtf <- t(border_rtf)

  # Encode RTF Text and Paragraph
  use_i18n <- attr(tbl, "page")$use_i18n %||% attr(tbl, "use_i18n") %||% FALSE
  text_rtf <- r2rtf:::rtf_text(tbl,
                       font = text_font,
                       font_size = text_font_size,
                       format = text_format,
                       color = text_color,
                       background_color = text_background_color,
                       text_convert = text_convert
                       #, use_i18n = use_i18n
  )

  cell_rtf <- r2rtf:::rtf_paragraph(text_rtf,
                            justification = text_justification,
                            indent_first = text_indent_first,
                            indent_left = text_indent_left,
                            indent_right = text_indent_right,
                            space = text_space,
                            space_before = text_space_before,
                            space_after = text_space_after,
                            new_page = FALSE,
                            hyphenation = FALSE,
                            cell = TRUE
  )

  rbind(row_begin, border_rtf, t(cell_rtf), row_end)
}

