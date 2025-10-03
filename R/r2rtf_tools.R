#' Encode Tables and Figures to RTF Format
#'
#' This function provides a unified interface for encoding tables and figures
#' into Rich Text Format (RTF) with customizable page layout options.
#' Supports both single tables/figures and lists of multiple objects.
#'
#' @param tbl A table object (data.frame with RTF attributes), figure object,
#'   or list of such objects to be encoded as RTF.
#' @param doc_type A character string specifying the document type.
#'   Must be either "table" or "figure". Default is "table".
#' @param page_title A character string specifying on which pages to display
#'   the title. Options are "all", "first", or "last". Default is "all".
#' @param page_footnote A character string specifying on which pages to display
#'   the footnote. Options are "all", "first", or "last". Default is "all".
#' @param page_source A character string specifying on which pages to display
#'   the source note. Options are "all", "first", or "last". Default is "last".
#' @param verbose A logical value indicating whether to return detailed RTF
#'   components. If TRUE, returns verbose output with individual sections.
#'   Default is FALSE.
#'
#' @return For tables: Returns RTF encoded list with start, body, and end
#'   components. For figures: Returns RTF encoded figure object.
#'   The exact structure depends on the input type and verbose setting.
#'
#' @details
#' This function serves as a dispatcher for different RTF encoding methods:
#' \itemize{
#'   \item For data.frame objects: Uses \code{rtf_encode_table()}
#'   \item For list objects: Uses \code{r2rtf:::rtf_encode_list()}
#'   \item For figure objects: Uses \code{rtf_encode_figure()}
#' }
#'
#' The function automatically sets page layout attributes based on the provided
#' parameters before calling the appropriate encoding method.
#'
#' Page element positioning:
#' \itemize{
#'   \item "all": Element appears on every page
#'   \item "first": Element appears only on the first page
#'   \item "last": Element appears only on the last page
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
#' \code{\link{rtf_encode_table2}} for advanced table encoding,
#' \code{\link{rtf_encode_table3}} for enhanced table encoding
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic table encoding
#' tbl <- data.frame(
#'   Treatment = c("Placebo", "Drug A"),
#'   N = c(25, 30),
#'   Response = c(5, 12)
#' ) %>%
#'   rtf_body()
#'
#' # Encode with default settings
#' rtf_result <- rtf_encode(tbl)
#'
#' # Customize page layout
#' rtf_result <- rtf_encode(
#'   tbl,
#'   page_title = "first",
#'   page_footnote = "last",
#'   page_source = "last"
#' )
#'
#' # Encode a figure
#' fig <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' rtf_fig <- rtf_encode(fig, doc_type = "figure")
#'
#' # Write to RTF file
#' rtf_result %>% write_rtf("output.rtf")
#' }
rtf_encode <- function(tbl,
                       doc_type = "table",
                       page_title = "all",
                       page_footnote = "all",
                       page_source = "last",
                       verbose = FALSE) {
  r2rtf:::match_arg(doc_type, c("table", "figure"))
  r2rtf:::match_arg(page_title, c("all", "first", "last"))
  r2rtf:::match_arg(page_footnote, c("all", "first", "last"))
  r2rtf:::match_arg(page_source, c("all", "first", "last"))

  if (doc_type == "table") {
    if (any(class(tbl) %in% "list")) {
      for (i in 1:length(tbl)) {
        attr(tbl[[i]], "page")$page_title <- page_title
        attr(tbl[[i]], "page")$page_footnote <- page_footnote
        attr(tbl[[i]], "page")$page_source <- page_source
      }

      warning("List of tables is not yet supported. This feature will be added in a future version. Currently calling r2rtf:::rtf_encode_list().")
      return(r2rtf:::rtf_encode_list(tbl))
    }

    if (any(class(tbl) %in% "data.frame")) {
      attr(tbl, "page")$page_title <- page_title
      attr(tbl, "page")$page_footnote <- page_footnote
      attr(tbl, "page")$page_source <- page_source

      return(rtf_encode_table(tbl, verbose = verbose))
    }
  }

  if (doc_type == "figure") {
    attr(tbl, "page")$page_title <- page_title
    attr(tbl, "page")$page_footnote <- page_footnote
    attr(tbl, "page")$page_source <- page_source
    return(rtf_encode_figure(tbl))
  }
}

#####  ******************************************************************
rtf_encode_table3 <- function(tbl, verbose = FALSE, first_page = FALSE) {
  rtf_encode_table(tbl, verbose = verbose)
}

rtf_encode_table <- function(tbl, verbose = FALSE) {
  # Update First and Last Border
  tbl_1 <- r2rtf:::update_border_first(tbl)
  tbl_1 <- r2rtf:::update_border_last(tbl_1)

  # Get content
  page <- attr(tbl, "page")
  pageby <- attr(tbl, "rtf_pageby")

  start_rtf <- paste(

    as_rtf_init(),
    as_rtf_font(),
    r2rtf:::as_rtf_color(tbl),
    as_rtf_page(tbl),
    sep = "\n"
  )

  ## get rtf code for page, margin, header, footnote, source, new_page
  page_rtftext        <- as_rtf_section(tbl)
  page_header_rtftext <- as_rtf_header(tbl)
  page_footer_rtftext <- as_rtf_footer(tbl)
  header_rtftext      <- r2rtf:::as_rtf_title(tbl)
  subline_rtftext     <- r2rtf:::as_rtf_subline(tbl)


  new_page_rtftext <- r2rtf:::as_rtf_new_page()

  ## rtf encode for column header
  colheader_rtftext_1 <- paste(unlist(as_rtf_colheader(tbl_1)), collapse = "\n") # First page
  colheader_rtftext <- paste(unlist(as_rtf_colheader(tbl)), collapse = "\n") # Rest of page

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

  table_rtftext <- as_rtf_blank_rows(tbl, table_rtftext)
  pos <- attr(tbl, "rtf_blank_rows")
  pages <- info$page
  if(!is.null(pos) & length(pos) > 0) {
    pages <- insert_elements(pages, pos)
  }

  # if (pageby$new_page) {
  #   body_rtftext <- tapply(table_rtftext, paste0(info$id, info$page), FUN = function(x) paste(x, collapse = "\n"))
  # } else {
  body_rtftext <- tapply(table_rtftext, pages, FUN = function(x) paste(x, collapse = "\n"))
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
    page_header_rtftext,
    page_footer_rtftext,
    header_rtftext,
    subline_rtftext,
    sublineby_rtftext,
    c(colheader_rtftext_1, rep(colheader_rtftext, n_page - 1)),
    body_rtftext,
    footnote_rtftext,
    source_rtftext,
    c(rep(new_page_rtftext, n_page - 1), ""),
    sep = ""
    # sep = "\n"
  )

  rtf_feature <- paste(unlist(rtf_feature), collapse = "\n")

  ## Post Processing for total page number
  rtf_feature <- gsub("\\totalpage", n_page, rtf_feature, fixed = TRUE) # total page number

  end <- r2rtf:::as_rtf_end()
  if (verbose) {
    rtf <- list(
      start = start_rtf,
      page = page_rtftext,
      page_header = page_header_rtftext,
      page_footer = page_footer_rtftext,
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

#これから修正
rtf_encode_list <- function(tbl) {
  # Page Input checking
  page <- lapply(tbl, function(x) attr(x, "page"))
  width <- length(unique(lapply(page, function(x) x$width))) > 1
  height <- length(unique(lapply(page, function(x) x$height))) > 1
  orientation <- length(unique(lapply(page, function(x) x$orientation))) > 1
  use_color <- length(unique(lapply(page, function(x) x$use_color))) > 1

  if (width) {
    stop("Page width must be the same")
  }
  if (height) {
    stop("Page height must be the same")
  }
  if (orientation) {
    stop("Page orientation must be the same")
  }

  page_title <- unlist(unique(lapply(page, function(x) x$page_title)))
  page_footnote <- unlist(unique(lapply(page, function(x) x$page_footnote)))
  page_source <- unlist(unique(lapply(page, function(x) x$page_source)))
  if (length(page_title) > 1) {
    stop("Table title location must be the same")
  }
  if (length(page_footnote) > 1) {
    stop("Table footnote location must be the same")
  }
  if (length(page_source) > 1) {
    stop("Table source location must be the same")
  }
  if (page_title != "all") {
    stop("Only page_title = 'all' is supported in list")
  }
  if (page_footnote != "last") {
    stop("Only page_footnote = 'last' is supported in list")
  }
  if (page_source != "last") {
    stop("Only page_source = 'last' is supported in list")
  }

  # Number of tbls
  n <- length(tbl)
  if (n < 2) {
    stop("The length of input list must >= 2")
  }

  # Footnote and Data Source
  tbl[2:n] <- lapply(tbl[2:n], function(x) {
    if (!is.null(attr(x, "rtf_footnote"))) {
      message("Only rtf_footnote in first item is used")
    }

    if (!is.null(attr(x, "rtf_source"))) {
      message("Only rtf_source in first item is used")
    }

    attr(x, "page")$nrow <- attr(tbl[[1]], "page")$nrow
    x
  })

  if (n > 2) {
    tbl[2:(n - 1)] <- lapply(tbl[2:(n - 1)], function(x) {
      attr(x, "page")$border_first <- NULL
      attr(x, "page")$border_last <- NULL
      attr(x, "page")$border_color_first <- NULL
      attr(x, "page")$border_color_last <- NULL
      x
    })
  }

  attr(tbl[[1]], "page")$border_last <- NULL
  attr(tbl[[1]], "page")$border_color_last <- NULL
  attr(tbl[[1]], "page")$use_color <- use_color
  attr(tbl[[n]], "page")$border_first <- NULL
  attr(tbl[[n]], "page")$border_color_first <- NULL
  attr(tbl[[n]], "rtf_footnote") <- attr(tbl[[1]], "rtf_footnote")
  attr(tbl[[n]], "rtf_source") <- attr(tbl[[1]], "rtf_source")
  attr(tbl[[1]], "rtf_footnote") <- NULL
  attr(tbl[[1]], "rtf_source") <- NULL

  # Split page if necessary
  item <- 0
  iter <- 0
  item_next <- tbl[[1]]
  while (nrow(item_next) > 0) {
    if (item == 0) {
      # Render first time
      encode <- lapply(tbl, rtf_encode_table, verbose = TRUE)
    } else {
      index <- item_next$index[item_next$page1 == item_next$page1[1]]

      tbl0 <- list()
      tbl0[[1]] <- rtf_subset(tbl[[item - iter]], row = index)
      tbl0[[2]] <- rtf_subset(tbl[[item - iter]], row = -index)

      # Update border
      attr(tbl0[[1]], "page")$border_last <- NULL
      attr(tbl0[[1]], "page")$border_color_last <- NULL
      attr(tbl0[[1]], "rtf_title") <- NULL
      attr(tbl0[[1]], "rtf_footnote") <- NULL
      attr(tbl0[[1]], "rtf_source") <- NULL
      encode0 <- c(encode[1:(item - 1)], lapply(tbl0, rtf_encode_table, verbose = TRUE))

      if (item < length(encode)) {
        encode0 <- c(encode0, encode[(item + 1):n])
      }

      encode <- encode0

      iter <- iter + 1
    }

    # Split page
    info <- list()
    for (i in 1:length(encode)) {
      info[[i]] <- data.frame(item = i, encode[[i]]$info)
    }
    info <- do.call(rbind, info)

    info$total <- min(info$total)

    info$page1 <- r2rtf::page_dict_page(info)

    page1 <- info[info$page == 1, ]
    page1 <- split(page1, page1$item)
    item1 <- which(unlist(lapply(page1, function(x) length(unique(x$page1)))) > 1)[1]
    if (is.na(item1)) {
      item1 <- 0
    }
    item_next <- info[info$item == item1 & info$page == 1, ]
    item <- item1
  }


  start <- encode[[1]]$start
  new_page_rtftext <- r2rtf:::as_rtf_new_page()


  body <- lapply(encode, function(x) {
    n_page <- length(x$body)
    paste(
      x$page,
      x$margin,
      x$header,
      x$subline,
      x$sublineby,
      x$colheader,
      x$body,
      x$footnote,
      x$source,
      c(rep(new_page_rtftext, n_page - 1), ""),
      sep = "\n"
    )
  })

  # add page break
  break_index <- lapply(split(info, info$page1), function(x) {
    unique(x$item)[-length(unique(x$item))]
  })
  break_index <- sort(unique(unlist(break_index)))

  page_break <- rep(new_page_rtftext, length(body))
  page_break[c(break_index, length(body))] <- ""

  for (i in 1:length(body)) {
    body[[i]] <- c(body[[i]], page_break[i])
  }
  body <- paste(unlist(body), collapse = "\n")
  rtf <- list(start = start, body = body, end = as_rtf_end())

  rtf
}

rtf_encode_figure <- function(tbl) {
  page <- attr(tbl, "page")

  start_rtf <- paste(
    as_rtf_init(),
    as_rtf_font(),
    r2rtf:::as_rtf_color(tbl),
    as_rtf_page(tbl),
    sep = "\n"
  )

  # Footnote always be free text in figures
  footnote <- attr(tbl, "rtf_footnote")

  if (!is.null(footnote)) {
    attr(footnote, "as_table") <- FALSE
    attr(tbl, "rtf_footnote") <- footnote
  }

  ## get rtf code for page, margin, header, footnote, source, new_page
  page_rtftext   <- as_rtf_section(tbl)
  page_header_rtftext <- as_rtf_header(tbl)
  page_footer_rtftext <- as_rtf_footer(tbl)

  header_rtftext <- r2rtf:::as_rtf_title(tbl)
  subline_rtftext <- r2rtf:::as_rtf_subline(tbl)
  footnote_rtftext <- r2rtf:::as_rtf_footnote(tbl)
  source_rtftext <- r2rtf:::as_rtf_source(tbl)
  new_page_rtftext <- r2rtf:::as_rtf_new_page()

  ## get rtf code for figure width and height
  fig_width <- attr(tbl, "fig_width")
  fig_height <- attr(tbl, "fig_height")

  ## get rtf code for figure format
  fig_format <- r2rtf:::fig_format()
  fig_format <- factor(attr(tbl, "fig_format"), levels = fig_format$type, labels = fig_format$rtf_code)

  rtf_fig <- paste0(
    "{\\pict", fig_format, "\\picwgoal",
    round(fig_width * 1440), "\\pichgoal",
    round(fig_height * 1440), " ", lapply(tbl, paste, collapse = ""), "}"
  )

  n_page <- length(tbl)
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
                             last  = c(rep("", n_page - 1), footnote_rtftext),
                             all   = rep(footnote_rtftext, n_page)
  )

  # Source RTF encoding by page
  source_rtftext <- switch(page$page_source,
                           first = c(source_rtftext, rep("", n_page - 1)),
                           last  = c(rep("", n_page - 1), source_rtftext),
                           all   = rep(source_rtftext, n_page)
  )

  rtf_feature <- paste(
    page_rtftext,
    page_header_rtftext,
    page_footer_rtftext,
    header_rtftext,
    subline_rtftext,
    rtf_fig,
    r2rtf:::rtf_paragraph(""), # new line after figure
    footnote_rtftext,
    source_rtftext,
    c(rep(new_page_rtftext, length(rtf_fig) - 1), ""),
    sep = ""
  )

  rtf_feature <- paste(rtf_feature, collapse = "\n")

  ## Post Processing
  rtf_feature <- gsub("\\totalpage", n_page, rtf_feature, fixed = TRUE) # total page number

  list(start = start_rtf, body = rtf_feature, end = r2rtf:::as_rtf_end())
}

as_rtf_blank_rows <- function(tbl, table_rtftext) {
  blank_rows <- attr(tbl, "rtf_blank_rows")

  if(!is.null(blank_rows)) {
    col_width <- round(attr(tbl, "page")$col_width[1] * 1440, 0)
    text_font <- attr(tbl, "text_font")[1]
    text_font_size <- attr(tbl, "text_font_size")[1]
    cell_height <- round(r2rtf:::inch_to_twip(attr(tbl, "cell_height")[1]) / 2, 0)
    cell_height_trrh <- attr(tbl, "cell_height_trrh")[1]
    text_space_before <- attr(tbl, "text_space_before")[1]
    text_space_after <- attr(tbl, "text_space_after")[1]


    if (!is.null(cell_height_trrh)) cell_height_trrh <- paste0("\\trrh", cell_height_trrh)
    else cell_height_trrh <- ""

    attr(tbl, "cell_height")[1]


    rtf_text <- paste0(
      "\\trowd\\trgaph", cell_height, "\\trleft0\\trqc", cell_height_trrh, "\n",
      "\\clvertalt\\cellx", col_width, "\n",
      "\\pard\\hyphpar0\\sb", text_space_before, "\\sa", text_space_after, "\\li0\\ri0\\qc\\fs", text_font_size * 2, "{\\f", text_font, "  }\\cell\n",
      "\\intbl\\row\\pard\n"
    )

    table_rtftext <- insert_text(table_rtftext, blank_rows, rtf_text)
  }

  return(table_rtftext)
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

  cell_height_trrh <- attr(tbl, "cell_height_trrh")


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
  if (!is.null(cell_height_trrh)) {
    row_begin <- paste0(row_begin, "\\trrh", cell_height_trrh)
  }
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
  # border_left_rtf[as.vector(t(border_left)) == ""] <- ""
  # border_right_rtf[as.vector(t(border_right)) == ""] <- ""
  # border_top_rtf[as.vector(t(border_top)) == ""] <- ""
  # border_bottom_rtf[as.vector(t(border_bottom)) == ""] <- ""

  border_left_rtf[as.vector(border_left) == ""] <- ""
  border_right_rtf[as.vector(border_right) == ""] <- ""
  border_top_rtf[as.vector(border_top) == ""] <- ""
  border_bottom_rtf[as.vector(border_bottom) == ""] <- ""


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

as_rtf_colheader <- function(tbl) {
  rtf_colheader <- attr(tbl, "rtf_colheader")

  rtf_code <- lapply(rtf_colheader, rtf_table_content,
                     use_border_bottom = TRUE,
                     col_total_width = attr(tbl, "page")$col_width
  )

  unlist(rtf_code)
}


# RTF commantds generators
as_rtf_init <- function ()
{
  paste0("{", "\\rtf1\\ansi\\ansicpg65001\\uc1\\deff0\\deflang1033\\deflangfe1041")
}

as_rtf_font <- function ()
{
  font_type  <- font_type()
  font_rtf   <- factor(c(1:10), levels = font_type$type, labels = font_type$rtf_code)
  font_style <- factor(c(1:10), levels = font_type$type, labels = font_type$style)
  font_name  <- factor(c(1:10), levels = font_type$type, labels = font_type$name)
  char_set   <- factor(c(1:10), levels = font_type$type, labels = font_type$char_set)
  font_table <- paste0(
    "{\\fonttbl",
    paste(paste0("{", font_rtf, font_style, char_set, "\\fprq2 ", font_name, ";}\n"), collapse = ""),
    "}"
  )
  font_table
}

font_type <- function ()
{
  data.frame(
    type = 1:10,
    name = c(
      "Times New Roman",
      "Times New Roman Greek",
      "Arial Greek",
      "Arial",
      "Helvetica",
      "Georgia",
      "Cambria",
      "Courier New",
      "Courier",
      "Symbol"
    ),
    style = c(
      "\\froman",
      "\\froman",
      "\\fswiss",
      "\\fswiss",
      "\\fswiss",
      "\\froman",
      "\\ffroman",
      "\\fmodern",
      "\\fmodern",
      "\\ftech"
    ),
    rtf_code = c(
      "\\f0",
      "\\f1",
      "\\f2",
      "\\f3",
      "\\f4",
      "\\f5",
      "\\f6",
      "\\f7",
      "\\f8",
      "\\f9"
    ),
    family = c(
      "Times",
      "Times",
      "ArialMT",
      "ArialMT",
      "Helvetica",
      "Georgia",
      "Cambria",
      "Courier",
      "Courier",
      "Times"
    ),
    char_set = c(
      "\\fcharset0",
      "\\fcharset161",
      "\\fcharset161",
      "\\fcharset0",
      "\\fcharset0",
      "\\fcharset0",
      "\\fcharset0",
      "\\fcharset0",
      "\\fcharset0",
      "\\fcharset0"
    ),
    stringsAsFactors = FALSE
  )
}

as_rtf_page <- function (tbl)
{
  page <- attr(tbl, "page")
  page_size <- c("\\paperw", "\\paperh")
  page_size <- paste(paste0(page_size, r2rtf:::inch_to_twip(c(page$width,
                                                              page$height))), collapse = "")
  if (page$orientation == "landscape") {
    page_size <- paste0(page_size, "\\landscape\n")
  }
  if (page$orientation == "portrait") {
    page_size <- paste0(page_size, "\n")
  }

  margin <- c("\\margl", "\\margr", "\\margt", "\\margb")
  margin <- paste(paste0(margin, r2rtf:::inch_to_twip(page$margin[1:4])),
                  collapse = "")
  margin <- paste0(margin, "\n")

  paste0(page_size, margin)
}

as_rtf_section <- function (tbl)
{
  page <- attr(tbl, "page")

  section <- "\\sectd\\linex0\\endnhere"

  sec_page_size <- c("\\pgwsxn", "\\pghsxn")
  sec_page_size <- paste(paste0(sec_page_size, r2rtf:::inch_to_twip(c(page$width,
                                                                      page$height))), collapse = "")
  if (page$orientation == "landscape") {
    sec_page_size <- paste0(sec_page_size, "\\lndscpsxn\n")
  }
  if (page$orientation == "portrait") {
    sec_page_size <- paste0(sec_page_size, "\n")
  }

  sec_margin <- c("\\marglsxn", "\\margrsxn", "\\margtsxn", "\\margbsxn", "\\headery", "\\footery")
  sec_margin <- paste(paste0(sec_margin, r2rtf:::inch_to_twip(page$margin)),
                      collapse = "")
  sec_margin <- paste0(sec_margin, "\n")

  paste0(section, sec_page_size, sec_margin)
}

as_rtf_header <- function(tbl) {
  encode <- ""
  if (!is.null(attr(tbl, "page_header_tbl"))) {
    encode <- paste("{\\header\\pard\\plain\\ql",
                    as_rtf_header_tbl(tbl, attr(tbl, "page_header_tbl")),
                    "}", sep = "\n")
  } else if (!is.null(attr(tbl, "rtf_page_header_direct"))) {
    encode <- paste("{\\header\\pard\\plain\\ql", attr(tbl, "rtf_page_header_direct"), "}", sep = "\n")
  } else if (!is.null(attr(tbl, "rtf_page_header"))) {
    encode <- c("{\\header", r2rtf:::as_rtf_paragraph(attr(tbl, "rtf_page_header"),
                                                      combine = FALSE), "}")
    encode <- paste(encode, collapse = "\n")
  }
  encode
}

as_rtf_footer <- function(tbl) {
  encode <- ""
  if (!is.null(attr(tbl, "page_footer_tbl"))) {
    encode <- paste("{\\footer\\pard\\plain\\ql",
                    as_rtf_footer_tbl(tbl, attr(tbl, "page_footer_tbl")),
                    "}", sep = "\n")
  } else if (!is.null(attr(tbl, "rtf_page_footer_direct"))) {
    encode <- paste("{\\footer\\pard\\plain\\ql", attr(tbl, "rtf_page_footer_direct"), "}", sep = "\n")
  } else  if (!is.null(attr(tbl, "rtf_page_footer"))) {
    encode <- c("{\\footer", r2rtf:::as_rtf_paragraph(attr(tbl, "rtf_page_footer"),
                                                      combine = FALSE), "}")
    encode <- paste(encode, collapse = "\n")
  }
  encode
}

as_rtf_new_page <- function() {
  "\\pard\n\\sect"
  # paste("{\\pard\\fs2\\par}\\page{\\pard\\fs2\\par}")
}



#公開する関数（作成中）
rtf_page_header_tbl <- function(tbl,
                                page_header_tbl,
                                text_font = 1,
                                text_font_size = 9,
                                col_width = NULL,
                                cell_height = NULL) {
  attr(tbl, "page_header_tbl") <- page_header_tbl
  attr(tbl, "page_header_tbl_font") <- text_font
  attr(tbl, "page_header_tbl_size") <- text_font_size
  attr(tbl, "page_header_col_width") <- col_width
  attr(tbl, "page_header_tbl_gap")  <- cell_height
  tbl
}

rtf_page_footer_tbl <- function(tbl,
                                page_footer_tbl,
                                text_font = 1,
                                text_font_size = 9,
                                col_width = NULL,
                                cell_height = NULL) {
  attr(tbl, "page_footer_tbl") <- page_footer_tbl
  attr(tbl, "page_footer_tbl_font") <- text_font
  attr(tbl, "page_footer_tbl_size") <- text_font_size
  attr(tbl, "page_footer_col_width") <- col_width
  attr(tbl, "page_footer_tbl_gap")  <- cell_height
  tbl
}

rtf_footer_tbl <- function(tbl, footer_tbl) {
  attr(tbl, "footer_tbl") <- footer_tbl
  tbl
}

as_rtf_header_tbl <- function(tbl, texts, width = NULL) {
  as_rtf_hf_tbl(tbl, texts, "page_header", width)
}

as_rtf_footer_tbl <- function(tbl, texts, width = NULL) {
  as_rtf_hf_tbl(tbl, texts, "page_footer", width)
}

as_rtf_hf_tbl <- function(tbl, texts, module = "page_header", width = NULL) {
  cell_height <- attr(tbl, paste0(module, "_tbl_gap"))
  text_font <- attr(tbl, paste0(module, "_tbl_font"))
  text_font_size <- attr(tbl, paste0(module, "_tbl_size"))
  col_width <- attr(tbl, paste0(module, "_col_width"))

  if(!is.null(cell_height))
    cell_height <- round(r2rtf:::inch_to_twip(cell_height) / 2, 0)
  else
    cell_height <- 0

  page <- attr(tbl, "page")
  if(is.null(col_width)) {
    if(!is.null(page)) {
      col_width <- attr(tbl, "page")$col_width
      if (is.null(col_width)) {
        col_width <- 10
      }
    }
  }

  if(!is.null(width)) col_width <- width

  cellx2 <- round(col_width * 1440, 0)
  cellx1 <- round(cellx2 / 2, 0)


  replace_page <- function(text) {
    result <- gsub("__PAGE__", "{\\field{\\*\\fldinst { PAGE }}}", text, fixed = TRUE)
    result <- gsub("__NUMPAGES__", "{\\field{\\*\\fldinst { NUMPAGES }}}", result, fixed = TRUE)
    result
  }

  rtf_rows <- lapply(texts, function(vec) {
    if(length(vec) == 2) {
      left_text  <- if ("l" %in% names(vec)) vec["l"] else vec[1]
      left_text <- replace_page(left_text)
      right_text <- if ("r" %in% names(vec)) vec["r"] else vec[2]
      right_text <- replace_page(right_text)



      rtf_text <- paste0(
        "{\\trowd\\trgaph", cell_height, "\\trleft0\n",
        "\\cellx", cellx1, "\\cellx", cellx2, "\n",
        "\\intbl\\ql\\f", text_font, "\\fs", text_font_size * 2, " ", left_text, "\\cell\n",
        "\\intbl\\qr\\f", text_font, "\\fs", text_font_size * 2, " ", right_text, "\\cell\n",
        "\\row}"
      )
    }else if (length(vec) == 1) {
      text <- if ("c" %in% names(vec)) vec["c"] else vec[1]
      text <- replace_page(text)

      # get alignment from names (l, c, r)
      align <- if ("l" %in% names(vec)) "\\ql" else if ("r" %in% names(vec)) "\\qr" else "\\qc"

      rtf_text <- paste0(
        "{\\trowd\\trgaph", cell_height, "\\trleft0\n",
        "\\cellx", cellx2, "\n",
        "\\intbl", align, "\\f", text_font, "\\fs", text_font_size * 2, " ", text, "\\cell\n",
        "\\row}"
      )
    } else {
      stop("texts must be a list of character vector of length 1 or 2.")
    }
    rtf_text
  })

  return(paste(rtf_rows, collapse = "\n"))
}






get_paper_dimension <- function(paper_size = "letter",
                                unit = "inch") {

  # 用紙サイズの定義（mm単位、portrait基準）
  paper_sizes <- list(
    # ISO A series
    "a0" = c(width = 841, height = 1189),
    "a1" = c(width = 594, height = 841),
    "a2" = c(width = 420, height = 594),
    "a3" = c(width = 297, height = 420),
    "a4" = c(width = 210, height = 297),
    "a5" = c(width = 148, height = 210),
    "a6" = c(width = 105, height = 148),

    # US paper sizes
    "letter" = c(width = 215.9, height = 279.4),
    "legal" = c(width = 215.9, height = 355.6),
    "tabloid" = c(width = 279.4, height = 431.8),
    "ledger" = c(width = 431.8, height = 279.4),

    # Other common sizes
    "b4" = c(width = 250, height = 353),
    "b5" = c(width = 176, height = 250),
    "executive" = c(width = 184.2, height = 266.7),
    "folio" = c(width = 210, height = 330),
    "quarto" = c(width = 215, height = 275)
  )

  # 入力値の正規化
  paper_size <- tolower(paper_size)
  unit <- tolower(unit)

  # 用紙サイズの確認
  if (!paper_size %in% names(paper_sizes)) {
    stop("Unsupported paper size: ", paper_size,
         "\nSupported sizes: ", paste(names(paper_sizes), collapse = ", "))
  }

  # 基本サイズの取得（portrait固定）
  base_size <- paper_sizes[[paper_size]]

  # 単位変換
  converted_size <- convert_units(base_size, from = "mm", to = unit)

  # 長さ2の名前なしベクトルとして返す
  return(round(as.numeric(converted_size), 2))
}

#' Convert Units
#'
#' @param values Numeric vector of values
#' @param from Source unit
#' @param to Target unit
#' @return Converted values
convert_units <- function(values, from = "mm", to = "mm") {

  # mm を基準とした変換係数
  conversion_factors <- list(
    "mm" = 1,
    "cm" = 0.1,
    "inch" = 0.0393701,
    "in" = 0.0393701,
    "pt" = 2.83465,
    "twip" = 56.6929
  )

  from <- tolower(from)
  to <- tolower(to)

  if (!from %in% names(conversion_factors)) {
    stop("Unsupported source unit: ", from)
  }

  if (!to %in% names(conversion_factors)) {
    stop("Unsupported target unit: ", to)
  }

  # 単位変換
  if (from == to) {
    return(values)
  }

  # mmから目的単位への変換
  if (from == "mm") {
    result <- values * conversion_factors[[to]]
  } else if (to == "mm") {
    result <- values / conversion_factors[[from]]
  } else {
    # mm経由で変換
    mm_values <- values / conversion_factors[[from]]
    result <- mm_values * conversion_factors[[to]]
  }

  return(result)
}

insert_text <- function(target, pos, text) {
  # posを昇順にソートして、後ろから挿入する（インデックスのズレを防ぐため）
  sorted_indices <- order(pos, decreasing = TRUE)

  result <- target

  for (i in sorted_indices) {
    position <- pos[i]

    if (position == 0) {
      # 最初に挿入
      result <- c(text, result)
    } else if (position <= length(result)) {
      # 指定位置の後ろに挿入
      if (position == length(result)) {
        # 最後に追加する場合
        result <- c(result, text)
      } else {
        # 途中に挿入する場合
        result <- c(result[1:position], text, result[(position + 1):length(result)])
      }
    } else {
      # 位置が範囲外の場合は最後に追加
      result <- c(result, text)
    }
  }

  return(result)
}

insert_elements <- function(page, pos) {
  # posを降順にソートして後ろから処理（インデックスのズレを防ぐ）
  sorted_indices <- order(pos, decreasing = TRUE)

  result <- page

  for (i in sorted_indices) {
    position <- pos[i]

    if (position == 0) {
      # 最初に1番目の要素をコピー
      result <- c(result[1], result)
    } else if (position <= length(result)) {
      # 指定位置の要素をその直後にコピー
      element_to_copy <- result[position]

      if (position == length(result)) {
        # 最後の要素の場合
        result <- c(result, element_to_copy)
      } else {
        # 途中の要素の場合
        result <- c(result[1:position], element_to_copy, result[(position + 1):length(result)])
      }
    }
  }

  return(result)
}


assemble_rtf <- function(input,
                         output,
                         landscape = FALSE) {
  # input checking
  check_args(input, type = "character")
  check_args(output, type = "character", length = 1)

  # define variables
  input <- normalizePath(input)
  n_input <- length(input)
  missing_input <- input[!file.exists(input)]
  ext_output <- tolower(tools::file_ext(output))

  # input checking
  check_args(landscape, "logical", length = 1)
  match_arg(ext_output, "rtf")

  # warning missing input
  if (length(missing_input) > 0) {
    warning("Missing files: \n", paste(missing_input, collapse = "\n"))
    input <- setdiff(input, missing_input)
  }

  # assemble RTF
  rtf <- lapply(input, readLines)
  n <- length(rtf)
  start <- c(1, vapply(rtf[-1], function(x) max(grep("margl", rtf[[1]])) + 2, numeric(1)))
  end <- vapply(rtf, length, numeric(1))
  end[-n] <- end[-n] - 1

  for (i in seq_len(n)) {
    rtf[[i]] <- rtf[[i]][start[i]:end[i]]
    if (i < n) rtf[[i]] <- c(rtf[[i]], r2rtf:::as_rtf_new_page())
  }

  rtf <- do.call(c, rtf)

  write_rtf(rtf, output)
}
