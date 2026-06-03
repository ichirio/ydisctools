extract_rd_field <- function(lines, field) {
  prefix <- paste0("\\", field, "{")
  hit <- which(startsWith(lines, prefix))
  if (length(hit) == 0) {
    return(NA_character_)
  }
  val <- sub(prefix, "", lines[hit[1]], fixed = TRUE)
  sub("\\}.*$", "", val)
}

man_dir <- "man"
rd_files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)

rows <- lapply(rd_files, function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  nm <- extract_rd_field(x, "name")
  ttl <- extract_rd_field(x, "title")
  if (is.na(nm)) {
    return(NULL)
  }
  data.frame(
    function_name = nm,
    title = ifelse(is.na(ttl), "", ttl),
    rd_file = basename(f),
    stringsAsFactors = FALSE
  )
})

rows <- rows[!vapply(rows, is.null, logical(1))]

if (length(rows) == 0) {
  stop("No Rd entries were found in man/.")
}

ref <- do.call(rbind, rows)
ref <- ref[!is.na(ref$function_name) & nzchar(ref$function_name), , drop = FALSE]
ref <- ref[order(ref$function_name), , drop = FALSE]

header <- c(
  "# Function Reference",
  "",
  "This document is generated from `man/*.Rd`.",
  "",
  "- Regenerate with: `Rscript tools/build_function_reference.R`",
  "- For details in R: `?function_name`",
  "",
  "| Function | Description | Rd |",
  "|---|---|---|"
)

body <- paste0(
  "| `", ref$function_name, "` | ", gsub("\\|", "\\\\|", ref$title), " | `", ref$rd_file, "` |"
)

out <- c(header, body)

if (!dir.exists("docs")) {
  dir.create("docs", recursive = TRUE)
}

writeLines(out, con = "docs/FUNCTION_REFERENCE.md", useBytes = TRUE)
cat(normalizePath("docs/FUNCTION_REFERENCE.md", winslash = "/"), "\n")
