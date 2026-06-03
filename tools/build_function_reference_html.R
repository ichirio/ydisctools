input_md <- "docs/FUNCTION_REFERENCE.md"
output_html <- "docs/FUNCTION_REFERENCE.html"

lines <- readLines(input_md, warn = FALSE, encoding = "UTF-8")

data_lines <- grep("^\\| `[^`]+` \\|", lines, value = TRUE)

parse_row <- function(x) {
  x <- sub("^\\| ", "", x)
  x <- sub(" \\|$", "", x)
  parts <- strsplit(x, " \\| ", fixed = FALSE)[[1]]
  if (length(parts) < 3) {
    return(NULL)
  }
  fn <- gsub("`", "", parts[1], fixed = TRUE)
  desc <- parts[2]
  rd <- gsub("`", "", parts[3], fixed = TRUE)
  list(fn = fn, desc = desc, rd = rd)
}

rows <- lapply(data_lines, parse_row)
rows <- rows[!vapply(rows, is.null, logical(1))]

make_id <- function(fn) {
  gsub("[^a-z0-9]+", "-", tolower(fn))
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

tr_html <- vapply(rows, function(r) {
  fid <- make_id(r$fn)
  paste0(
    "<tr>",
    "<td><a href=\"#fn-", fid, "\"><code>", escape_html(r$fn), "</code></a></td>",
    "<td>", escape_html(r$desc), "</td>",
    "<td><code>", escape_html(r$rd), "</code></td>",
    "</tr>"
  )
}, character(1))

detail_html <- vapply(rows, function(r) {
  fid <- make_id(r$fn)
  rd_url <- paste0("https://github.com/ichirio/ydisctools/blob/main/man/", r$rd)
  src_url <- paste0("https://github.com/ichirio/ydisctools/search?q=", utils::URLencode(r$fn), "&type=code")
  paste0(
    "<article id=\"fn-", fid, "\" class=\"doc-card\" style=\"margin-bottom:12px;\">",
    "<strong><code>", escape_html(r$fn), "</code></strong>",
    "<div class=\"meta\">", escape_html(r$desc), "</div>",
    "<div style=\"margin-top:8px;font-size:14px;\">",
    "<a href=\"", rd_url, "\">Rd manual</a>",
    " · ",
    "<a href=\"", src_url, "\">Source search</a>",
    " · ",
    "<a href=\"#top\">Back to top</a>",
    "</div>",
    "</article>"
  )
}, character(1))

html <- c(
  "<!doctype html>",
  "<html lang=\"en\">",
  "<head>",
  "  <meta charset=\"utf-8\">",
  "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  "  <title>Function Reference - ydisctools</title>",
  "  <link rel=\"stylesheet\" href=\"assets/site.css\">",
  "</head>",
  "<body id=\"top\">",
  "  <nav class=\"topbar\">",
  "    <div class=\"topbar-inner\">",
  "      <div class=\"brand\">ydisctools</div>",
  "      <div class=\"nav-links\">",
  "        <a href=\"index.html\">Home</a>",
  "        <a class=\"active\" href=\"FUNCTION_REFERENCE.html\">Reference</a>",
  "        <a href=\"SANKEY_GUIDE.html\">Sankey Guide</a>",
  "        <a href=\"PAGES_SETUP.html\">Pages Setup</a>",
  "        <a href=\"https://github.com/ichirio/ydisctools\">GitHub</a>",
  "      </div>",
  "    </div>",
  "  </nav>",
  "",
  "  <header class=\"hero\">",
  "    <div class=\"hero-box\">",
  "      <h1>Function Reference</h1>",
  "      <p>Auto-generated from <code>man/*.Rd</code>.</p>",
  "      <div class=\"chips\">",
  "        <span class=\"chip\">Regenerate: Rscript tools/build_function_reference.R</span>",
  "        <span class=\"chip\">Build HTML: Rscript tools/build_function_reference_html.R</span>",
  "      </div>",
  "    </div>",
  "  </header>",
  "",
  "  <main class=\"content\">",
  "    <section class=\"panel\">",
  "      <p class=\"meta\">For in-R docs: <code>?function_name</code></p>",
  "      <p class=\"meta\">Click a function name to jump to its detail card below.</p>",
  "      <table>",
  "        <thead><tr><th>Function</th><th>Description</th><th>Rd</th></tr></thead>",
  "        <tbody>",
  tr_html,
  "        </tbody>",
  "      </table>",
  "    </section>",
  "",
  "    <section class=\"panel\">",
  "      <h2>Function Details</h2>",
  detail_html,
  "    </section>",
  "  </main>",
  "",
  "  <div class=\"footer\">ydisctools documentation site · hosted on GitHub Pages</div>",
  "</body>",
  "</html>"
)

writeLines(html, output_html, useBytes = TRUE)
cat(normalizePath(output_html, winslash = "/"), "\n")
