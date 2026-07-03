# Shared helper for the ARS tests: run a siera-generated ARD programme and
# return its ARD object.
#
# The generated scripts open with library() calls for siera's whole runtime
# stack (broom, cardx, ...), but the generated statements are all
# namespace-qualified, so the library() lines are commented out before
# sourcing: under R CMD check --as-cran (_R_CHECK_SUGGESTS_ONLY_) attaching a
# package outside this package's dependency set would fail the test run.
run_ard_script <- function(path) {
  code <- gsub("^\\s*library\\(", "# library(", readLines(path))
  tf <- tempfile(fileext = ".R")
  writeLines(code, tf)
  on.exit(unlink(tf), add = TRUE)
  env <- new.env(parent = globalenv())
  suppressMessages(suppressWarnings(source(tf, local = env)))
  get("ARD", envir = env)
}
