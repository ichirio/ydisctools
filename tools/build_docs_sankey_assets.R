if (!dir.exists("docs/assets")) {
  dir.create("docs/assets", recursive = TRUE)
}

source("output/create_line1_line5_sankey_sample.R", local = new.env(parent = globalenv()))

targets <- c(
  "sankey_line1_line5_sample_full_shared_scale.png",
  "sankey_line1_line5_sample_subgroup_shared_scale.png"
)

for (f in targets) {
  src <- file.path("output", f)
  dst <- file.path("docs", "assets", f)
  if (!file.exists(src)) {
    stop("Missing generated file: ", src)
  }
  ok <- file.copy(src, dst, overwrite = TRUE)
  if (!ok) {
    stop("Failed to copy file: ", src, " -> ", dst)
  }
}

cat(normalizePath(file.path("docs", "assets"), winslash = "/"), "\n")
