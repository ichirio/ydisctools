# ydisctools (development version)

`ydisctools` is a Proof-of-Concept collection of small CDISC tools and is
not versioned for release; this changelog tracks notable changes only.

## New features

* Added an ARS (CDISC Analysis Results Standard) generator for
  [siera](https://cran.r-project.org/package=siera): `ars_param_template()`
  writes a compact parameter workbook (one row per analysis, with a worked
  demographics + adverse-event example), `read_ars_params()` reads it back,
  `build_ars()` expands it into the full set of ARS metadata sheets
  (de-duplicated analysis sets / groupings / data subsets, automatic
  numerator-denominator wiring, method sheets expanded from siera's bundled
  MIT-licensed method template catalog), and `write_ars_xlsx()` writes a
  workbook that `siera::readARS()` consumes directly (#5).

## Site & tooling

* `R CMD check` is now clean (no WARNINGs/ERROR) so CI is green: documented
  previously-undocumented arguments and the `plot_sankey_subgroups_batch()`
  object, fixed a failing `check_invalid_chars_in_datasets()` example (now also
  tolerates unnamed dataset lists), escaped non-ASCII source to `\u` literals,
  declared `R (>= 4.1.0)` and the `tibble` / `magrittr` test dependencies, and
  tidied internal cross-references.
* Added a [CONTRIBUTING.md](https://github.com/ichirio/ydisctools/blob/main/CONTRIBUTING.md)
  documenting the issue -> branch -> PR -> green-CI workflow, the `exec:*`
  labels, and the hand-maintained-docs rule (no `devtools::document()`).
* Added a [pkgdown](https://pkgdown.r-lib.org/) documentation site published to
  GitHub Pages at <https://ichirio.github.io/ydisctools/>, replacing the earlier
  hand-built HTML pages. The site mirrors the
  [rtfreporter](https://github.com/ichirio/rtfreporter) design language with an
  indigo theme, hex logo, and favicons.

## Tools

* `format_stats()` — collapse a long ("tidy", ARD-like) statistic table into one
  formatted display string per group, driven by a format-spec table with inline
  digit tokens (`{mean:2} ({sd:3})`). Grouping-key columns are arbitrary; bare
  `{stat}` tokens pass pre-formatted character values through verbatim.
* `pivot_stats_wider()` — spread one or more columns into combined wide headers
  (joined with `____` by default), filling cells from a value column.

* `plot_sankey()` — treatment-transition Sankey plotter (Bezier links,
  rectangle nodes, isolated-node support, per-line / across-line colouring),
  plus `plot_sankey_subgroups_batch()` for subgroup batches.
