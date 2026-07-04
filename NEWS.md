# ydisctools (development version)

`ydisctools` is a Proof-of-Concept collection of small CDISC tools and is
not versioned for release; this changelog tracks notable changes only.

## Documentation

* New article [The ARS chain](https://ichirio.github.io/ydisctools/articles/ars-chain.html):
  a runnable walk-through of the whole toolchain -- recovering compact
  parameters from an existing {cards} programme, expanding them to a
  siera-ready ARS workbook, and running the generated ARD programmes into
  one study-level ARD keyed by `OutputId` (#14).

## New features

* TOC hub (part 1 of the SAP -> TOC -> mock -> ARS chain, #18): a bundled
  display-recipe catalog (`inst/ars-display-recipes/`, transcribed from the
  FDA Standard Safety Tables & Figures Integrated Guide and PHUSE white
  papers), `ars_toc_template()` (TOC workbook pre-filled with the standard
  safety set), and `ars_from_toc()` (recipe expansion + custom passthrough +
  rule-based `toc_no` auto-numbering with an ICH E3-style default section
  map, overridable per sponsor) feeding straight into `build_ars()` /
  `ars_generate_ard()` (#19).
* Readable categorical ARDs: `build_ars()` now uses an improved
  `categorical_summary` method template (a ydisctools overlay over the
  vendored siera catalog) built on the modern cards
  `by = ` / `variables = ` denominator pattern, so generated ARDs carry the
  real category variable name and levels (`variable = "AGEGR1"`,
  `variable_level = "<65"`) instead of the legacy `dummy` placeholder -
  statistics unchanged. The parameter template and the shipped example set
  now mirror a realistic DM display (big N, Age, two age groupings, Sex,
  Race, Ethnicity) (#16).
* Added `ars_generate_ard()`: generate the per-output ARD programmes from an
  ARS file (or a `build_ars()` result) via `siera::readARS()`, run them in
  isolated environments, and return one combined ARD data frame keyed by
  `OutputId` (or a per-output list) (#12).
* Added `ars_params_from_code()` / `write_ars_params()`: statically analyse
  existing {cards}/{cardx} table programmes (or siera-generated ARD
  programmes) and draft the compact ARS parameter set from them, including
  populations/subsets/group levels recovered from the `dplyr::filter()`
  pipeline and display furniture (titles, footnotes, page headers/footers,
  row-label column header) from rtfreporter idioms; everything undecidable
  is surfaced as review notes. `build_ars()` now always emits the ARS
  `Outputs` sheet and, when display metadata is present, the `Displays` /
  `GlobalDisplaySections` sheets (#10).
* Shipped a runnable end-to-end ARS example set under
  `inst/ars-examples/`: compact parameter workbook, the ARS workbook built
  from it, the `siera::readARS()`-generated ARD programmes, and deterministic
  dummy ADaM data (Demographics + Adverse Events displays mirroring the
  rtfreporter showcase, CDISC-pilot arms) (#8).
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
