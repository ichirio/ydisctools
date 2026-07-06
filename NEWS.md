# ydisctools (development version)

`ydisctools` is a Proof-of-Concept collection of small CDISC tools and is
not versioned for release; this changelog tracks notable changes only.

## New features

* `build_ars()` accepts **`population = "ALL"`** -- no analysis-set filter,
  for data that already *are* the intended analysis set (e.g. pre-filtered
  upstream, the usual shape of a recovered single-programme draft). No
  `AnalysisSets` row is emitted and `analysisSetId` stays blank;
  `siera::readARS()` then uses the analysis dataset unfiltered (its
  documented fallback, with a generation-time warning). An `"ALL"` output
  must keep all its analyses on one dataset (siera's no-filter fallback
  cannot do the cross-dataset merge). Mock shells label it "All Subjects"
  (#37).

## Bug fixes

* `build_ars()` now rejects a percentage analysis whose `total_n`
  denominator is listed **after** it, with a hint to reorder -- siera
  generates programmes in row order, so the late denominator produced
  `object 'df2_An_NN' not found` at run time. `ars_params_from_ard()`
  correspondingly emits `total_n` analyses first (cards ARDs list
  `.total_n` / `.by_stats` counts last) (#37).

* The `build_ars()` required-fields error now names only the column(s)
  actually missing (it always claimed both `population` and `group_by`)
  and points at `population = "ALL"` for pre-filtered data (#37).

## New features

* New `ars_params_recover()`: the recommended front door of the
  parameter-recovery family. With one source it delegates to
  `ars_params_from_code()` / `ars_params_from_ard()`; with **both** it
  merges the drafts for a more accurate result than either alone -- the ARD
  is authoritative for *which* analyses exist, each is enriched with the
  code's provenance (population / `where` / dataset / pre-defined groups /
  display furniture) by signature matching, unmatched analyses inherit the
  output's unanimous provenance with an ASSUMED note, and code-only /
  ARD-only discrepancies become explicit review notes. *The ARS chain*
  article gained a section walking the ARD route and the both-sources
  merge (#35).

* New `ars_params_from_ard()`: draft the compact parameter set from an
  existing **ARD** -- a cards `ard_stack()` result (optionally
  `unlist_ard_columns()`-flattened), a siera-generated combined ARD from
  `ars_generate_ard()`, or a csv/xlsx export of either -- for when the
  producing programme is unavailable or unreadable. Analyses are grouped by
  `AnalysisId` (siera) or grouping/variable combination (cards) and
  classified onto the method catalog (continuous stats ->
  `continuous_summary`, n/p tabulations -> the ARS `categorical_summary`
  idiom, `..total_n..` / `.by_stats` / `.flag_` idioms -> `total_n`).
  Population, `where` and dataset names do not live in an ARD and are
  surfaced as REVIEW notes (#32).

## Bug fixes

* `ars_params_from_code()` now resolves the variable-list indirections real
  programmes use (#32): character vectors assigned earlier in the file and
  referenced via `all_of(vars)` / `any_of(vars)` (or directly), names
  injected with `!!sym(col)` / `syms()` / `.data[[col]]`, and magrittr
  pipes (`df %>% ard_stack(...)`) are collapsed so the piped data feeds the
  dataset / population detection (previously such calls yielded a single
  bogus `all_of(vars_cont)` "variable" and `dataset = UNKNOWN`).
  `ard_stack(.by_stats = TRUE)` is now recognised as a per-group subject
  count (`total_n`). Unresolvable wrappers keep the symbol and emit a
  REVIEW note.

## New features

* New bundled artefact set `inst/sap-pipeline/`: the **output of every
  stage** of the SAP -> TOC -> shells -> ARS -> ARD programmes -> tables
  chain for the ten-display sample study STUDY01, regenerated end-to-end by
  one seeded script (`make_sap_pipeline.R`). Ships deterministic dummy ADaM
  (ADSL / ADAE / ADEFF), the reviewed TOC workbook, xlsx + rtf mock shells,
  the ARS workbook, the ten siera-generated ARD programmes, the combined
  study ARD (csv) and a generic **table programme**
  (`06_tfl/make_tfl.R`, built on `format_stats()` +
  `pivot_stats_wider()` + rtfreporter) with its final RTF output -- so the
  whole chain can be inspected without executing anything (#31).

* The sample SAP (`inst/ars-doc-samples/sample_sap.docx`) grew to a
  decent-scale document: objectives/endpoints, design, analysis-set,
  methods and sample-size sections plus a **ten-display** planned-display
  appendix (was seven) -- adding the by-preferred-term and serious-AE views
  and a second (key secondary) efficacy display (#31).

## Bug fixes

* The overlay `categorical_summary` method template now computes **per-group
  percentage denominators for flat analyses** (no category variable, e.g.
  "subjects with at least one TEAE"): it tabulates a constant flag with the
  group as `by=`, so p = n / that arm's big N instead of n / overall N.
  Flat ARD rows now carry `variable = ".flag_"` and the group in
  `group1_level`, consistent with grouped rows (#31).

* `ars_from_toc()`: a TOC row's extra `where` condition no longer narrows
  the display's `total_n` analysis -- big-N denominators stay at
  analysis-set level (it still narrows every other analysis), which makes
  serious-AE-style displays (`where = "AESER EQ Y"`) work end-to-end (#31).

## New features

* `read_sap_toc()` gains a **prose route** and a `mode` argument
  (`"auto"` / `"table"` / `"prose"`). When a SAP carries no planned-display
  table, it now scans the *Statistical Methods / Analyses* section for display
  declarations -- sentences whose subject names a clinical data domain and
  that carry a display verb (e.g. "Demographic data will be summarized by
  treatment group") -- classifies each via the display-recipe catalog, and
  keeps the verbatim sentence in a new `source` column for review.
  Statistics-detail lines ("95% CIs will be provided") are dropped, and the
  scan is anchored to the methods subtree so out-of-scope prose is excluded.
  This was designed from a cross-analysis of 19 real SAPs, which showed the
  display list almost always lives in prose rather than a table (#29).
  *Follow-up (not in this release):* a PDF route (via `pdftools`) with an
  extraction-yield gate for scanned / font-obfuscated SAPs.

## Documentation

* The SAP article was extended through the final tables and retitled
  [From the SAP to the tables](https://ichirio.github.io/ydisctools/articles/sap-to-ars.html):
  it now walks the ten-display STUDY01 chain end-to-end -- CSR numbering
  map, SAP TOC draft, the materialised review step (including the serious-AE
  `where` narrowing and two custom efficacy displays), mock shells, ARS
  build, running ALL generated ARD programmes against the bundled dummy
  ADaM, and the `format_stats()` / `pivot_stats_wider()` table-programme
  pattern -- with every stage pointing at its installed artefact in
  `inst/sap-pipeline/` (#31; original upstream-half article #25).

* New article [The ARS chain](https://ichirio.github.io/ydisctools/articles/ars-chain.html):
  a runnable walk-through of the whole toolchain -- recovering compact
  parameters from an existing {cards} programme, expanding them to a
  siera-ready ARS workbook, and running the generated ARD programmes into
  one study-level ARD keyed by `OutputId` (#14).

## New features

* TOC hub part 3: `read_csr_map()` derives the toc_no numbering section map
  from a company CSR template's heading structure (literal numbers when
  present, rule-based reconstruction of Word auto-numbering otherwise,
  anchored to the tables section so body sections do not capture the map),
  and `read_sap_toc()` drafts TOC rows from a SAP's planned-display table
  (numbers cleaned, titles keyword-matched onto the recipe catalog,
  everything else surfaced as review notes) (#23).
* TOC hub part 2: `ars_mock()` generates TFL mock shells from the same TOC
  workbook the ARS is built from - one xlsx sheet per display ({openxlsx})
  or one rtf document with a display per page ({rtfreporter}), with title
  block, `(N=XXX)` arm columns and per-method statistic placeholder rows
  (#21).
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
