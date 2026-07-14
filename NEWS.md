# ydisctools (development version)

## Bug fixes

* `plot_sankey_subgroups_batch()` now has a runnable example on its reference page, rendering the same two subgroups (Overall N=200 vs Biomarker+ N=20) under `scale_strategy = "shared_max"` (same scale everywhere; node heights directly comparable) and `"first_stage_normalized"` (each subgroup magnified so the first-line column spans the same height; shapes comparable). The `first_stage_max_multiplier` default was raised from 3 to 100, so first-stage normalization now equalises the first line in practice unless a subgroup is extremely small (#65).

* `plot_sankey()` link ribbons now respect `baseline`: with `baseline = "top"` they stack from the node top downward, ordered by the counterpart node's position (top-most first); with `"bottom"` from the node bottom upward. Previously the offsets always accumulated from the node bottom (the bottom-aligned logic) and the within-node order was alphabetical on the node id. The example flows were also corrected: a new line implies a change of therapy, so same-treatment links (e.g. "L1: Chemo" -> "L2: Chemo") are gone, and every treated node now also feeds the next line's "No Treatment" node — only "L1: No Treatment" remains the link-less isolated-node showcase (#63).

* `plot_sankey()` now draws links in a single light grey (`link_fill = "#CCCCCC"`) by default instead of the grey-blue `"#8AA1B1"`; colouring ribbons by their end nodes stays available via `use_link_color_by_source` / `use_link_color_by_target` (the pkgdown example previously forced `use_link_color_by_source = TRUE` and now shows the default). The example data were also made cohort-consistent — every node carries an explicit `node_n`, about 30% of each line is "No Treatment", and a line's total N never exceeds the treated N of the previous line — and the example labels no longer touch the ribbons (`label_size = 2.5` plus wider pkgdown figures via `figures: fig.width: 9`) (#61).

* `plot_sankey()` no longer clips the node labels of the last stage at the device edge. Labels are drawn `label_nudge` off the node along the stage axis, but `geom_text()` does not contribute to the panel range, so the terminal-stage labels ("Line5: ..." in the pkgdown example) fell outside the device. The panel is now extended on the label side of the terminal stage by the span a link occupies between two adjacent stages (`1 - node_width`) plus `label_nudge`, so terminal labels get the same room as the between-stage ones. A new `label_position = c("right", "left")` argument (default `"right"`, the previous behaviour) picks the side; the reserved space follows it. The example labels were also shortened to the `"L1: Chemo"` style so they no longer collide with the link ribbons (#59).

* The programmes `ars_generate_ard()` writes now carry the column reordering themselves: a trailing `ARD <- cards::tidy_ard_column_order(ARD)` is appended to each generated `ARD_<OutputId>.R`. Previously the group1-first order was only restored in ydisctools' runtime (#55/#56), so a standalone run of the saved programme -- or the code returned by `run = FALSE` -- still produced group1 at the back. Now the saved code and the in-memory result agree (#57).

* `ars_generate_ard()` now returns the combined ARD in the canonical column order (`group1`, `group1_level`, ... first). siera's generated programmes combine the per-analysis ARDs with `dplyr::bind_rows()`, which takes the column order from the first analysis -- usually `total_n`, which has no grouping columns -- so `group1*` were appended at the back. The runner now applies `cards::tidy_ard_column_order()` after combining (not `bind_ard()`, which de-duplicates on group/variable/stat ignoring `OutputId` and would drop identical stats shared by two displays) (#55).

* `build_ars()` no longer makes a lowercase `dataset` (e.g. `adsl`) produce two ADaM reads in the generated programme. siera's `.generate_adam_loading_code()` collects the datasets to read case-sensitively across `Analyses$dataset`, `AnalysisSets$condition_dataset` and `DataSubsets$condition_dataset`; a bare-flag population baked in the fixed spelling `"ADSL"` while the user's `dataset` stayed `adsl`, so the same file was read twice. `build_ars()` now folds every dataset reference to one spelling per case-insensitive key (the user-authored `Analyses$dataset` spelling wins); a genuinely different dataset (an AE output's `ADAE` events + `ADSL` population) folds to different keys and correctly stays two reads (#53).


## New features

* **`ars_generate_ard()` reads the real ADaM files, in any common format,
  and can generate code without data** (#50). siera 0.5.6 hardcodes
  `readr::read_csv('<adam_path>/<DATASET>.csv')` in every programme, so a
  study whose ADaM are `.xpt` (or `.sas7bdat` / `.rds` / `.rda`) could not
  be read. The wrapper now rewrites that reader to the new exported helper
  **`read_adam()`**, which resolves the actual file present by format
  priority **`.xpt` > `.sas7bdat` > `.rds` > `.rda` > `.csv`** (haven for
  the SAS/transport formats, with value labels zapped so downstream logic
  matches the csv path; case-insensitive file matching). A new
  **`run = FALSE`** argument generates and rewrites the programmes without
  executing them, so the R code can be produced with no data present
  (`adam_path` then only sets the path baked into the code). Fixes the
  roxygen that wrongly claimed `adam_path` already read `.csv` / `.xpt`.

`ydisctools` is a Proof-of-Concept collection of small CDISC tools and is
not versioned for release; this changelog tracks notable changes only.

## New features

* **Response-rate confidence intervals** (#40): new catalog method
  `proportion_ci` -- per-group proportion of each level of the analysis
  variable with its CI, generated with `cardx::ard_categorical_ci()`. The
  CI method, confidence level and an optional single response level ride a
  new optional `Analyses` column **`options`**
  (`"method=clopper-pearson; conf.level=0.9; value=Response"`); because
  siera resolves template parameters only from its own computed value
  sources, `build_ars()` bakes each distinct option set into its own
  generated method entry (readable ids like
  `Mth_proportion_ci_clopper_pearson_90_Response`), so several CI flavours
  coexist in one study. `ars_params_from_code()` recovers
  `ard_categorical_ci()` calls -- including from the generated code, so the
  #39 loop guarantee holds for CIs too -- and `ars_params_from_ard()`
  classifies `conf.low`/`conf.high` stat sets, recovering the confidence
  level and CI method from the ARD's metadata rows.

* Population/`where` bare-flag sugar can now be dataset-qualified:
  `"ADRS.ITTFL"` keeps its dataset instead of defaulting to ADSL (#40).

* `ars_params_from_code()` handles two more real-world shapes (#40):
  a multi-variable `by = c(...)` tabulation keeps its faithful group list
  with an explicit LIMITATION note (3+ simultaneous groupings, #6), and an
  external `denominator = <dataset>` gets a REVIEW note (the compact format
  wires percentages to a `total_n` analysis instead). purrr
  `imap()`-wrapped analyses are scanned through; dynamic
  `filter(!!parse_expr(cond))` subgroup conditions stay REVIEW notes.

* **Human-maintainable generated code + round-trip loop guarantee** (#39).
  Generated programmes are ultimately finished and maintained by people, so:
  - the overlay `categorical_summary` method was **split**: flat
    subject-count analyses (no category variable) now come from a dedicated
    `categorical_summary_flat` template -- a single clean cards call with an
    explanatory comment -- and grouped analyses lost the dead flat branch.
    Every generated analysis now contains only its own live code.
    `build_ars()` picks the flat template automatically; the compact
    parameter format is unchanged.
  - `ars_params_from_code()` recovers the **pre-defined group levels** from
    the generated groupId-stamping `case_when()` blocks (the only place
    generated code carries them), so `groups` survives the loop.
  - new `test-ars-roundtrip.R` locks the loop in: *human code -> params ->
    ARS -> generated code -> params -> ...* converges in one step, is
    idempotent afterwards (params and rebuilt ARS identical sheet-by-sheet),
    and both generated ARDs match the ARD produced by executing the
    original human-style programme directly.

* `build_ars()` accepts **`population = "ALL"`** -- no analysis-set filter,
  for data that already *are* the intended analysis set (e.g. pre-filtered
  upstream, the usual shape of a recovered single-programme draft). No
  `AnalysisSets` row is emitted and `analysisSetId` stays blank;
  `siera::readARS()` then uses the analysis dataset unfiltered (its
  documented fallback, with a generation-time warning). An `"ALL"` output
  must keep all its analyses on one dataset (siera's no-filter fallback
  cannot do the cross-dataset merge). Mock shells label it "All Subjects"
  (#37).

## New features

* **Verification matrix over the official cards / cardx examples** (#48,
  after the #37/#44/#46 sequence): `tests/testthat/test-ars-function-matrix.R`
  now pushes the reference-documentation examples (`ard_summary`,
  `ard_tabulate`, `ard_stack`, `ard_stack_hierarchical`,
  `ard_categorical_ci`, `ard_stats_chisq_test`, `ard_stats_aov` /
  `ard_stats_oneway_test`, `ard_stats_fisher_test`, `ard_stats_prop_test`,
  on `cards::ADSL` / `cards::ADAE`) through the whole
  code -> params -> ARS -> generated-programme -> executed-ARD chain and
  compares every variable's every statistic against a direct run of the
  same code -- including a mixed continuous + categorical DM display
  checked per variable, both recovery routes. Fixes that fell out:

  - `ars_params_from_code()`: `ard_stack_hierarchical(over_variables =
    TRUE)` no longer swallows the per-level analyses and the
    `denominator=` total_n (an early return emitted ONLY the any-event
    row); the cardx `ard_stats_*` handlers now also read the FORMULA call
    form the official examples use (`ard_stats_aov(AGE ~ ARM, data =
    ADSL)` used to recover nothing, silently); `chisq` is emitted in the
    category-as-second-grouping shape its catalog template actually tests;
    a `variables = <column>` fisher / prop_test call gets a REVIEW note
    spelling out the subject-flag completion (`variable = USUBJID`,
    `where = <condition>`) instead of silently generating a test of the
    wrong thing.
  - `ars_params_from_ard()`: real hierarchical ARDs carry their
    denominator only as per-row `N` stats -- when percentage analyses
    need a denominator and no subject-count unit exists, the missing
    `total_n` analysis is added (ASSUMED note; review its `dataset`).
    Such synthesized analyses are marked internally and do NOT count as
    evidence in `ars_params_recover()`'s mismatched-pairing check.

## Bug fixes

* Hierarchical ARDs (`cards::ard_stack_hierarchical()`) no longer leak the
  cards sentinel `..ard_hierarchical_overall..` into `group_by`, which made
  `build_ars()` fail with "Cannot parse group_by entry" (#46).
  `ars_params_from_ard()` now maps the `over_variables` rows to *Subjects
  with at least one event, n (%)* (the same shape `ars_params_from_code()`
  emits), drops hierarchy parents from deeper levels' groupings (flat
  per-level analyses + LIMITATION note), skips unrecognised `..x..`
  sentinels with a REVIEW note, and notes any faithful 3+-grouping analyses
  that `build_ars()` will reject (the 2-grouping cap is ydisctools #6). A
  standard one-`by` hierarchical AE ARD now recovers and builds with no
  manual edit.

* `ars_params_recover()` now stops with a diagnosis when the programme and
  the ARD describe fully disjoint analyses (not one (method, variable,
  groupings) signature in common) instead of silently drafting a
  meaningless merge -- the #46 report was a demographics programme paired
  with an adverse-events ARD (a stale `ard` object). The error says so and
  points at single-source recovery for intentional pairings.

* `write_ars_params()`'s write-time guard now also flags `group_by` values
  `build_ars()` cannot use (unparseable entries, 3+ simultaneous
  groupings), not just blank `population` / `group_by` (#46).

* The parameter-recovery functions no longer draft workbooks their own
  `build_ars()` deterministically rejects (#44, the root cause behind the
  #37 report resurfacing). #37 taught `build_ars()` to *accept*
  `population = "ALL"` but the recovery routes still emitted a blank, so a
  recovered draft could not build whenever the analysis data were filtered
  upstream (no `*FL` filter in the code -- the most common real-world
  shape). Now the two knowledge states are encoded differently instead of
  both becoming a blank: `ars_params_from_code()` emits `"ALL"` (ASSUMED
  note) when the data pipeline was fully resolved and carries no
  analysis-set filter, and keeps blank + REVIEW only when the population is
  genuinely unknowable (unresolved data context / unconvertible condition);
  `ars_params_from_ard()` sets `"ALL"` on the output row (an ARD is
  computed from the analysed data as-is, so that is the faithful recovery);
  in `ars_params_recover()` a unanimous code-side flag (e.g. `SAFFL`) still
  beats the assumed `"ALL"`, and the ARD side's `dataset UNKNOWN` alarm is
  dropped when the code side resolved every dataset. As a safety net for
  the remaining unknowable cases, `write_ars_params()` now warns at *write*
  time when a draft is missing a `population` / `group_by` that
  `build_ars()` will require, instead of deferring the failure to the build
  step.

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
