# STUDY01 pipeline artefacts: SAP -> TOC -> shells -> ARS -> ARD -> tables

This folder ships the **output of every stage** of the ydisctools ARS
toolchain for the bundled synthetic study STUDY01 (the ten-display sample
SAP in `../ars-doc-samples`).  The *From the SAP to the tables* article
re-runs the same chain step by step; here you can inspect each artefact
without executing anything.

| Stage | Artefact | Produced by |
|-------|----------|-------------|
| input | `adam/ADSL.csv`, `adam/ADAE.csv`, `adam/ADEFF.csv` | deterministic dummy ADaM (60 subjects, 3 arms) -- `make_sap_pipeline.R` |
| 1 TOC | `01_toc/TOC_STUDY01.xlsx` | `read_csr_map()` + `read_sap_toc()` on the Word samples, then the review edits the article walks through |
| 2 shells | `02_shells/shells_STUDY01.xlsx` / `.rtf` | `ars_mock()` (same TOC, same recipes as the ARS) |
| 3 ARS | `03_ars/ARS_STUDY01.xlsx` | `ars_from_toc()` + `build_ars()` + `write_ars_xlsx()` |
| 4 programmes | `04_programmes/ARD_Out_*.R` (10) | `siera::readARS()` via `ars_generate_ard()` |
| 5 ARD | `05_ard/ARD_STUDY01.csv` | the ten programmes run and combined (`ars_generate_ard()`), flattened to csv |
| 6 tables | `06_tfl/make_tfl.R` + `06_tfl/TFL_STUDY01.rtf` | the study **table programme**: ARD -> `format_stats()` / `pivot_stats_wider()` -> rtfreporter, one display per landscape page |

The ten displays: demographics, disposition, exposure (15.1.x); overall /
by-SOC / by-PT / by-severity / serious-by-SOC TEAE summaries (15.3.1.x, the
serious view narrows the `ae_soc` recipe with the TOC `where` column); and
two custom efficacy displays on ADEFF (15.2.x).  Numbering follows the
*company* CSR template's section 15 via `read_csr_map()`.

## Regenerate

From this folder (needs {officer}, {siera}, {cards}, {openxlsx} and
[rtfreporter](https://github.com/ichirio/rtfreporter)):

```r
# Rscript make_sap_pipeline.R
```

Everything is seeded, so a regeneration only changes files when the code
or the source documents change.

## Run pieces yourself

```r
setwd(system.file("sap-pipeline", package = "ydisctools"))
source("04_programmes/ARD_Out_dm.R")   # -> ARD tibble for one display
# Rscript 06_tfl/make_tfl.R            # -> re-render 06_tfl/TFL_STUDY01.rtf
```

(The generated programmes read `adam/*.csv` relative to this folder; run
them from here.  In an installed library the folder is read-only -- copy it
somewhere writable first if you want to re-render outputs.)

## Provenance

The dummy ADaM data is generated, deterministic and fictional; the source
SAP / CSR template are original synthetic documents (see
`../ars-doc-samples/README.md` for the provenance and licence note).
