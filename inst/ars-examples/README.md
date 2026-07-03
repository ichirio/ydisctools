# End-to-end ARS example set (DM + AE)

A complete, runnable artefact chain for the ydisctools ARS generator and
[siera](https://cran.r-project.org/package=siera), on the same displays the
[rtfreporter showcase](https://ichirio.github.io/rtfreporter/) articles use:
a Demographics summary and Treatment-Emergent Adverse Event summaries for the
three CDISC-pilot arms (Placebo / Xanomeline Low Dose / Xanomeline High Dose).

| File | Stage | Produced by |
|------|-------|-------------|
| `params_dm_ae.xlsx` | compact parameters (the only hand-authored input) | `make_ars_examples.R` |
| `ARS_dm_ae.xlsx` | full ARS metadata workbook | `ydisctools::build_ars()` + `write_ars_xlsx()` |
| `ARD_Out_dm.R`, `ARD_Out_ae.R` | ARD programmes | `siera::readARS()` |
| `adam/ADSL.csv`, `adam/ADAE.csv` | deterministic dummy ADaM data (30 subjects) | `make_ars_examples.R` |

Analyses: big N, Age (continuous), Age group / Sex / Race n (%) with the big-N
denominator (DM); big N, any-TEAE, TEAE by SOC / by PT / by severity under
`TRTEMFL EQ Y` (AE).  The nested arm x SOC x PT display would need three
simultaneous groupings, which the compact format does not support yet
(ydisctools issue #6), so the AE output ships flat by-SOC / by-PT analyses.

## Run the ARD programmes

From this folder (the emitted `readr::read_csv()` paths are relative):

```r
setwd(system.file("ars-examples", package = "ydisctools"))
source("ARD_Out_dm.R")   # -> ARD tibble in the session
source("ARD_Out_ae.R")
```

Requires the packages the generated code uses (`cards`, `dplyr`, `readr`, ...).

## Regenerate everything

From this folder in the source tree:

```sh
Rscript make_ars_examples.R
```

Requires ydisctools and siera installed. `tests/testthat/test-ars-examples.R`
keeps the shipped `ARS_dm_ae.xlsx` in sync with `params_dm_ae.xlsx`.
