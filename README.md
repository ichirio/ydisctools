
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ydisctools

<!-- badges: start -->
<!-- badges: end -->

`ydisctools` is a utility package for CDISC-related programming workflows
(SDTM/ADaM helpers, metadata utilities, text/RTF helpers, and data QC helpers).

This repository also includes a Sankey plotting utility for treatment transition
visualization:

- `plot_sankey_polygon()` (Bezier links + rectangle nodes)
- isolated node support (nodes with zero incoming/outgoing links)
- top/bottom baseline alignment and flexible scaling
- treatment color modes (`across_lines` / `by_line`)

## Installation

You can install the development version of ydisctools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ichirio/ydisctools")
```

## Documentation

- Function reference: `docs/FUNCTION_REFERENCE.md`
- Sankey usage guide: `docs/SANKEY_GUIDE.md`

Regenerate function reference from `man/*.Rd`:

``` r
Rscript tools/build_function_reference.R
```

## Quick Example

Basic utility example:

``` r
#library(ydisctools)
## basic example code
race1 <- c("ASIAN", "ASIAN", NA, "", NA)
race2 <- c("WHITE", "", "WHITE", NA, NA)
race3 <- c("BLACK", NA, "", "BLACK", "")

catx(":", race1, race2, race3)
#> [1] "ASIAN:WHITE:BLACK" "ASIAN"             "WHITE"            
#> [4] "BLACK"             ""
```

Sankey example script:

``` r
Rscript output/create_line1_line5_sankey_sample.R
```

Generated files:

- `output/sankey_line1_line5_sample_across_lines.png`
- `output/sankey_line1_line5_sample_by_line.png`
