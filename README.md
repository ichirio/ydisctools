
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ydisctools

<!-- badges: start -->
<!-- badges: end -->

The goal of ydisctools is to …

## Installation

You can install the development version of ydisctools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ichirio/ydisctools")
```

## Example

This is a basic example which shows you how to solve a common problem:

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
