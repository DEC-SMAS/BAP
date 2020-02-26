
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BAP

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of BAP is to calculate New York State Department of
Environmental Conservation’s Biological Assessment Profiles (BAPs). For
more information please refer to the following fact sheet:
<https://www.dec.ny.gov/docs/water_pdf/bapnarrative18.pdf>

## Installation

You can install the BAP package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DEC-SMAS/BAP")
```

## Example

This is a basic example to show you how to quickly calculate BAP scores.

``` r
library(BAP)
#> Loading required package: reshape2
#> Loading required package: vegan
#> Loading required package: permute
#> Loading required package: lattice
#> This is vegan 2.5-6
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric
## basic example code
```

Import your dataset.

``` r
taxa.df <- read.csv(file.path(here::here(),
                              "data",
                              "Mosher_Request_1_6_17.csv"),
                    stringsAsFactors = FALSE)
```

Prepare your dataset for processing.

``` r
long.df <- data_prep(taxa.df)
```

Calculate the BAP of interest.

``` r
bap_multi.df <- bap_mp_nav_waters(Long = long.df)
```

``` r
knitr::kable(bap_multi.df)
```

| EVENT\_ID                   | LOCATION | RIVMILE | BASIN | DATE     | RICHNESS | RICH\_SCORE | EPT\_RICH | EPT\_SCORE |  HBI | HBI\_SCORE |  SHANNON | SHANNON\_SCORE | FINAL\_SCORE |
| :-------------------------- | :------- | :------ | ----: | :------- | -------: | ----------: | --------: | ---------: | ---: | ---------: | -------: | -------------: | -----------: |
| 82.9\_SCHO\_12\_9/9/2015\_1 | SCHO     | 82.9    |    12 | 9/9/2015 |       37 |          10 |        16 |         10 | 3.15 |         10 | 4.737373 |             10 |           10 |
| 86.6\_SCHO\_12\_9/9/2015\_1 | SCHO     | 86.6    |    12 | 9/9/2015 |       36 |          10 |        16 |         10 | 3.88 |         10 | 4.692271 |             10 |           10 |
| 87\_SCHO\_12\_9/9/2015\_1   | SCHO     | 87      |    12 | 9/9/2015 |       36 |          10 |        13 |         10 | 4.33 |         10 | 4.907209 |             10 |           10 |

Export the results as a CSV.

``` r
write.csv(bap_multi.df, 
          file.path(paste0(Sys.Date(), "_bap_multiplate", ".csv")))
```
