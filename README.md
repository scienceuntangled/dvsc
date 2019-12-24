
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dvsc

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/scienceuntangled/dvsc.svg?branch=master)](https://travis-ci.org/scienceuntangled/dvsc)
<!-- badges: end -->

## Installation

You can install from [GitHub](https://github.com/scienceuntangled/dvsc)
with:

``` r
## install.packages("remotes") ## if needed
remotes::install_github("scienceuntangled/dvsc")
```

## Example usage

``` r
library(dvsc)

## read your dvw file (use an example file here)
x <- read_dv(dv_example_file(2))
## unfortunately this example file is missing its video_time entries so we'll add some placeholder values
x$plays$video_time <- seq_len(nrow(x$plays))

## create the sportscode file
myxml <- dv2sc(x, destfile = "~/example.xml", home_team_short = "ACH", visiting_team_short = "MAR")
```
