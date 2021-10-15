
<!-- README.md is generated from README.Rmd. Please edit the Rmd file -->

# `eavs`: Election Administration and Voting Survey (2008-2020)

`eavs` is intended to help users download, match, and correct the
biennial [Election Administration and Voting
Survey](https://www.eac.gov/research-and-data/studies-and-reports)
(EAVS), published by the U.S. Election Assistance Commission. This
unique survey measures a variety of administrative practices that affect
US elections. Currently working on mirroring the project elsewhere.

This package consolidates common questions across the different
structures of the eavs. It provides functions that can help coalesce the
data types and codes.

## Installing and Loading the Package

You can install the development version from the [GitHub
repository](https://github.com/jw2249a/eavs).

``` r
remotes::install_github("jw2249a/eavs")
library(eavs)
```

OR clone the repository and install the script locally

``` r
library(devtools)
devtools::install()
library(eavs)
```

## Introduction

To run the multiple functions that will clean and standardize the EAVS
(without added corrections), run the `create_eavs_years()` command:

``` r
# Load All Years
# Takes a good deal of time
eavs <- create_eavs_years()  
dim(eavs)
```

``` r
# Just load 2008 and 2012
eavs <- create_eavs_years(years = c(2008, 2012))
dim(eavs)
```

The `create_eavs_years()` function is a relatively simple function that
highlights the process to create the aggregated eavs. dplyr’s
`bind_rows()` function is used here for ease of displaying the function.

``` r
create_eavs_years <- function(years = seq(2008, 2020, 2), eavs.dir="data") {
  
  dplyr::bind_rows(lapply(years, function(year) {
    eavs <- fix_eavs_vars(read_eavs(year, data.dir=eavs.dir)
    eavs <- rename_eavs(eavs)
    eavs <- fix_types(eavs)
  }))
}

create_eavs_years()
```

This command assumes you have downloaded the EAVS already. IF you have
not, then run the `download_eavs()` command. This will create a folder
locally with the copies of the EAVS data (note: we are working on
mirroring the repository for stability). You can also specify a
directory using the extract.dir argument \*note specifying a specific
directory will require you to specify the `eavs.dir` argument in the
`create_eavs_years()` function.

``` r
# Just load 2008 and 2012
download_eavs(years = c(2008, 2012), extract.dir = "unique.dir")
eavs2008_12 <- create_eavs_years(years = c(2008, 2012), eavs.dir = "unique.dir")
```
