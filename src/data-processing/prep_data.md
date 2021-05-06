Prepare Data
================

# Scripts

  - [Required Packages &
    Reproducibility](#required-packages-&-reproducibility)
  - [Tidy Data](#tidy-data)
  - [Save Data for Analysis](#save-data-for-analysis)
  - [Check Missing Values](#check-missing-values)
  - [Visualization of Data](#visualization-of-data)
      - [Dependent Variable](#dependent-variable)
      - [Treatment Conditions](#treatment-conditions)
      - [Correlations Matrix](#correlations-matrix)

## Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
renv::snapshot()
```

    ## * The lockfile is already up to date.

## Tidy Data

This code chuck downloads the data from Qualtrics using the API and
cleans the raw data.

``` r
source(here("src/data-processing/tidy_data.R"))
source(here("src/data-processing/Urbaness.R"))
```

## Save Data for Analysis

``` r
save(d, file = here("data/intermediate/cleaned_data.RData"))
```

## Visualization of Data

### Dependent Variable

<img src="../../report/figures/Dependent Variable-1.png" style="display: block; margin: auto;" />

### Treatment Conditions

<img src="../../report/figures/Independent Variables-1.png" style="display: block; margin: auto;" />

### Correlations Matrix

<img src="../../report/figures/Correlations Matrix-1.png" style="display: block; margin: auto;" />
