Prepare Data
================

# Scripts

  - [Required Packages &
    Reproducibility](#required-packages-&-reproducibility)
  - [Tidy Data](#tidy-data)
  - [Save Data for Analysis](#save-data-for-analysis)
  - [Visualization of Data](#visualization-of-data)
      - [Dependent Variable](#dependent-variable)
      - [Independent Variable](#independent-variable)
      - [Control Variables](#control-variables)
      - [Correlations Matrix](#correlations-matrix)

## Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
renv::snapshot()
```

    ## * Lockfile written to '~/Dropbox/Papers/compromise-punish/src/data-processing/renv.lock'.

## Tidy Data

``` r
source(here("src/data-processing/tidy_data.R"))
```

## Save Data for Analysis

``` r
save(d, file = here("data/intermediate/cleaned_data.RData"))
```

## Visualization of Data

### Dependent Variable

### Independent Variable

### Correlations Matrix
