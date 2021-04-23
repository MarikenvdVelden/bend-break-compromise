Analyses
================

# Scripts

  - [Required Packages &
    Reproducibility](#required-packages-&-reproducibility)
  - [Analyses](#analyses)
      - [Balance Checks](#balance-checks)
      - [Gender Hypothesis](#gender-hypothesis)
      - [Migration Hypothesis](#migration-hypothesis)
      - [Intersection Hypothesis](#intersection-hypothesis)
      - [Intersection Hypothesis –
        Pooled](#intersection-hypothesis--pooled)

# Required Packages & Reproducibility

``` r
rm(list=ls())
source(here("src/lib/functions.R"))
```

# Analyses

``` r
load(here("data/intermediate/cleaned_data.RData"))
source(here("src/analysis/data-for-analyses.R"))
```

Next, we automatically extract a `.md` file for the online appendix, as
well as a latex table for the manuscript. We are using jinja2 template
[src/analysis/table\_descriptives.tex.j2](table.tex.j2) which is called
with a json string containing the data. To replicate, make sure
`env/bin/pip install -U j2cli` is installed via your command line.

``` r
source(here("src/analysis/descriptive-information-overview.R"))
table2 <- knitr::kable(descr, digits=2)
fn <- here("report/figures/table_descriptives.md")
cat("# Table: Descriptive Information of Variables under Study \n\n", file=fn)
cat(table2, file=fn, sep="\n", append=T)

methodnames <- setNames(as.list(descr$name), descr$name)
table <- purrr::map(descr, .f= ".") 
#render_j2("src/analysis/table_descriptives.tex.j2", "report/figures/table_descriptives.tex", data=list(data=table, methods=methodnames))
```

## Balance Checks

``` r
source(here("src/analysis/balance-test.R"))
```

## Gender Hypothesis

``` r
source(here("src/analysis/h1.R"))
```

## Migration Hypothesis

``` r
source(here("src/analysis/h2.R"))
```

## Intersection Hypothesis

``` r
source(here("src/analysis/h3.R"))
```

## Intersection Hypothesis – Pooled

``` r
source(here("src/analysis/h3-pooled.R"))
```
