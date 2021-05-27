Analyses
================

# Scripts

  - [Required Packages &
    Reproducibility](#required-packages-&-reproducibility)
  - [Analyses](#analyses)
      - [Descriptive Results](#descriptive-results)
      - [Balance Checks](#balance-checks)
      - [Gender Hypothesis](#gender-hypothesis)
      - [Migration Hypothesis](#migration-hypothesis)
      - [Intersection Hypothesis](#intersection-hypothesis)
      - [Intersection Hypothesis –
        Pooled](#intersection-hypothesis--pooled)

# Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
```

# Analyses

``` r
load(here("data/intermediate/cleaned_data.RData"))
source(here("src/analysis/scales.R"))
kable(scales, digits = 2) %>%
  kable_styling(full_width = F, position = "center")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

Variable

</th>

<th style="text-align:right;">

Cronbach’s Alpha

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

DV: Trait Evaluation (Corona)

</td>

<td style="text-align:right;">

0.82

</td>

</tr>

<tr>

<td style="text-align:left;">

DV: Trait Evaluation (Defense)

</td>

<td style="text-align:right;">

0.82

</td>

</tr>

<tr>

<td style="text-align:left;">

DV: Trait Evaluation (Education)

</td>

<td style="text-align:right;">

0.84

</td>

</tr>

<tr>

<td style="text-align:left;">

DV: Trait Evaluation (Immigration)

</td>

<td style="text-align:right;">

0.83

</td>

</tr>

<tr>

<td style="text-align:left;">

Attitudes: Women in Politics

</td>

<td style="text-align:right;">

0.86

</td>

</tr>

<tr>

<td style="text-align:left;">

Political Efficacy

</td>

<td style="text-align:right;">

0.23

</td>

</tr>

</tbody>

</table>

``` r
source(here("src/analysis/data-for-analyses.R"))
```

## Check Missing Values

I employ the following criteria:

  - If 10% or less of the values on the dimension are missing, then we
    re-code the missing values to the overall mean.
  - If 11% or more of the values on the dimension are missing, then we
    re-code the missing values to a constant (for instance 0) and
    include a dummy variable indicating whether the response on the
    covariate was missing or not.

<!-- end list -->

``` r
descr <- d %>%
  summarise(`DV: Trait Evaluation` = round(sum(is.na(PT1))/n(),4),
            `DV: Favorability` = round(sum(is.na(PT2))/n(),4),
            `DV: Representation` = round(sum(is.na(PT3))/n(),4),
            `DV: Career Prospects` = round(sum(is.na(PT4))/n(),4),
            `Immigration (%)` = round(sum(is.na(PreT1))/n(),4),
            `Position: Immigration` = round(sum(is.na(PreT2))/n(),4),
            `Position: Defense` = round(sum(is.na(PreT3))/n(),4),
            `Position: Education` = round(sum(is.na(PreT4))/n(),4),
            `Attitude: Women in Politics` = round(sum(is.na(PreT5))/n(),4),
            `Ideology` = round(sum(is.na(PreT6))/n(),4),
            `Political Efficacy (1)` = round(sum(is.na(PreT7_1))/n(),4),
            `Political Efficacy (2)` = round(sum(is.na(PreT7_2))/n(),4),
            `Political Efficacy (3)` = round(sum(is.na(PreT7_3))/n(),4),
            `Political Efficacy (4)` = round(sum(is.na(PreT7_4))/n(),4),
            `Political Efficacy (5)` = round(sum(is.na(PreT7_5))/n(),4),
            `Political Efficacy (6)` = round(sum(is.na(PreT7_6))/n(),4),
            `Political Efficacy (7)` = round(sum(is.na(PreT7_7))/n(),4),
            `Position: COVID-19` = round(sum(is.na(PreT8))/n(),4),
            `Gender` = round(sum(is.na(F1))/n(),4),
            `Age` = round(sum(is.na(F2))/n(),4),
            `Region` = round(sum(is.na(F3))/n(),4),
            `Urbaness` = round(sum(is.na(F4))/n(),4),
            `Migration Background` = round(sum(is.na(F5))/n(),4),
            `Party Preference` = round(sum(is.na(F6))/n(),4),
            `Employment` = round(sum(is.na(F7))/n(),4),
            `Income` = round(sum(is.na(F8))/n(),4),
            `Education` = round(sum(is.na(F9))/n(),4)) %>%
  pivot_longer(cols = `DV: Trait Evaluation`:`Education`,
              names_to = "Variables", values_to = "% Missing Values") %>%
  filter(`% Missing Values` > 0) 

kable(descr) %>%
  kable_styling(full_width = F, position = "center")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

Variables

</th>

<th style="text-align:right;">

% Missing Values

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Urbaness

</td>

<td style="text-align:right;">

0.0951

</td>

</tr>

<tr>

<td style="text-align:left;">

Migration Background

</td>

<td style="text-align:right;">

0.0020

</td>

</tr>

<tr>

<td style="text-align:left;">

Income

</td>

<td style="text-align:right;">

0.0885

</td>

</tr>

<tr>

<td style="text-align:left;">

Education

</td>

<td style="text-align:right;">

0.0033

</td>

</tr>

</tbody>

</table>

I recode the missing values of the variables `Urbaness`, `Migration
Background`, `Income`, and `Education` to the mean value of the
respective variables.

``` r
d <- d %>%
  mutate(F4 = replace_na(F4, round(mean(F4, na.rm = T),0)),
         F5 = replace_na(F5, round(mean(F5, na.rm = T),0)),
         F8 = replace_na(F8,round(mean(F8, na.rm = T),0)),
         F9 = replace_na(F9,round(mean(F9, na.rm = T),0)))
rm(descr, scales)
```

Next, we automatically extract a `.md` file for the online appendix, as
well as a latex table for the manuscript. We are using jinja2 template
[src/analysis/table\_descriptives.tex.j2](table.tex.j2) which is called
with a json string containing the data. To replicate, make sure
`env/bin/pip install -U j2cli` is installed via your command line.

``` r
source(here("src/analysis/descriptive-information-overview.R"))
table2 <- knitr::kable(descr, digits=2)
fn <- here("report/figures/table_descriptives.tex")
cat("# Table: Descriptive Information of Variables under Study \n\n", file=fn)
cat(table2, file=fn, sep="\n", append=T)

methodnames <- setNames(as.list(descr$name), descr$name)
table <- purrr::map(descr, .f= ".") 
#render_j2("src/analysis/table_descriptives.tex.j2", "report/figures/table_descriptives.tex", data=list(data=table, methods=methodnames)
rm(descr, methodnames, table, fn, table2)
```

## Descriptive Results

<img src="../../report/figures/descriptive-results-1.png" style="display: block; margin: auto;" />

## Balance Checks

The figure below shows that the data is unbalanced for the variables:
`Education`,`Income`, `Employment`, `Urbaness`, `Age`, `Position:
COVID`, `Political Efficacy`, `Ideology`, `Attitudes: Women in
Politics`, `Position: Education`, `Position: Defense`, `Position:
Immigration`, and `Immigration (%)`. As described in the Pre-Analysis
Plan (p.10), I will add these covariates to the analyses as controls.

``` r
source(here("src/analysis/balance-test.R"))
df
```

<img src="../../report/figures/balance-checks-1.png" style="display: block; margin: auto;" />

``` r
source(here("src/analysis/distribution_unbalanced_vars.R"))
p1 + p2 + p3 + p4 + plot_layout(ncol=2)
```

<img src="../../report/figures/visualize-unbalanced-vars-1.png" style="display: block; margin: auto;" />

## Gender Hypothesis

``` r
source(here("src/analysis/h1.R"))
p1
```

<img src="../../report/figures/h1-1.png" style="display: block; margin: auto;" />

## Migration Hypothesis

``` r
source(here("src/analysis/h2.R"))
p2
```

<img src="../../report/figures/h2-1.png" style="display: block; margin: auto;" />

## Intersection Hypothesis

``` r
source(here("src/analysis/h3.R"))
p3
```

<img src="../../report/figures/h3-1.png" style="display: block; margin: auto;" />

## Intersection Hypothesis – Pooled

``` r
source(here("src/analysis/h3-pooled.R"))
p3p
```

<img src="../../report/figures/h3-pooled-1.png" style="display: block; margin: auto;" />

## Exploration

### Ideology

<img src="../../report/figures/explore-ideology-1.png" style="display: block; margin: auto;" />

### Willingness to Accept Compromise

<img src="../../report/figures/explore-wtac-1.png" style="display: block; margin: auto;" />

### Attitude towards Women in Politics

<img src="../../report/figures/explore-women-politics-1.png" style="display: block; margin: auto;" />

### Position on COVID-19

<img src="../../report/figures/explore-covid-1.png" style="display: block; margin: auto;" />

### Position on Defense

<img src="../../report/figures/explore-defense-1.png" style="display: block; margin: auto;" />

### Position on Education

<img src="../../report/figures/explore-education-1.png" style="display: block; margin: auto;" />

### Position on Immigration

<img src="../../report/figures/explore-immigration-1.png" style="display: block; margin: auto;" />
