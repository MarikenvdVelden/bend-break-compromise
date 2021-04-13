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
renv::snapshot()
```

    ## The following package(s) will be updated in the lockfile:
    ## 
    ## # CRAN ===============================
    ## - Rcpp        [* -> 1.0.5]
    ## - digest      [* -> 0.6.27]
    ## - evaluate    [* -> 0.14]
    ## - glue        [* -> 1.4.2]
    ## - here        [* -> 1.0.1]
    ## - highr       [* -> 0.8]
    ## - htmltools   [* -> 0.4.0]
    ## - jsonlite    [* -> 1.7.1]
    ## - knitr       [* -> 1.30]
    ## - magrittr    [* -> 1.5]
    ## - markdown    [* -> 1.1]
    ## - mime        [* -> 0.9]
    ## - printr      [* -> 0.1]
    ## - renv        [* -> 0.12.2]
    ## - rlang       [* -> 0.4.8]
    ## - rmarkdown   [* -> 2.5]
    ## - rprojroot   [* -> 2.0.2]
    ## - stringi     [* -> 1.5.3]
    ## - stringr     [* -> 1.4.0]
    ## - tinytex     [* -> 0.20]
    ## - xfun        [* -> 0.18]
    ## - yaml        [* -> 2.2.1]
    ## 
    ## * Lockfile written to '~/Dropbox/Papers/compromise-punish/src/data-processing/renv.lock'.

``` r
source(here::here("src/lib/functions.R"))
```

## Tidy Data

``` r
pret <- fetch_survey(surveyID = "SV_7aEiPm6fLzSon4O",
                  force_request = TRUE, verbose = TRUE,
                 label = FALSE, convert = FALSE) %>%
  filter(Consent == 1)  %>%
  select(matches("PreT\\d"), id = ResponseId) %>%
  select(-matches("PreT[26]_NPS_GROUP")) %>%
  unite(order_PreT5, matches("PreT5_DO_\\d+"), sep="|") %>%
  unite(order_PreT7, matches("PreT7_DO_\\d+"), sep="|") %>%
  unite(order_PreT, matches("Pre-Treatment_DO_PreT\\d+"), sep="|") %>%
  select(id, PreT1:PreT5_4, PreT6:PreT7_7, PreT8, 
         order_PreT5, order_PreT7, order_PreT) %>%
  mutate(across(matches("PreT5_\\d+"), 
            ~recode(., `15` = 5, `14` = 4, `13` = 3,
                    `12` = 2, `11` = 1))) 
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

``` r
tr <- fetch_survey(surveyID = "SV_7aEiPm6fLzSon4O",
                  force_request = TRUE, verbose = TRUE,
                  label = FALSE, convert = FALSE) %>%
  filter(Consent == 1) %>%
  select(matches("C-[A-Z]\\d"), id = ResponseId) %>%
  pivot_longer(cols = `C-D1`:`C-D8`,
               names_to = "Def") %>%
  drop_na(value) %>%
  select(-value) %>%
  pivot_longer(cols = `C-E1`:`C-E8`,
               names_to = "Edu") %>%
  drop_na(value) %>%
  select(-value) %>%
  pivot_longer(cols = `C-I1`:`C-I8`,
               names_to = "Imm") %>%
  drop_na(value) %>%
  select(-value) %>%
  pivot_longer(cols = `C-C1`:`C-C8`,
               names_to = "COVID") %>%
  drop_na(value) %>%
  select(-value) %>%
  mutate(across(-id, ~str_sub(., -1))) %>%
  mutate(across(-id, ~recode(.,
                       `1` = "Rachid Amezian + Compromis",
                       `2` = "Rachid Amezian + Geen compromis",
                       `3` = "Karel van der Kleijn + Compromis",
                       `4` = "Karel van der Kleijn + Geen compromis",
                       `5` = "Rachida Amezian + Compromis",
                       `6` = "Rachida Amezian + Geen compromis",
                       `7` = "Karin van der Kleijn + Compromis",
                       `8` = "Karin van der Kleijn + Geen compromis")))
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

``` r
pt <- fetch_survey(surveyID = "SV_7aEiPm6fLzSon4O",
                  force_request = TRUE, verbose = TRUE,
                 label = FALSE, convert = FALSE) %>%
  filter(Consent == 1) %>%
  select(matches("PT-[A-Z]\\d"), id = ResponseId) %>%
  select(-matches("PT-[A-Z]\\d_NPS_GROUP")) %>%
  unite(order_PT_D1, matches("PT-D1_DO_\\d+"), sep="|") %>%
  unite(order_PT_D, matches("PostTreatment-Defense_DO_PT-D\\d+"), sep="|") %>%
  unite(order_PT_E1, matches("PT-E1_DO_\\d+"), sep="|") %>%
  unite(order_PT_E, matches("PostTreatment-Education_DO_PT-E\\d+"), sep="|") %>%
  unite(order_PT_I1, matches("PT-I1_DO_\\d+"), sep="|") %>%
  unite(order_PT_I, matches("PostTreatment-Immigration_DO_PT-I\\d+"), sep="|") %>%
  unite(order_PT_C1, matches("PT-C1_DO_\\d+"), sep="|") %>%
  unite(order_PT_C, matches("PostTreatment-Corona_DO_PT-C\\d+"), sep="|") %>%
  select(id, `PT-D1_1`:`PT-D1_6`, `PT-D2`:`PT-I1_6`, `PT-I2`:`PT-E1_6`, 
         `PT-E2`: `PT-C1_6`, `PT-C2`:`PT-C4`, order_PT_D1, order_PT_D,
         order_PT_E1, order_PT_E, order_PT_I1, order_PT_I, 
         order_PT_C1, order_PT_C)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

``` r
bg <- fetch_survey(surveyID = "SV_7aEiPm6fLzSon4O",
                  force_request = TRUE, verbose = TRUE,
                 label = FALSE, convert = FALSE) %>%
  filter(Consent == 1) %>%
  select(id = ResponseId, matches("F\\d"), etnicity) %>%
  unite(order_F6, matches("F6_DO_\\d+"), sep="|") %>%
  mutate(F1 = recode(F1, `1` = 0, `2` = 1, `3` = 2, `4` = 999)) %>%
  select(id, F1:F3, F4 = F4_1_TEXT, F5 = etnicity, F6, F7:F8, order_F6)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

``` r
d <- left_join(bg, pret, by = "id")
d <- left_join(d, tr, by = "id")
d <- left_join(d, pt, by = "id")
rm(bg, pret, tr, pt)
```

## Save Data for Analysis

``` r
save(d, file = here("data/intermediate/cleaned_data.RData"))
```

## Visualization of Data

### Dependent Variable

### Independent Variable

### Correlations Matrix
