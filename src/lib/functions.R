library(here)
library(kableExtra)
library(tidyverse)
library(DeclareDesign)
library(ggpubr)
library(scales)
library(cobalt)
library(margins)
library(lme4)
library(ggstatsplot)
library(ggrepel)
library(haven)
library(sjlabelled)
library(patchwork)
fig_cols <- yarrr::piratepal(palette = "basel", 
             trans = .2)
fig_cols <- as.character(fig_cols[1:8])

regression <- function(df, a, compromise){
  
  depVarList <- df %>% select(matches("PT[0-9]"))
  indepVarList <- df %>% select(a, compromise, F2, F4, F7, F8, F9, PreT1:PreT8) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * compromise +
                                                   factor(F2) + F4 + F7 + F8 + F9 + PreT1 +
                                                   PreT2 + PreT3 + PreT4 + PreT5 +
                                                   PreT6 + PreT7_1 + PreT7_2 +
                                                   PreT7_3 + PreT7_4 + PreT7_5 + 
                                                   PreT7_6 + PreT7_7 + PreT8, 
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("PT[0-9]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "a", at = list(compromise = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, compromise)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "a", at = list(compromise = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, compromise)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression <- function(df, a, compromise, issue){
  
  depVarList <- df %>% select(matches("PT[0-9]"))
  indepVarList <- df %>% select(a, compromise, F2, F4, F7, F8, F9, PreT1:PreT8, issue) 
  allModels <- apply(depVarList,2,function(xl)lmer(xl ~ a * compromise +
                                                     F2 + F4 + F7 + F8 + F9 + PreT1 +
                                                     PreT2 + PreT3 + PreT4 + PreT5 +
                                                     PreT6 + PreT7_1 + PreT7_2 +
                                                     PreT7_3 + PreT7_4 + PreT7_5 + 
                                                     PreT7_6 + PreT7_7 + PreT8 +
                                                     (1 | issue),data= indepVarList))
  depVarList <- df %>% select(matches("PT[0-9]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "a", at = list(compromise = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, compromise)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "a", at = list(compromise = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, compromise)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

#' Render a template using jinja2 command line tool
render_j2 = function(template, output, data, auto_unbox=TRUE, na="string") {
  data = jsonlite::toJSON(data, pretty=TRUE, auto_unbox=auto_unbox, na=na)
  system(glue::glue("env/bin/j2 --format json {template} -o {output}"), input=data)
}
