library(here)
library(tidyverse)
library(qualtRics)
library(DeclareDesign)
library(ggpubr)
library(scales)
library(margins)
library(lme4)

regression <- function(df, a, compromise, n){
  
  depVarList <- df %>% select(matches("PT[0-9]"))
  indepVarList <- df %>% select(a, compromise) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * compromise, data= indepVarList))
  depVarList <- df %>% select(matches("PT[0-9]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]]), at = list(compromise = n))[2,] %>%
        mutate(y = depVarList[i],
               n = n,
               lower = AME - (1.645 * SE),
               upper = AME + (1.645 * SE)) %>%
        select(AME, upper, lower, y, n)
    }
    else{
      tmp <- summary(margins(allModels[[i]]), at = list(compromise = n))[2,] %>%
        mutate(y = depVarList[i],
               n = n,
               lower = AME - (1.645 * SE),
               upper = AME + (1.645 * SE)) %>%
        select(AME, upper, lower, y, n)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression <- function(df, a, compromise, issue, n){
  
  depVarList <- df %>% select(matches("PT[0-9]"))
  indepVarList <- df %>% select(a, compromise, issue)
  allModels <- apply(depVarList,2,function(xl)lmer(xl ~ a * compromise + (1 | issue),data= indepVarList))
  depVarList <- df %>% select(matches("PT[0-9]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]]), at = list(compromise = n))[2,] %>%
        mutate(y = depVarList[i],
               n = n,
               lower = AME - (1.645 * SE),
               upper = AME + (1.645 * SE)) %>%
        select(AME, upper, lower, y, n)
    }
    else{
      tmp <- summary(margins(allModels[[i]]), at = list(compromise = n))[2,] %>%
        mutate(y = depVarList[i],
               n = n,
               lower = AME - (1.645 * SE),
               upper = AME + (1.645 * SE)) %>%
        select(AME, upper, lower, y, n)
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
