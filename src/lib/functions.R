library(here)
library(tidyverse)
library(qualtRics)
library(DeclareDesign)
library(ggpubr)
library(scales)
library(margins)
library(lme4)

regression <- function(data, a, b){
  
  depVarList <- data %>% select(matches("PT[0-9]"))
  indepVarList <- data %>% select(a, b) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a*b, data= indepVarList))
  depVarList <- data %>% select(matches("PT[0-9]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]]), at = list(b = 1))[2,] %>%
        mutate(y = depVarList[i],
               lower = AME - (1.645 * SE),
               upper = AME + (1.645 * SE)) %>%
        select(AME, upper, lower, y)
    }
    else{
      tmp <- summary(margins(allModels[[i]]), at = list(b = 1))[2,] %>%
        mutate(y = depVarList[i],
               lower = AME - (1.645 * SE),
               upper = AME + (1.645 * SE)) %>%
        select(AME, upper, lower, y)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}
pooled_regression <- function(data, a, b, c){
  
  depVarList <- data %>% select(matches("PT[0-9]"))
  indepVarList <- data %>% select(a, b, c)
  allModels <- apply(depVarList,2,function(xl)lmer(xl ~ a * b + (1 | c),data= indepVarList))
  
  depVarList <- data %>% select(matches("PT[0-9]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]]), at = list(b = 1))[2,] %>%
        mutate(y = depVarList[i],
               lower = AME - (1.645 * SE),
               upper = AME + (1.645 * SE)) %>%
        select(AME, upper, lower, y)
    }
    else{
      tmp <- summary(margins(allModels[[i]]), at = list(b = 1))[2,] %>%
        mutate(y = depVarList[i],
               lower = AME - (1.645 * SE),
               upper = AME + (1.645 * SE)) %>%
        select(AME, upper, lower, y)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}
