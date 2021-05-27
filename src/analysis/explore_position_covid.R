regression_explore <- function(df, a, b, compromise, issue){
  
  depVarList <- df %>% select(matches("PT[0-9]"))
  indepVarList <- df %>% select(a, b, compromise, F2, F4, F7, F8,  F9,
                                PreT1:PreT7_7, issue)   
  allModels <- apply(depVarList,2,function(xl)lmer(xl ~ a * compromise * b +
                                                     F2 + F4 + F7 + F8 + PreT1 +
                                                     PreT2 + PreT3 + PreT4 + PreT5 +
                                                     PreT6 + PreT7_1 + PreT7_2 +
                                                     PreT7_3 + PreT7_4 + PreT7_5 + 
                                                     PreT7_6 + PreT7_7 +
                                                     (1 | issue), 
                                                   data= indepVarList))
  depVarList <- df %>% select(matches("PT[0-9]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "a", at = list(compromise = 0:1, b = fivenum(df$b)))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, compromise, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "a", at = list(compromise = 0:1, b = fivenum(df$b)))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, compromise, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

df <- d %>% select(a = gender, b = PreT8, compromise, 
                   PT1:PT4, F2, F4, F7, F8, F9, PreT1:PreT7_7, issue) 
e1 <- regression_explore(df, a, b, compromise)

p1e <- e1 %>%
  mutate(y = recode(y,
                    `PT1` = "DV: Trait Evaluation",
                    `PT2` = "DV: Favorability",
                    `PT3` = "DV: Representation",
                    `PT4` = "DV: Career Prospects"),
         compromise = recode(compromise,
                             `1` = "Compromise (H1a)",
                             `0` = "No Compromise (H1b)")) %>%
  ggplot(aes(x = b, 
             y = AME,
             ymin = lower,
             ymax = upper)) +
  geom_line(color = fig_cols[1]) +
  geom_ribbon(fill = fig_cols[1], alpha = .4) +
  theme_minimal() +
  labs(x = "Position on COVID-19 Policies\n Negative (1) - Positive (5)", 
       y = "Average Marginal Effects of Being a Women Politician",
       title = "Gender Hypothesis") +
  facet_grid(y~compromise) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") 

df <- d %>% select(a = immigration, b = PreT8, compromise, 
                   PT1:PT4, F2, F4, F7, F8, F9, PreT1:PreT7_7, issue) 
e2 <- regression_explore(df, a, b, compromise)

p2e <- e2 %>%
  mutate(y = recode(y,
                    `PT1` = "DV: Trait Evaluation",
                    `PT2` = "DV: Favorability",
                    `PT3` = "DV: Representation",
                    `PT4` = "DV: Career Prospects"),
         compromise = recode(compromise,
                             `1` = "Compromise (H2)",
                             `0` = "No Compromise")) %>%
  ggplot(aes(x = b, 
             y = AME,
             ymin = lower,
             ymax = upper)) +
  geom_line(color = fig_cols[1]) +
  geom_ribbon(fill = fig_cols[1], alpha = .4) +
  theme_minimal() +
  labs(x = "Position on COVID-19 Policies\n Negative (1) - Positive (5)", 
       y = "Average Marginal Effects of Being a Politician with a Migration Background",
       title = "Migration Hypothesis") +
  facet_grid(y~compromise) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") 

df <- d %>% select(a = intersection, b = PreT8, compromise, 
                   PT1:PT4, F2, F4, F7, F8, F9, PreT1:PreT7_7, issue) 
e3 <- regression_explore(df, a, b, compromise)

p3e <- e3 %>%
  mutate(y = recode(y,
                    `PT1` = "DV: Trait Evaluation",
                    `PT2` = "DV: Favorability",
                    `PT3` = "DV: Representation",
                    `PT4` = "DV: Career Prospects"),
         compromise = recode(compromise,
                             `1` = "Compromise (H3a)",
                             `0` = "No Compromise (H3b)")) %>%
  ggplot(aes(x = b, 
             y = AME,
             ymin = lower,
             ymax = upper)) +
  geom_line(color = fig_cols[1]) +
  geom_ribbon(fill = fig_cols[1], alpha = .4) +
  theme_minimal() +
  labs(x = "Position on COVID-19 Policies\n Negative (1) - Positive (5)", 
       y = "Average Marginal Effects of Being a Women Politician with a Migration Background",
       title = "Intersection Hypothesis") +
  facet_grid(y~compromise) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") 
