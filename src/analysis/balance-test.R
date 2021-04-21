covs <- d %>%
  mutate(treatment = paste(name, compromis, sep = "-")) %>%
  select(treatment, PreT1:PreT8, F1:F9) 

balanced <-bal.tab(Treatment ~ PreT1 + PreT2 + PreT3 + PreT4 +
                     PreT5 + PreT6 + PreT7 + PreT8 +
                     factor(F1) + F2 + factor(F9) + factor(F3) +
                     factor(F4) + factor(F5) + factor(F6) + F7 + F8,
                   Age + Education + factor(Sex_str), data = covs,
                   thresholds = c(m = 0.05))[[1]] 

balanced <- balanced %>%
  mutate(variable = c("Immigration (%)", "Immigration", "Defense", "Education", "Sexism",
                      "Ideology", "Poltical Efficacy", "COVID-19",
                      "Gender: Male", "Age", "Education: Middle", "Education: High",
                      "Region: West", "Region: North", "Region: East", "Region: South",
                      "Little Urban", "Moderatly Urban", "Highly Urban", 
                      "Very Highly Urban", "Migration Background", "Bij1","CDA",
                      "ChristenUnie", "D66", "Denk", "Forum for Democracy",
                      "GroenLinks", "JA21", "PvdA", "Animal Rights Party", "PVV", 
                      "SGP", "SP", "VOLT","VVD", "50Plus Party", "Employment",
                      "Income"),
         variable = factor(variable,
                           levels = c("Immigration (%)", "Immigration", "Defense",
                                      "Education", "Sexism", "Ideology", 
                                      "Poltical Efficacy","COVID-19", 
                                      "Gender: Male", "Age", "Education: Middle",
                                      "Education: High", "Region: West", "Region: North",
                                      "Region: East", "Region: South",
                                      "Little Urban", "Moderatly Urban", "Highly Urban",
                                      "Very Highly Urban", "Migration Background",
                                      "Bij1","CDA", "ChristenUnie", "D66", "Denk", 
                                      "Forum for Democracy", "GroenLinks", "JA21", 
                                      "PvdA", "Animal Rights Party", "PVV", 
                                      "SGP", "SP", "VOLT","VVD", "50Plus Party",
                                      "Employment", "Income")),
         difference = Diff.Un,2) %>%
  select(variable, difference) %>%
  ggplot(aes(x = variable, y = difference)) +
  geom_point(size = 3, colour = "gray55") +
  theme_bw() +
  labs(x="", y= "Standardized Mean Differences") +
  ggtitle("Covariate Balance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  geom_hline(yintercept = -0.05, linetype = "dashed") +
  coord_flip()