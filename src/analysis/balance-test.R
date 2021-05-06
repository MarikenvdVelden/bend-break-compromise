covs <- d %>%
  mutate(compromise = if_else(compromise == 0, "No Compromise", "Compromise"),
         treatment = paste(name, compromise, sep = "-")) %>%
  select(treatment, PreT1:PreT8, F1:F9) 

balanced <-bal.tab(treatment ~ PreT1 + PreT2 + PreT3 + PreT4 +
                     PreT5 + PreT6 + PreT7_1 + PreT7_2 +
                     PreT7_3 + PreT7_4 + PreT7_5 + PreT7_6 + 
                     PreT7_7 + PreT8 +
                     factor(F1) + F2 +  factor(F3) +
                     F4 + factor(F5) + factor(F6) +
                     F7 + F8 + factor(F9),
                   data = covs,
                   thresholds = c(m = 0.05))[[1]] 

df <- balanced[1]$`Karel van der Kleijn-No Compromise vs. Karel van der Kleijn-Compromise`$Balance %>%
  add_case(balanced[2]$`Karin van der Kleijn-Compromise vs. Karel van der Kleijn-Compromise`$Balance) %>%
  add_case(balanced[3]$`Karin van der Kleijn-No Compromise vs. Karel van der Kleijn-Compromise`$Balance) %>%
  add_case(balanced[4]$`Rachid Amezian-Compromise vs. Karel van der Kleijn-Compromise`$Balance) %>%
  add_case(balanced[5]$`Rachid Amezian-No Compromise vs. Karel van der Kleijn-Compromise`$Balance) %>%
  add_case(balanced[6]$`Rachida Amezian-Compromise vs. Karel van der Kleijn-Compromise`$Balance) %>%
  add_case(balanced[7]$`Rachida Amezian-No Compromise vs. Karel van der Kleijn-Compromise`$Balance) %>%
  add_case(balanced[8]$`Karin van der Kleijn-Compromise vs. Karel van der Kleijn-No Compromise`$Balance) %>%
  add_case(balanced[9]$`Karin van der Kleijn-No Compromise vs. Karel van der Kleijn-No Compromise`$Balance) %>%
  add_case(balanced[10]$`Rachid Amezian-Compromise vs. Karel van der Kleijn-No Compromise`$Balance) %>%
  add_case(balanced[11]$`Rachid Amezian-No Compromise vs. Karel van der Kleijn-No Compromise`$Balance) %>%
  add_case(balanced[12]$`Rachida Amezian-Compromise vs. Karel van der Kleijn-No Compromise`$Balance) %>%
  add_case(balanced[13]$`Rachida Amezian-No Compromise vs. Karel van der Kleijn-No Compromise`$Balance) %>%
  add_case(balanced[14]$`Karin van der Kleijn-No Compromise vs. Karin van der Kleijn-Compromise`$Balance) %>%
  add_case(balanced[15]$`Rachid Amezian-Compromise vs. Karin van der Kleijn-Compromise`$Balance) %>%
  add_case(balanced[16]$`Rachid Amezian-No Compromise vs. Karin van der Kleijn-Compromise`$Balance) %>%
  add_case(balanced[17]$`Rachida Amezian-Compromise vs. Karin van der Kleijn-Compromise`$Balance) %>%
  add_case(balanced[18]$`Rachida Amezian-No Compromise vs. Karin van der Kleijn-Compromise`$Balance) %>%
  add_case(balanced[19]$`Rachid Amezian-Compromise vs. Karin van der Kleijn-No Compromise`$Balance) %>%
  add_case(balanced[20]$`Rachid Amezian-No Compromise vs. Karin van der Kleijn-No Compromise`$Balance) %>%
  add_case(balanced[21]$`Rachida Amezian-Compromise vs. Karin van der Kleijn-No Compromise`$Balance) %>%
  add_case(balanced[22]$`Rachida Amezian-No Compromise vs. Karin van der Kleijn-No Compromise`$Balance) %>%
  add_case(balanced[23]$`Rachid Amezian-No Compromise vs. Rachid Amezian-Compromise`$Balance) %>%
  add_case(balanced[24]$`Rachida Amezian-Compromise vs. Rachid Amezian-Compromise`$Balance) %>%
  add_case(balanced[25]$`Rachida Amezian-No Compromise vs. Rachid Amezian-Compromise`$Balance) %>%
  add_case(balanced[26]$`Rachida Amezian-Compromise vs. Rachid Amezian-No Compromise`$Balance) %>%
  add_case(balanced[27]$`Rachida Amezian-No Compromise vs. Rachid Amezian-No Compromise`$Balance) %>%
  add_case(balanced[28]$`Rachida Amezian-No Compromise vs. Rachida Amezian-Compromise`$Balance)
  
df <- df %>%
  mutate(variable = rep(c("Immigration (%)", "Position: Immigration", "Position: Defense", "Position:Education", 
                          "Attiitudes: Women in Politics",
                      "Ideology", "Poltical Efficacy (1)", 
                      "Poltical Efficacy (2)", "Poltical Efficacy (3)",
                      "Poltical Efficacy (4)", "Poltical Efficacy (5)",
                      "Poltical Efficacy (6)", "Poltical Efficacy (7)",
                      "Position: COVID",
                      "Gender: Male", "Gender: Female", "Gender: Other",
                      "Age", "Region: Big Cities","Region: West", "Region: North", "Region: East", 
                      "Region: South", "Urbaness", "Migration Background: None",
                      "Migration Background: 1st Generation Western", 
                      "Migration Background: 2nd Generation Western",
                      "Migration Background: 1st Generation Non-Western", 
                      "Migration Background: 2nd Generation Non-Western",
                      "50+", "Bij1", "BBB", "Blanco", "CDA",
                      "ChristenUnie", "D66", "Didn't vote", "Don't know",
                      "Denk", "Forum for Democracy","GroenLinks", "JA21", 
                      "Not eligible", "PvdA", "Animal Rights Party", "PVV", 
                      "SGP", "SP", "VOLT","VVD","Employment",
                      "Income",  "Education: High", "Education: Middle", "Education: Low"),28),
         variable = factor(variable,
                           levels = c("Immigration (%)", "Position: Immigration", "Position: Defense", "Position:Education", 
                                      "Attiitudes: Women in Politics",
                                      "Ideology", "Poltical Efficacy (1)",
                                      "Poltical Efficacy (2)", "Poltical Efficacy (3)",
                                      "Poltical Efficacy (4)", "Poltical Efficacy (5)",
                                      "Poltical Efficacy (6)", "Poltical Efficacy (7)",
                                      "Position: COVID",
                                      "Gender: Male", "Gender: Female", "Gender: Other",
                                      "Age", "Region: Big Cities","Region: West", "Region: North", "Region: East", 
                                      "Region: South", "Urbaness", "Migration Background: None",
                                      "Migration Background: 1st Generation Western", 
                                      "Migration Background: 2nd Generation Western",
                                      "Migration Background: 1st Generation Non-Western", 
                                      "Migration Background: 2nd Generation Non-Western",
                                      "50+", "Bij1", "BBB", "Blanco", "CDA",
                                      "ChristenUnie", "D66", "Didn't vote", "Don't know",
                                      "Denk", "Forum for Democracy","GroenLinks", "JA21", 
                                      "Not eligible", "PvdA", "Animal Rights Party", "PVV", 
                                      "SGP", "SP", "VOLT","VVD","Employment",
                                      "Income",  "Education: High", "Education: Middle", "Education: Low")),
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

rm(covs, balanced)