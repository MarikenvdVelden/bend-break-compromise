## H2
issues <- unique(d$issue)
for(i in 1:length(issues)){
  df <- d %>% select(a = immigration, compromise, PT1:PT4, F2, F4, F7, F8, F9, PreT1:PreT8, issue) %>%
    filter(issue == issues[i]) %>%
    select(-issue)
  if(i==1){
    h2 <- regression(df, a, compromise) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression(df, a, compromise) %>%
      mutate(issue = issues[i])
    h2 <- h2 %>% add_case(tmp)
  }
}

p2 <- h2 %>%
  mutate(y = recode(y,
                    `PT1` = "DV: Trait Evaluation",
                    `PT2` = "DV: Favorability",
                    `PT3` = "DV: Representation",
                    `PT4` = "DV: Career Prospects"),
         compromise = recode(compromise,
                             `1` = "Compromise (H2)",
                             `0` = "No Compromise")) %>%
  ggplot(aes(x = y, 
             y = AME,
             color = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  geom_text(aes(y = upper +.15), position = position_dodge(.5),na.rm = TRUE) + 
  theme_minimal() +
  labs(x = "", y = "Average Marginal Effects of Being a Politician with a Migration Background",
       title = "Migration Hypothesis") +
  facet_grid(.~compromise) +
  ylim(c(-.7, .9)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()
