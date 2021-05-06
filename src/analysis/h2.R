## H2
issues <- unique(d$issue)
for(i in 1:length(issues)){
  df <- d %>% select(a = immigration, compromise, PT1:PT4, F2, F4, F7, F8, PreT2:PreT8, issue) %>%
    filter(issue == issues[i]) %>%
    select(-issue)
  if(i==1){
    h2 <- regression(df, a, compromise, 1) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression(df, a, compromise, 1) %>%
      mutate(issue = issues[i])
    h2 <- h2 %>% add_case(tmp)
  }
}

h2 <- h2 %>%
  filter(compromise==1) %>%
  mutate(y = recode(y,
                    `PT1` = "DV: Trait Evaluation",
                    `PT2` = "DV: Favorability",
                    `PT3` = "DV: Representation",
                    `PT4` = "DV: Career Prospects"),
         compromise = recode(compromise,
                             `1` = "Compromise (H2)")) %>%
  ggplot(aes(x = y, 
             y = AME,
             color = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_point(position = position_dodge(.2)) + 
  geom_errorbar(position = position_dodge(.2), width = 0) +
  geom_label_repel(nudge_x = 0.2,  nudge_y = 0.0, size = 4) + 
  theme_minimal() +
  labs(x = "", y = "Average Marginal Effects of Being a Politician with a Migration Background") +
  facet_grid(.~compromise, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()