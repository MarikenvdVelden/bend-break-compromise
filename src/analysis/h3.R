## H3
issues <- unique(d$issue)
for(i in 1:length(issues)){
  df <- d %>% select(a = intersection, compromise, PT1:PT4, issue) %>%
    filter(issue == issues[i]) %>%
    select(-issue)
  if(i==1){
    h3a <- regression(df, a, compromise, 1) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression(df, a, compromise, 1) %>%
      mutate(issue = issues[i])
    h3a <- h3a %>% add_case(tmp)
  }
}

for(i in 1:length(issues)){
  df <- d %>% select(a = intersection, compromise, PT1:PT4, issue) %>%
    filter(issue == issues[i]) %>%
    select(-issue)
  if(i==1){
    h3b <- regression(df, a, compromise, 0) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression(df, a, compromise, 0) %>%
      mutate(issue = issues[i])
    h3b <- h3b %>% add_case(tmp)
  }
}

h3a %>% add_case(h3b) %>%
  mutate(y = recode(y,
                    `PT1` = "DV: Trait Evaluation",
                    `PT2` = "DV: Favorability",
                    `PT3` = "DV: Representation",
                    `PT4` = "DV: Career Prospects"),
         n = recode(n,
                    `1` = "Compromise (H3a)",
                    `0` = "No Compromise (H3b)"),
         id = 1:(dim(h3a)[1]+dim(h3b)[1])) %>%
  ggplot(aes(x = reorder(id, AME), 
             y = AME,
             color = y,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = ) + geom_errorbar(width = 0) +
  theme_bw() +
  labs(x = "", y = "Average Marginal Effects of Being a Women Politician with a Migration Background") +
  facet_grid(issue~n, scales = "free_y") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y = element_blank()) +
  scale_color_viridis_d() +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  coord_flip()
