## H3
df <- d %>% select(a = intersection, compromise, PT1:PT4,issue, F2, F4, F7, F8, PreT2:PreT8)
h3p <- pooled_regression(df, a, compromise, issue, 1)


h3p <- h3p %>%
  mutate(id = if_else(y == "PT1", 1,
              if_else(y == "PT2", 2,
              if_else(y == "PT4", 3, 4))),
         y = recode(y,
                    `PT1` = "DV: Trait Evaluation",
                    `PT2` = "DV: Favorability",
                    `PT3` = "DV: Representation",
                    `PT4` = "DV: Career Prospects"),
         compromise = recode(compromise,
                             `1` = "Compromise (H3a)",
                             `0` = "No Compromise (H3b)")) %>%
  ggplot(aes(x = reorder(id, AME), 
             y = AME,
             color = y,
             ymin = lower,
             ymax = upper,
             label = y)) +
  geom_point(position = ) + geom_errorbar(width = 0) +
  geom_label_repel(nudge_y = 0.5, size = 2) + 
  theme_bw() +
  labs(x = "", y = "Average Marginal Effects of Being a Women Politician with a Migration Background") +
  facet_grid(.~compromise, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  coord_flip()
