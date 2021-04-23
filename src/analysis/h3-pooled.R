## H3
df <- d %>% select(a = intersection, compromise, PT1:PT4, issue)
h3ap <- pooled_regression(df, a, compromise, issue, 1) %>%
  mutate(n = 1)
h3bp <- pooled_regression(df, a, compromise, issue, 0) %>%
  mutate(n = 0)

h3ap %>% add_case(h3bp) %>%
  mutate(y = recode(y,
                    `PT1` = "DV: Trait Evaluation",
                    `PT2` = "DV: Favorability",
                    `PT3` = "DV: Representation",
                    `PT4` = "DV: Career Prospects"),
         n = recode(n,
                    `1` = "Compromise (H3a - Pooled)",
                    `0` = "No Compromise (H3b - Pooled)"),
         id = 1:(dim(h3ap)[1]+dim(h3bp)[1])) %>%
  ggplot(aes(x = reorder(id, AME), 
             y = AME,
             color = y,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = ) + geom_errorbar(width = 0) +
  theme_bw() +
  labs(x = "", y = "Average Marginal Effects of Being a Women Politician with a Migration Background") +
  facet_grid(.~n, scales = "free_y") +
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
