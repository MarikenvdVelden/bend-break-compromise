df <- d %>%
  select(name, compromise, F2, F4, F7, F8, F9, PreT2:PreT8,issue)  %>%
  mutate(compromise = if_else(compromise==1, "Compromise", "No Compromise")) %>%
  mutate_at(c("F2", "F4", "F7", "F8", "PreT2", "PreT3", "PreT4", "PreT5", "PreT6",
              "PreT7_1", "PreT7_2", "PreT7_3", "PreT7_4", "PreT7_5", "PreT7_6",
              "PreT7_7", "PreT8"), ~(scale(.) %>% as.vector)) %>%
  pivot_longer(cols = F2:PreT8,
               names_to = "variables") %>%
  mutate(variables = recode(variables,
                       `F2` = "Age",
                       `F4` = "Urbaness",
                       `F7` = "Employment",
                       `F8` = "Income",
                       `F9` = "Education",
                       `PreT2`  = "Position: Immigration",
                       `PreT3`  = "Position: Defense",
                       `PreT4`  = "Position: Education",
                       `PreT5`  = "Attitude: Women in Politics",
                       `PreT6`  = "Ideology",
                       `PreT7_1` = "Political Efficacy (1)",
                       `PreT7_2` = "Political Efficacy (2)",
                       `PreT7_3` = "Political Efficacy (3)",
                       `PreT7_4` = "Political Efficacy (4)",
                       `PreT7_5` = "Political Efficacy (5)",
                       `PreT7_6` = "Political Efficacy (6)",
                       `PreT7_7` = "Political Efficacy (7)",
                       `PreT8` = "Position: COVID-19")) 

p1 <- df %>% filter(issue == "COVID") %>%
ggplot(aes(x=variables, y=value, fill=name, color=name)) +
  geom_violin(width=2.1, size=0.2) +
  facet_grid(name~compromise, scales = "free") +
  scale_fill_manual(values = fig_cols) +
  scale_color_manual(values = fig_cols) +
  labs(x = "", y = "", title = "COVID") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank(),
        axis.text=element_text(size=8)) +
  coord_flip() 

p2 <- df %>% filter(issue == "Defense") %>%
  ggplot(aes(x=variables, y=value, fill=name, color=name)) +
  geom_violin(width=2.1, size=0.2) +
  facet_grid(name~compromise, scales = "free") +
  scale_fill_manual(values = fig_cols) +
  scale_color_manual(values = fig_cols) +
  labs(x = "", y = "", title = "Defense") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank(),
        axis.text=element_text(size=8)) +
  coord_flip() 

p3 <- df %>% filter(issue == "Education") %>%
  ggplot(aes(x=variables, y=value, fill=name, color=name)) +
  geom_violin(width=2.1, size=0.2) +
  facet_grid(name~compromise, scales = "free") +
  scale_fill_manual(values = fig_cols) +
  scale_color_manual(values = fig_cols) +
  labs(x = "", y = "", title = "Education") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank(),
        axis.text=element_text(size=8)) +
  coord_flip() 

p4 <- df %>% filter(issue == "Immigration") %>%
  ggplot(aes(x=variables, y=value, fill=name, color=name)) +
  geom_violin(width=2.1, size=0.2) +
  facet_grid(name~compromise, scales = "free") +
  scale_fill_manual(values = fig_cols) +
  scale_color_manual(values = fig_cols) +
  labs(x = "", y = "", title = "Immigration") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank(),
        axis.text=element_text(size=8)) +
  coord_flip() 
