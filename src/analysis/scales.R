PT_D1 <- d %>%
  select(matches("PT_D1_\\d")) %>%
  drop_na() %>%
  ltm::cronbach.alpha(probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

PT_C1 <- d %>%
  select(matches("PT_C1_\\d")) %>%
  drop_na() %>%
  ltm::cronbach.alpha(probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

PT_E1 <- d %>%
  select(matches("PT_E1_\\d")) %>%
  drop_na() %>%
  ltm::cronbach.alpha(probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)


PT_I1 <- d %>%
  select(matches("PT_I1_\\d")) %>%
  drop_na() %>%
  ltm::cronbach.alpha(probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)


PreT5 <- d %>%
  select(matches("PreT5_\\d")) %>%
  drop_na() %>%
  ltm::cronbach.alpha(probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

PreT7 <- d %>%
  select(matches("PreT7_\\d")) %>%
  drop_na() %>%
  mutate(PreT7_2 = 6 - PreT7_2,
         PreT7_6 = 6 - PreT7_6,
         PreT7_7 = 6 - PreT7_7) %>%
  ltm::cronbach.alpha(probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

scales <- tibble(Variable = c("DV: Trait Evaluation (Corona)",
                              "DV: Trait Evaluation (Defense)",
                              "DV: Trait Evaluation (Education)",
                              "DV: Trait Evaluation (Immigration)",
                              "Attitudes: Women in Politics",
                              "Political  Efficacy"),
                 `Cronbach's Alpha` = c(PT_C1[[1]], PT_D1[[1]], PT_E1[[1]],
                                        PT_I1[[1]], PreT5[[1]], PreT7[[1]]))

rm(PreT5, PreT7, PT_C1, PT_D1, PT_E1, PT_I1)
