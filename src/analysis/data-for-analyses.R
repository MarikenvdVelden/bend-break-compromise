d <- d %>%
  mutate(PT_D1 = round((PT_D1_1 + PT_D1_2 + PT_D1_3 + PT_D1_4 + PT_D1_5 + PT_D1_6)/6, 0),
         PT_E1 = round((PT_E1_1 + PT_E1_2 + PT_E1_3 + PT_E1_4 + PT_E1_5 + PT_E1_6)/6, 0),
         PT_C1 = round((PT_C1_1 + PT_C1_2 + PT_C1_3 + PT_C1_4 + PT_C1_5 + PT_C1_6)/6, 0),
         PT_I1 = round((PT_I1_1 + PT_I1_2 + PT_I1_3 + PT_I1_4 + PT_I1_5 + PT_I1_6)/6, 0),
         PreT5 = round((PreT5_1 + PreT5_2 + PreT5_3 + PreT5_4)/4,0),
         PreT7_2 = 6 - PreT7_2,
         PreT7_6 = 6 - PreT7_6,
         PreT7_7 = 6 - PreT7_7) %>%
  pivot_longer(cols = c(`name_D`, `name_I`, `name_E`, `name_C`),
               values_to = "name",
               names_to = "issue") %>%
  drop_na(name) %>%
  mutate(issue = recode(issue, 
                        `name_C` = "COVID",
                        `name_D` = "Defense",
                        `name_E` = "Education",
                        `name_I` = "Immigration")) %>%
  pivot_longer(cols = c(`PT_D1`, `PT_E1`, `PT_C1`, `PT_I1`),
               values_to = "PT1",
               names_to = "issue1") %>%
  drop_na(PT1) %>%
  mutate(issue1 = recode(issue1, 
                        `PT_C1` = "COVID",
                        `PT_D1` = "Defense",
                        `PT_E1` = "Education",
                        `PT_I1` = "Immigration")) %>%
  pivot_longer(cols = c(`PT_D2`, `PT_E2`, `PT_C2`, `PT_I2`),
               values_to = "PT2",
               names_to = "issue2") %>%
  mutate(issue2 = recode(issue2, 
                        `PT_C2` = "COVID",
                        `PT_D2` = "Defense",
                        `PT_E2` = "Education",
                        `PT_I2` = "Immigration")) %>%
  pivot_longer(cols = c(`PT_D3`, `PT_E3`, `PT_C3`, `PT_I3`),
               values_to = "PT3",
               names_to = "issue3") %>%
  mutate(issue3 = recode(issue3, 
                         `PT_C3` = "COVID",
                         `PT_D3` = "Defense",
                         `PT_E3` = "Education",
                         `PT_I3` = "Immigration")) %>%
  pivot_longer(cols = c(`PT_D4`, `PT_E4`, `PT_C4`, `PT_I4`),
               values_to = "PT4",
               names_to = "issue4") %>%
  mutate(issue4 = recode(issue4, 
                         `PT_C4` = "COVID",
                         `PT_D4` = "Defense",
                         `PT_E4` = "Education",
                         `PT_I4` = "Immigration"),
        check = if_else(issue == issue1 & issue == issue2 & issue == issue3 & issue == issue4, 1,0)) %>%
  filter(check == 1) %>%
  mutate(gender = recode(name,
                         `Karel van der Kleijn` = 0,
                         `Karin van der Kleijn` = 1,
                         `Rachid Amezian` = 0,
                         `Rachida Amezian` = 1),
         immigration = recode(name,
                              `Karel van der Kleijn` = 0,
                              `Karin van der Kleijn` = 0,
                              `Rachid Amezian` = 1,
                              `Rachida Amezian` = 1),
         intersection = recode(name,
                               `Karel van der Kleijn` = 0,
                               `Karin van der Kleijn` = 0,
                               `Rachid Amezian` = 0,
                               `Rachida Amezian` = 1),
         compromise = if_else(compromis == "een compromis",1, 0),
         F9 = recode(F9, `1` = 1, `2` = 1, `3` = 2, `4` = 2, `5` = 3, `6` = 3, `7` = 3)) %>%
  select(id, F1,F2,F3, F4, F5, F6, F7, F8,F9, PreT1:PreT4, PreT5, PreT6, PreT7_1:PreT7_7, PreT8,
         name, gender, immigration, intersection, compromise,
         issue, PT1, PT2, PT3, PT4) 
