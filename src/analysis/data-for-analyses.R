d <- d %>%
  mutate(`PT-D1` = round((`PT-D1_1` + `PT-D1_2` + `PT-D1_3` + `PT-D1_4` + `PT-D1_5` + `PT-D1_6`)/6, 0),
         `PT-E1` = round((`PT-E1_1` + `PT-E1_2` + `PT-E1_3` + `PT-E1_4` + `PT-E1_5` + `PT-E1_6`)/6, 0),
         `PT-C1` = round((`PT-C1_1` + `PT-C1_2` + `PT-C1_3` + `PT-C1_4` + `PT-C1_5` + `PT-C1_6`)/6, 0),
         `PT-I1` = round((`PT-I1_1` + `PT-I1_2` + `PT-I1_3` + `PT-I1_4` + `PT-I1_5` + `PT-I1_6`)/6, 0),
         PreT5 = round((PreT5_1 + PreT5_2 + PreT5_3 + PreT5_4)/4,0),
         PreT7 = round((PreT7_1 + PreT7_2 + PreT7_3 + PreT7_4 + PreT7_5 + PreT7_6 + PreT7_7)/7,0)) %>%
  pivot_longer(cols = c(`name-D`, `name-I`, `name-E`, `name-C`),
               values_to = "name",
               names_to = "issue") %>%
  drop_na(name) %>%
  mutate(issue = recode(issue, 
                        `name-C` = "COVID-19",
                        `name-D` = "Defense",
                        `name-E` = "Education",
                        `name-I` = "Immigration")) %>%
  pivot_longer(cols = c(`PT-D1`, `PT-E1`, `PT-C1`, `PT-I1`),
               values_to = "PT1",
               names_to = "issue1") %>%
  drop_na(PT1) %>%
  mutate(issue1 = recode(issue1, 
                        `PT-C1` = "COVID-19",
                        `PT-D1` = "Defense",
                        `PT-E1` = "Education",
                        `PT-I1` = "Immigration")) %>%
  pivot_longer(cols = c(`PT-D2`, `PT-E2`, `PT-C2`, `PT-I2`),
               values_to = "PT2",
               names_to = "issue2") %>%
  mutate(issue2 = recode(issue2, 
                        `PT-C2` = "COVID-19",
                        `PT-D2` = "Defense",
                        `PT-E2` = "Education",
                        `PT-I2` = "Immigration")) %>%
  pivot_longer(cols = c(`PT-D3`, `PT-E3`, `PT-C3`, `PT-I3`),
               values_to = "PT3",
               names_to = "issue3") %>%
  mutate(issue3 = recode(issue3, 
                         `PT-C3` = "COVID-19",
                         `PT-D3` = "Defense",
                         `PT-E3` = "Education",
                         `PT-I3` = "Immigration")) %>%
  pivot_longer(cols = c(`PT-D4`, `PT-E4`, `PT-C4`, `PT-I4`),
               values_to = "PT4",
               names_to = "issue4") %>%
  mutate(issue4 = recode(issue4, 
                         `PT-C4` = "COVID-19",
                         `PT-D4` = "Defense",
                         `PT-E4` = "Education",
                         `PT-I4` = "Immigration"),
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
         compromise = if_else(compromis == "een compromis",1, 0)) %>%
  select(id:F8, PreT1:PreT4, PreT5, PreT6, PreT7, PreT8, gender, immigration, intersection, compromise,
         issue, PT1, PT2, PT3, PT4)
  
