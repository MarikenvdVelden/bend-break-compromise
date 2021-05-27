# Tidy Qualtrics Data
pret <- read_sav(here("data/raw-private/experiment.sav")) %>%
  remove_all_labels() %>%
  filter(Consent == 1, Attention2_3 ==1, Attention2_5==1) %>%
  select(matches("PreT\\d"), id = ResponseId) %>%
  select(-matches("PreT[26]_NPS_GROUP")) %>%
  unite(order_PreT5, matches("PreT5_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PreT7, matches("PreT7_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PreT, matches("Pre_Treatment_DO_PreT\\d+"), sep="|", na.rm = TRUE) %>%
  select(id, PreT1:PreT5_4, PreT6:PreT7_7, PreT8, 
         order_PreT5, order_PreT7, order_PreT) %>%
  mutate(across(matches("PreT5_\\d+"), 
                ~recode(., `15` = 5, `14` = 4, `13` = 3,
                        `12` = 2, `11` = 1))) %>%
  drop_na()

tmp <- read_sav(here("data/raw-private/experiment.sav")) %>%
  remove_all_labels() %>%
  filter(Consent == 1, Attention2_3 ==1, Attention2_5==1) %>%
  select(matches("FL_"),id = ResponseId)

tr1 <- read_sav(here("data/raw-private/experiment.sav")) %>%
  remove_all_labels() %>%
  filter(Consent == 1, Attention2_3 == 1, Attention2_5 == 1) %>%
  select(matches("FL_"),id = ResponseId) %>%
  mutate(FL_32_DO_FL_104 = recode(FL_32_DO_FL_104, `1` = "Rachid Amezian", .default = "NO"),
         FL_32_DO_FL_106 = recode(FL_32_DO_FL_106, `1` = "Rachid Amezian", .default = "NO"),
         FL_32_DO_FL_108 = recode(FL_32_DO_FL_108, `1` = "Karel van der Kleijn", .default = "NO"),
         FL_32_DO_FL_110 = recode(FL_32_DO_FL_110, `1` = "Karel van der Kleijn", .default = "NO"),
         FL_32_DO_FL_112 = recode(FL_32_DO_FL_112, `1` = "Rachida Amezian", .default = "NO"),
         FL_32_DO_FL_114 = recode(FL_32_DO_FL_114, `1` = "Rachida Amezian", .default = "NO"),
         FL_32_DO_FL_116 = recode(FL_32_DO_FL_116, `1` = "Karin van der Kleijn", .default = "NO"),
         FL_32_DO_FL_118 = recode(FL_32_DO_FL_118, `1` = "Karin van der Kleijn", .default = "NO"),
         FL_78_DO_FL_123 = recode(FL_78_DO_FL_123, `1` = "Rachid Amezian", .default = "NO"),
         FL_78_DO_FL_125 = recode(FL_78_DO_FL_125, `1` = "Rachid Amezian", .default = "NO"),
         FL_78_DO_FL_129 = recode(FL_78_DO_FL_129, `1` = "Karel van der Kleijn", .default = "NO"),
         FL_78_DO_FL_132 = recode(FL_78_DO_FL_132, `1` = "Karel van der Kleijn", .default = "NO"),
         FL_78_DO_FL_134 = recode(FL_78_DO_FL_134, `1` = "Rachida Amezian", .default = "NO"),
         FL_78_DO_FL_137 = recode(FL_78_DO_FL_137, `1` = "Rachida Amezian", .default = "NO"),
         FL_78_DO_FL_139 = recode(FL_78_DO_FL_139, `1` = "Karin van der Kleijn", .default = "NO"),
         FL_78_DO_FL_142 = recode(FL_78_DO_FL_142, `1` = "Karin van der Kleijn", .default = "NO"),
         FL_84_DO_FL_144 = recode(FL_84_DO_FL_144, `1` = "Rachid Amezian", .default = "NO"),
         FL_84_DO_FL_145 = recode(FL_84_DO_FL_145, `1` = "Rachid Amezian", .default = "NO"),
         FL_84_DO_FL_148 = recode(FL_84_DO_FL_148, `1` = "Karel van der Kleijn", .default = "NO"),
         FL_84_DO_FL_151 = recode(FL_84_DO_FL_151, `1` = "Karel van der Kleijn", .default = "NO"),
         FL_84_DO_FL_153 = recode(FL_84_DO_FL_153, `1` = "Rachida Amezian", .default = "NO"),
         FL_84_DO_FL_157 = recode(FL_84_DO_FL_157, `1` = "Rachida Amezian", .default = "NO"),
         FL_84_DO_FL_159 = recode(FL_84_DO_FL_159, `1` = "Karin van der Kleijn", .default = "NO"),
         FL_84_DO_FL_161 = recode(FL_84_DO_FL_161, `1` = "Karin van der Kleijn", .default = "NO"),
         FL_101_DO_FL_163 = recode(FL_101_DO_FL_163, `1` = "Rachid Amezian", .default = "NO"),
         FL_101_DO_FL_165 = recode(FL_101_DO_FL_165, `1` = "Rachid Amezian", .default = "NO"),
         FL_101_DO_FL_167 = recode(FL_101_DO_FL_167, `1` = "Karel van der Kleijn", .default = "NO"),
         FL_101_DO_FL_169 = recode(FL_101_DO_FL_169, `1` = "Karel van der Kleijn", .default = "NO"),
         FL_101_DO_FL_171 = recode(FL_101_DO_FL_171, `1` = "Rachida Amezian", .default = "NO"),
         FL_101_DO_FL_173 = recode(FL_101_DO_FL_173, `1` = "Rachida Amezian", .default = "NO"),
         FL_101_DO_FL_175 = recode(FL_101_DO_FL_175, `1` = "Karin van der Kleijn", .default = "NO"),
         FL_101_DO_FL_177 = recode(FL_101_DO_FL_177, `1` = "Karin van der Kleijn", .default = "NO")) %>%
  pivot_longer(cols = c(FL_32_DO_FL_104:FL_32_DO_FL_118),
               values_to = "name_D") %>%
  drop_na("name_D") %>%
  select(-name) %>%
  pivot_longer(cols = c(FL_78_DO_FL_123:FL_78_DO_FL_142),
               values_to = "name_I") %>%
  drop_na("name_I") %>%
  select(-name) %>%
  pivot_longer(cols = c(FL_84_DO_FL_144:FL_84_DO_FL_161),
               values_to = "name_E") %>%
  drop_na("name_E") %>%
  select(-name) %>%
  pivot_longer(cols = c(FL_101_DO_FL_163:FL_101_DO_FL_177),
               values_to = "name_C") %>%
  drop_na("name_C") %>%
  select(-name)

tr2 <- read_sav(here("data/raw-private/experiment.sav")) %>%
  remove_all_labels() %>%
  filter(Consent == 1, Attention2_3 == 1, Attention2_5 == 1) %>%
  select(matches("FL_"),id = ResponseId) %>%
  mutate(FL_32_DO_FL_104 = recode(FL_32_DO_FL_104, `1` = "een compromis", .default = "NO"),
         FL_32_DO_FL_106 = recode(FL_32_DO_FL_106, `1` = "geen compromis", .default = "NO"),
         FL_32_DO_FL_108 = recode(FL_32_DO_FL_108, `1` = "een compromis", .default = "NO"),
         FL_32_DO_FL_110 = recode(FL_32_DO_FL_110, `1` = "geen compromis", .default = "NO"),
         FL_32_DO_FL_112 = recode(FL_32_DO_FL_112, `1` = "een compromis", .default = "NO"),
         FL_32_DO_FL_114 = recode(FL_32_DO_FL_114, `1` = "geen compromis", .default = "NO"),
         FL_32_DO_FL_116 = recode(FL_32_DO_FL_116, `1` = "een compromis", .default = "NO"),
         FL_32_DO_FL_118 = recode(FL_32_DO_FL_118, `1` = "geen compromis", .default = "NO"),
         FL_78_DO_FL_123 = recode(FL_78_DO_FL_123, `1` = "een compromis", .default = "NO"),
         FL_78_DO_FL_125 = recode(FL_78_DO_FL_125, `1` = "geen compromis", .default = "NO"),
         FL_78_DO_FL_129 = recode(FL_78_DO_FL_129, `1` = "een compromis", .default = "NO"),
         FL_78_DO_FL_132 = recode(FL_78_DO_FL_132, `1` = "geen compromis", .default = "NO"),
         FL_78_DO_FL_134 = recode(FL_78_DO_FL_134, `1` = "een compromis", .default = "NO"),
         FL_78_DO_FL_137 = recode(FL_78_DO_FL_137, `1` = "geen compromis", .default = "NO"),
         FL_78_DO_FL_139 = recode(FL_78_DO_FL_139, `1` = "een compromis", .default = "NO"),
         FL_78_DO_FL_142 = recode(FL_78_DO_FL_142, `1` = "geen compromis", .default = "NO"),
         FL_84_DO_FL_144 = recode(FL_84_DO_FL_144, `1` = "een compromis", .default = "NO"),
         FL_84_DO_FL_145 = recode(FL_84_DO_FL_145, `1` = "geen compromis", .default = "NO"),
         FL_84_DO_FL_148 = recode(FL_84_DO_FL_148, `1` = "een compromis", .default = "NO"),
         FL_84_DO_FL_151 = recode(FL_84_DO_FL_151, `1` = "geen compromis", .default = "NO"),
         FL_84_DO_FL_153 = recode(FL_84_DO_FL_153, `1` = "een compromis", .default = "NO"),
         FL_84_DO_FL_157 = recode(FL_84_DO_FL_157, `1` = "geen compromis", .default = "NO"),
         FL_84_DO_FL_159 = recode(FL_84_DO_FL_159, `1` =  "een compromis", .default = "NO"),
         FL_84_DO_FL_161 = recode(FL_84_DO_FL_161, `1` = "geen compromis", .default = "NO"),
         FL_101_DO_FL_163 = recode(FL_101_DO_FL_163, `1` = "een compromis", .default = "NO"),
         FL_101_DO_FL_165 = recode(FL_101_DO_FL_165, `1` = "geen compromis", .default = "NO"),
         FL_101_DO_FL_167 = recode(FL_101_DO_FL_167, `1` = "een compromis", .default = "NO"),
         FL_101_DO_FL_169 = recode(FL_101_DO_FL_169, `1` = "geen compromis", .default = "NO"),
         FL_101_DO_FL_171 = recode(FL_101_DO_FL_171, `1` = "een compromis", .default = "NO"),
         FL_101_DO_FL_173 = recode(FL_101_DO_FL_173, `1` = "geen compromis", .default = "NO"),
         FL_101_DO_FL_175 = recode(FL_101_DO_FL_175, `1` = "een compromis", .default = "NO"),
         FL_101_DO_FL_177 = recode(FL_101_DO_FL_177, `1` = "geen compromis", .default = "NO")) %>%
  pivot_longer(cols = c(FL_32_DO_FL_104:FL_32_DO_FL_118),
               values_to = "compromise_D") %>%
  drop_na("compromise_D") %>%
  select(-name) %>%
  pivot_longer(cols = c(FL_78_DO_FL_123:FL_78_DO_FL_142),
               values_to = "compromise_I") %>%
  drop_na("compromise_I") %>%
  select(-name) %>%
  pivot_longer(cols = c(FL_84_DO_FL_144:FL_84_DO_FL_161),
               values_to = "compromise_E") %>%
  drop_na("compromise_E") %>%
  select(-name) %>%
  pivot_longer(cols = c(FL_101_DO_FL_163:FL_101_DO_FL_177),
               values_to = "compromise_C") %>%
  drop_na("compromise_C") %>%
  select(-name)

tr <- inner_join(tr1, tr2, by = "id")

pt <- read_sav(here("data/raw-private/experiment.sav")) %>%
  remove_all_labels() %>%
  filter(Consent == 1, Attention2_3 ==1, Attention2_5==1) %>%
  select(matches("PT_[A-Z]\\d"), id = ResponseId) %>%
  select(-matches("PT_[A-Z]\\d_NPS_GROUP")) %>%
  unite(order_PT_D1, matches("PT_D1_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_D, matches("PostTreatment_Defense_DO_PT_D\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_E1, matches("PT_E1_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_E, matches("PostTreatment_Education_DO_PT_E\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_I1, matches("PT_I1_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_I, matches("PostTreatment_Immigration_DO_PT_I\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_C1, matches("PT_C1_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_C, matches("PostTreatment_Corona_DO_PT_C\\d+"), sep="|", na.rm = TRUE) %>%
  select(id, PT_C1_1:PT_C1_6, PT_C2:PT_D1_6,
         PT_D2:PT_E1_6, PT_E2:PT_I1_6, PT_I2:PT_I4,
         order_PT_C1, order_PT_C, order_PT_D1, order_PT_D,
         order_PT_E1, order_PT_E, order_PT_I1, order_PT_I)

bg <- read_sav(here("data/raw-private/experiment.sav")) %>%
  remove_all_labels() %>%
  filter(Consent == 1, Attention2_3 ==1, Attention2_5==1) %>%
  select(id = ResponseId, matches("F\\d"), etnicity) %>%
  unite(order_F6, matches("F6_DO_\\d+"), sep="|") %>%
  mutate(F1 = recode(F1, `1` = 0, `2` = 1, `3` = 2, `4` = 999),
         F1 = na_if(F1, 999),
         F3 = recode(F3, `1` = 1, `5` = 2, `8` = 3, `9` = 4, `10` = 5),
         F6 = recode(F6, `1` = "Bij1", `2` = "BBB", `3` = "CDA", `4` = "CU",
                     `5` = "D66", `6` = "Denk", `7` = "FvD", `8` = "JA21",
                     `9` = "GL", `10` = "PvdA", `11` = "PvdD", `12` = "PVV",
                     `13` = "SGP", `14` = "SP", `15` = "Volt", `16` = "VVD",
                     `17` = "50+", `18` = "Other", `19` = "Blanco",
                     `20` = "Don't know", `21` = "Didn't vote", `22` = "Not eligible"),
         F7 = recode(F7, `1` = 1, `4` = 2, `5` = 3, `6` = 4, `7` = 5,
                     `8` = 6, `9` = 7, `10` = 8),
         F8 = na_if(F8, 15),
         F8 = na_if(F8, 16),
         F9 = na_if(F9, 8)) %>%
  select(id, F1:F3, F4 = F4_1_TEXT, F5 = etnicity, F6, F7:F8, F9, order_F6) %>%
  mutate(F5 = recode(F5, `1` = 1, `2` = 2, `3` = 3, `5` = 4, `6` = 5))

d <- left_join(pret, bg, by = "id")
d <- left_join(d, tr, by = "id")
d <- left_join(d, pt, by = "id") 
rm(bg, pret, tr1, tr2, tr, pt)