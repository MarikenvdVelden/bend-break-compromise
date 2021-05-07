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

tr <- read_sav(here("data/raw-private/experiment.sav")) %>%
  remove_all_labels() %>%
  filter(Consent == 1, Attention2_3 ==1, Attention2_5==1) %>%
  select(matches("name_[A-Z]"), compromis, id = ResponseId) %>%
  drop_na(name_I, name_E)

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
rm(bg, pret, tr, pt)