# Tidy Qualtrics Data
pret <- fetch_survey(surveyID = "SV_7aEiPm6fLzSon4O",
                     force_request = TRUE, verbose = TRUE,
                     label = FALSE, convert = FALSE) %>%
  filter(Consent == 1)  %>%
  select(matches("PreT\\d"), id = ResponseId) %>%
  select(-matches("PreT[26]_NPS_GROUP")) %>%
  unite(order_PreT5, matches("PreT5_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PreT7, matches("PreT7_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PreT, matches("Pre-Treatment_DO_PreT\\d+"), sep="|", na.rm = TRUE) %>%
  select(id, PreT1:PreT5_4, PreT6:PreT7_7, PreT8, 
         order_PreT5, order_PreT7, order_PreT) %>%
  mutate(across(matches("PreT5_\\d+"), 
                ~recode(., `15` = 5, `14` = 4, `13` = 3,
                        `12` = 2, `11` = 1))) 

tr <- fetch_survey(surveyID = "SV_7aEiPm6fLzSon4O",
                   force_request = TRUE, verbose = TRUE,
                   label = FALSE, convert = FALSE) %>%
  filter(Consent == 1) %>%
  select(matches("name-[A-Z]"), compromis, id = ResponseId)

pt <- fetch_survey(surveyID = "SV_7aEiPm6fLzSon4O",
                   force_request = TRUE, verbose = TRUE,
                   label = FALSE, convert = FALSE) %>%
  filter(Consent == 1) %>%
  select(matches("PT-[A-Z]\\d"), id = ResponseId) %>%
  select(-matches("PT-[A-Z]\\d_NPS_GROUP")) %>%
  unite(order_PT_D1, matches("PT-D1_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_D, matches("PostTreatment-Defense_DO_PT-D\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_E1, matches("PT-E1_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_E, matches("PostTreatment-Education_DO_PT-E\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_I1, matches("PT-I1_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_I, matches("PostTreatment-Immigration_DO_PT-I\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_C1, matches("PT-C1_DO_\\d+"), sep="|", na.rm = TRUE) %>%
  unite(order_PT_C, matches("PostTreatment-Corona_DO_PT-C\\d+"), sep="|", na.rm = TRUE) %>%
  select(id, `PT-D1_1`:`PT-D1_6`, `PT-D2`:`PT-I1_6`, `PT-I2`:`PT-E1_6`, 
         `PT-E2`: `PT-C1_6`, `PT-C2`:`PT-C4`, order_PT_D1, order_PT_D,
         order_PT_E1, order_PT_E, order_PT_I1, order_PT_I, 
         order_PT_C1, order_PT_C)

bg <- fetch_survey(surveyID = "SV_7aEiPm6fLzSon4O",
                   force_request = TRUE, verbose = TRUE,
                   label = FALSE, convert = FALSE) %>%
  filter(Consent == 1) %>%
  select(id = ResponseId, matches("F\\d"), etnicity) %>%
  unite(order_F6, matches("F6_DO_\\d+"), sep="|") %>%
  mutate(F1 = recode(F1, `1` = 0, `2` = 1, `3` = 2, `4` = 999)) %>%
  select(id, F1:F3, F4 = F4_1_TEXT, F5 = etnicity, F6, F7:F8, F9, order_F6)

d <- left_join(bg, pret, by = "id")
d <- left_join(d, tr, by = "id")
d <- left_join(d, pt, by = "id") 
rm(bg, pret, tr, pt)
