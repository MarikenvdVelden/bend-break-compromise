#codes <- read_sav(here("data/raw-private-encrypted/experiment.sav")) %>%
#  filter(Consent == 1, Attention2_3 ==1, Attention2_5==1) %>%
#  select(F4_1_TEXT) %>%
#  distinct() %>%
#  write_csv(here("data/raw-private-encrypted/Urbaness_NL.csv"))

codes <- read_csv(here("data/raw-private-encrypted/Urbaness_NL.csv")) %>%
  select(Urbaness = F4, F4 = F4_1_TEXT)

d <- left_join(d, codes, by = "F4") %>%
  select(-F4) %>%
  select(id:F3, F4 = Urbaness, F5:order_PT_I)