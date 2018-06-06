df_test %>%
  mutate(
    Factor_1 = case_when(
      Factor_1 >=2 ~ 2,
      TRUE ~ as.numeric(Factor_1)),
    Factor_2 = case_when(
      Factor_2 < -1 ~ -1,
      Factor_2 > 7 ~ 8,
      TRUE ~ as.numeric(Factor_2)),
    Factor_6 = case_when(
      Factor_6 <= -1 ~ -1,
      Factor_6 >= 2 ~ 2,
      TRUE ~ as.numeric(Factor_6)),
    Factor_7 = case_when(
      Factor_7 <= -1 ~ -1,
      Factor_7 >= 5 ~ 5,
      TRUE ~ as.numeric(Factor_7)),
    Factor_10 = case_when(
      Factor_10 < - 15 ~ -16,
      Factor_10 > 16 ~ 17,
      TRUE ~ as.numeric(Factor_10)),
    Factor_11 = case_when(
      Factor_11 > 2 ~ 3,
      TRUE ~ as.numeric(Factor_11)),
    Factor_12 = case_when(
      Factor_12 > 2 ~ 3,
      TRUE ~ as.numeric(Factor_12))
  )