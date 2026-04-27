library(tidyverse)

sh_gh <- read_csv("../minerva/Sharing/Upstream/DT-food-labor-sharing-combined-clean.csv")

owc_identifier <- read_csv("data/EA-societal-complexity-variables-qc-Imputed-Values-Data.csv") %>%
  mutate(
    OWC = str_trim(OWC, side = c("both"))
  ) %>% select(OWC, ID, eHRAF.Name, ea033)

hz <- read_csv("2025-sl-hazards-with-fd-freq-PC.csv")

sh_OWC <- full_join(owc_identifier, sh_gh, by = c("ID")) 

sh_FA <- sh_OWC %>%
  mutate(across(
    -c(1:3),
    ~ na_if(., 77) %>%
      na_if(88) %>%
      na_if(99)
  ))

hz_cat <- read_csv("../minerva/Hazards/Datasets/DT-hzcats-society-level.csv") %>%
  select(ID, OWC, DQ, allhazards_H9a_sev_exposure_30, disaster_h9a_sev_exposure_30, 
         allhazards_H9a_sev_exposure_30_IA, disaster_h9a_sev_exposure_30_IA, normative_est_count_30, normative_est_count_30_IA, allhazards_est_count_30_IA)

ds_FA_1 <- full_join(hz_cat, sh_FA, by = c("ID", "OWC"))

ds_FA <- full_join(ds_FA_1, hz, by = c("ID","OWC"))



hz_event <- read_csv("../minerva/Hazards/Datasets/DT-hz-event-level-expanded-FA.csv") %>%
  mutate(
    OWC = str_trim(OWC, side = c("both")),
    DQ = D.1.
  ) %>%
  select(-D.1.)

# dataset that contains identifier for the full hazard sample (e.g. minerva sample)
starting_sample <- read_csv("../minerva/Hazards/Datasets/DT-dq1-hz-clean.csv") %>%
  select(ID, OWC, D.1.) %>%
  mutate(
    OWC = str_trim(OWC, side = c("both")),
    DQ = D.1.
  ) %>%
  select(-D.1.)

# create sev nonfd exp variable

tf30 <- hz_event %>%
  mutate(
    # normalize H.3. formatting
    H.3. = gsub("\\s+", "", H.3.),
    
    # convert 99 -> NA
    across(c(H.8., H.7., H.9.a., H.9.b., H.9.c., H.9.d., H.11., H.10., H.13.), ~ na_if(.x, 99)),
    
    # assign time = 30 ONLY to threats whose H.3. includes 30
    time = if_else(
      H.11. == 2 & is.na(time) & H.3. %in% c("30", "30,60", "30,60,90"),
      30L,
      time
    )
  ) %>%
  filter(time == 30)

# master society frame that has every ID and DQ in event level dataset
soc_master <- tf30 %>%
  distinct(ID, OWC, DQ)

soc_master <- full_join(soc_master, starting_sample, by = c("ID", "OWC", "DQ"))

sevnonfd <- tf30 %>%
  filter(H.13. == 1) %>%
  mutate(
    sevnonfd_max = pmax(H.9.b., H.9.c., H.9.d., na.rm = TRUE),
    sevnonfd_sev_wt_30 = case_when(
      sevnonfd_max %in% c(0.5, 1, 1.5) ~ 0,
      sevnonfd_max == 2   ~ 0.25,
      sevnonfd_max == 2.5 ~ 0.5,
      sevnonfd_max == 3   ~ 1,
      sevnonfd_max == 3.5 ~ 1.5,
      sevnonfd_max == 4   ~ 2,
      TRUE ~ NA_real_
    ),
    sevnonfd_max_2.5 = ifelse(
      H.9.a. < 2.5 | is.na(H.9.a.),
      pmax(H.9.b., H.9.c., H.9.d., na.rm = TRUE),
      NA_real_
    ),
    
    sevnonfd_sev_wt2.5_30 = case_when(
      sevnonfd_max_2.5 %in% c(0.5, 1, 1.5) ~ 0,
      sevnonfd_max_2.5 == 2   ~ 0.25,
      sevnonfd_max_2.5 == 2.5 ~ 0.5,
      sevnonfd_max_2.5 == 3   ~ 1,
      sevnonfd_max_2.5 == 3.5 ~ 1.5,
      sevnonfd_max_2.5 == 4   ~ 2,
      TRUE ~ NA_real_
    ),
    sevnonfd_max_2 = ifelse(
      H.9.a. < 2 | is.na(H.9.a.),
      pmax(H.9.b., H.9.c., H.9.d., na.rm = TRUE),
      NA_real_
    ),
    
    sevnonfd_sev_wt2_30 = case_when(
      sevnonfd_max_2 %in% c(0.5, 1, 1.5) ~ 0,
      sevnonfd_max_2 == 2   ~ 0.25,
      sevnonfd_max_2 == 2.5 ~ 0.5,
      sevnonfd_max_2 == 3   ~ 1,
      sevnonfd_max_2 == 3.5 ~ 1.5,
      sevnonfd_max_2 == 4   ~ 2,
      TRUE ~ NA_real_
    )
  ) %>% 
  group_by(ID, OWC, DQ) %>%
  summarise(
    disaster_nonfd_sev_exposure_30 =
      if_else(any(!is.na(sevnonfd_sev_wt_30)),
              sum(sevnonfd_sev_wt_30, na.rm = TRUE),
              NA_real_),
    disaster_nonfd_sev_exposure2.5_30 =
      if_else(any(!is.na(sevnonfd_sev_wt2.5_30)),
              sum(sevnonfd_sev_wt2.5_30, na.rm = TRUE),
              NA_real_),
    disaster_nonfd_sev_exposure2_30 =
      if_else(any(!is.na(sevnonfd_sev_wt2_30)),
              sum(sevnonfd_sev_wt2_30, na.rm = TRUE),
              NA_real_),
    .groups = "drop"
  )

ds_sevnonfd <- full_join(sevnonfd, soc_master, by = c("ID","OWC", "DQ"))
  

nonfd <- tf30 %>%
  filter(H.13. %in% c(1, 2, 4) ) %>%
  mutate(
    nonfd_max = pmax(H.9.b., H.9.c., H.9.d., na.rm = TRUE),
    nonfd_sev_wt_30 = case_when(
      nonfd_max %in% c(0.5, 1, 1.5) ~ 0,
      nonfd_max == 2   ~ 0.25,
      nonfd_max == 2.5 ~ 0.5,
      nonfd_max == 3   ~ 1,
      nonfd_max == 3.5 ~ 1.5,
      nonfd_max == 4   ~ 2,
      TRUE ~ NA_real_
    ),
    nonfd_max_2.5 = ifelse(
      H.9.a. < 2.5 | is.na(H.9.a.),
      pmax(H.9.b., H.9.c., H.9.d., na.rm = TRUE),
      NA_real_),
    nonfd_sev_wt2.5_30 = case_when(
      nonfd_max_2.5 %in% c(0.5, 1, 1.5) ~ 0,
      nonfd_max_2.5 == 2   ~ 0.25,
      nonfd_max_2.5 == 2.5 ~ 0.5,
      nonfd_max_2.5 == 3   ~ 1,
      nonfd_max_2.5 == 3.5 ~ 1.5,
      nonfd_max_2.5 == 4   ~ 2,
      TRUE ~ NA_real_
    ),
    nonfd_max_2 = ifelse(
      H.9.a. < 2 | is.na(H.9.a.),
      pmax(H.9.b., H.9.c., H.9.d., na.rm = TRUE),
      NA_real_),
    nonfd_sev_wt2_30 = case_when(
      nonfd_max_2 %in% c(0.5, 1, 1.5) ~ 0,
      nonfd_max_2 == 2   ~ 0.25,
      nonfd_max_2 == 2.5 ~ 0.5,
      nonfd_max_2 == 3   ~ 1,
      nonfd_max_2 == 3.5 ~ 1.5,
      nonfd_max_2 == 4   ~ 2,
      TRUE ~ NA_real_
    )
  ) %>% 
  group_by(ID, OWC, DQ) %>%
  summarise(
    allhazards_nonfd_sev_exposure_30 =
      if_else(any(!is.na(nonfd_sev_wt_30)),
              sum(nonfd_sev_wt_30, na.rm = TRUE),
              NA_real_),
    allhazards_nonfd_sev_exposure2.5_30 =
      if_else(any(!is.na(nonfd_sev_wt2.5_30)),
              sum(nonfd_sev_wt2.5_30, na.rm = TRUE),
              NA_real_),
    allhazards_nonfd_sev_exposure2_30 =
      if_else(any(!is.na(nonfd_sev_wt2_30)),
              sum(nonfd_sev_wt2_30, na.rm = TRUE),
              NA_real_),
    .groups = "drop"
  )

nonfd_freq <- tf30 %>%
  filter(H.13. %in% c(1, 2, 4)) %>%
  mutate(
    nonfd_hz_ind = if_else(
      !is.na(H.9.b.) | !is.na(H.9.c.) | !is.na(H.9.d.),
      1, 0
    )
  ) %>%
  filter(nonfd_hz_ind == 1) %>%
  group_by(ID, OWC, DQ) %>%
  summarise(
    nonfd_2.5_est_count_30 = sum(H.11. >= 3 & (H.9.a. < 2.5 | is.na(H.9.a.)), na.rm = TRUE), # keep this variable
    nonfd_2.5_no_hz_count = sum(H.11. == 1 & (H.9.a. < 2.5 | is.na(H.9.a.)), na.rm = TRUE),
    nonfd_2.5_threat_count = sum(H.11. == 2 & (H.9.a. < 2.5 | is.na(H.9.a.)), na.rm = TRUE),
    nonfd_2.5_freq_30 = case_when(
      nonfd_2.5_est_count_30 == 1 ~ 3,
      nonfd_2.5_est_count_30 %in% 2:3 ~ 4,
      nonfd_2.5_est_count_30 %in% 4:9 ~ 5,
      nonfd_2.5_est_count_30 %in% 10:19 ~ 6,
      nonfd_2.5_est_count_30 >= 20 ~ 7,
      nonfd_2.5_threat_count > 0 ~ 2,
      nonfd_2.5_no_hz_count > 0 ~ 1,
      TRUE ~ NA
    ),
    
    nonfd_2_est_count_30 = sum(H.11. >= 3 & (H.9.a. < 2 | is.na(H.9.a.)), na.rm = TRUE), # keep this variable
    nonfd_2_no_hz_count = sum(H.11. == 1 & (H.9.a. < 2 | is.na(H.9.a.)), na.rm = TRUE),
    nonfd_2_threat_count = sum(H.11. == 2 & (H.9.a. < 2 | is.na(H.9.a.)), na.rm = TRUE),
    nonfd_2_freq_30 = case_when(
      nonfd_2_est_count_30 == 1 ~ 3,
      nonfd_2_est_count_30 %in% 2:3 ~ 4,
      nonfd_2_est_count_30 %in% 4:9 ~ 5,
      nonfd_2_est_count_30 %in% 10:19 ~ 6,
      nonfd_2_est_count_30 >= 20 ~ 7,
      nonfd_2_threat_count > 0 ~ 2,
      nonfd_2_no_hz_count > 0 ~ 1,
      TRUE ~ NA
    )
  ) %>%
  select(-c(nonfd_2.5_est_count_30, nonfd_2.5_no_hz_count, nonfd_2.5_threat_count, 
            nonfd_2_est_count_30, nonfd_2_no_hz_count, nonfd_2_threat_count))

nonfd <- full_join(nonfd, nonfd_freq, by = c("ID", "OWC", "DQ"))
ds_nonfd <- full_join(nonfd, soc_master, by = c("ID","OWC", "DQ"))

sevnonfd <- full_join(ds_nonfd, ds_sevnonfd, by = c("ID","OWC", "DQ"))
ds_nonfd <- full_join(sevnonfd, soc_master, by = c("ID","OWC", "DQ")) 

ds_FA_sevnonfd <- full_join(ds_FA, ds_nonfd, by = c("ID", "OWC", "DQ")) %>%
  mutate(
    disaster_nonfd_sev_exposure_30_IA = case_when(
      is.na(disaster_nonfd_sev_exposure_30) & (DQ == 1 | DQ == 2) ~ 0,
      is.na(disaster_nonfd_sev_exposure_30) & (DQ == 3 | DQ == 4) ~ NA_real_,
      TRUE ~ disaster_nonfd_sev_exposure_30),
    disaster_nonfd_sev_exposure2.5_30_IA = case_when(
      is.na(disaster_nonfd_sev_exposure2.5_30) & (DQ == 1 | DQ == 2) ~ 0,
      is.na(disaster_nonfd_sev_exposure2.5_30) & (DQ == 3 | DQ == 4) ~ NA_real_,
      TRUE ~ disaster_nonfd_sev_exposure2.5_30),
    disaster_nonfd_sev_exposure2_30_IA = case_when(
      is.na(disaster_nonfd_sev_exposure2_30) & (DQ == 1 | DQ == 2) ~ 0,
      is.na(disaster_nonfd_sev_exposure2_30) & (DQ == 3 | DQ == 4) ~ NA_real_,
      TRUE ~ disaster_nonfd_sev_exposure2_30),
    allhazards_nonfd_sev_exposure_30_IA = case_when(
      is.na(allhazards_nonfd_sev_exposure_30) & (DQ == 1 | DQ == 2) ~ 0,
      is.na(allhazards_nonfd_sev_exposure_30) & (DQ == 3 | DQ == 4) ~ NA_real_,
      TRUE ~ allhazards_nonfd_sev_exposure_30),
    allhazards_nonfd_sev_exposure2.5_30_IA = case_when(
      is.na(allhazards_nonfd_sev_exposure2.5_30) & (DQ == 1 | DQ == 2) ~ 0,
      is.na(allhazards_nonfd_sev_exposure2.5_30) & (DQ == 3 | DQ == 4) ~ NA_real_,
      TRUE ~ allhazards_nonfd_sev_exposure2.5_30),
    allhazards_nonfd_sev_exposure2_30_IA = case_when(
      is.na(allhazards_nonfd_sev_exposure2_30) & (DQ == 1 | DQ == 2) ~ 0,
      is.na(allhazards_nonfd_sev_exposure2_30) & (DQ == 3 | DQ == 4) ~ NA_real_,
      TRUE ~ allhazards_nonfd_sev_exposure2_30),
    nonfd_2.5_freq_30_IA = case_when(
      is.na(nonfd_2.5_freq_30) & (DQ == 1 | DQ == 2) ~ 1,
      is.na(nonfd_2.5_freq_30) & (DQ == 3 | DQ == 4) ~ NA_real_,
      TRUE ~ nonfd_2.5_freq_30),
    nonfd_2_freq_30_IA = case_when(
      is.na(nonfd_2_freq_30) & (DQ == 1 | DQ == 2) ~ 1,
      is.na(nonfd_2_freq_30) & (DQ == 3 | DQ == 4) ~ NA_real_,
      TRUE ~ nonfd_2_freq_30)
  )

write.csv(ds_FA_sevnonfd, "sharing-dataset-for-analysis.csv", row.names =  FALSE)
