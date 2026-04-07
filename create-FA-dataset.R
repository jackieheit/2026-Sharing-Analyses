library(tidyverse)

sh_gh <- read_csv("../minerva/Sharing/Upstream/DT-food-labor-sharing-combined-clean.csv")

owc_identifier <- read_csv("data/EA-societal-complexity-variables-qc-Imputed-Values-Data.csv") %>%
  mutate(
    OWC = str_trim(OWC, side = c("both"))
  )

owc_identifier <- owc_identifier[, -c(4:10)] 

hz <- read_csv("2025-sl-hazards-with-fd-freq-PC.csv")

sh_OWC <- full_join(owc_identifier, sh_gh, by = c("ID")) 

sh_FA <- sh_OWC %>%
  mutate(across(
    -c(1:3),
    ~ na_if(., 77) %>%
      na_if(88) %>%
      na_if(99)
  ))

ds_FA_1 <- full_join(hz_cat, sh_FA, by = c("ID", "OWC"))

ds_FA <- full_join(ds_FA_1, hz, by = c("ID","OWC"))

hz_cat <- read_csv("../minerva/Hazards/Datasets/DT-hzcats-society-level.csv") %>%
  select(ID, OWC, DQ, allhazards_H9a_sev_exposure_30, disaster_h9a_sev_exposure_30, 
         allhazards_H9a_sev_exposure_30_IA, disaster_h9a_sev_exposure_30_IA, normative_est_count_30, normative_est_count_30_IA, allhazards_est_count_30_IA)

hz_event <- read_csv("../minerva/Hazards/Datasets/DT-hz-event-level-expanded-FA.csv") %>%
  mutate(
    OWC = str_trim(OWC, side = c("both"))
  )
# dataset that contains identifier for the full hazard sample (e.g. minerva sample)
starting_sample <- read_csv("../minerva/Hazards/Datasets/DT-dq1-hz-clean.csv") %>%
  select(ID, OWC, D.1.) %>%
  mutate(
    OWC = str_trim(OWC, side = c("both"))
  )

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
  distinct(ID, OWC, D.1.)

soc_master <- full_join(soc_master, starting_sample, by = c("ID", "OWC", "D.1.")) %>%
  mutate(
    DQ = D.1.
  ) %>%
  select(-D.1.)

sevnonfd <- tf30 %>%
  filter(H.13. == 1) %>%
  mutate(
    DQ = D.1.,
    sevnonfd_max = pmax(H.9.b., H.9.c., H.9.d., na.rm = TRUE),
    sevnonfd_sev_wt_30 = case_when(
      sevnonfd_max %in% c(0.5, 1, 1.5) ~ 0,
      sevnonfd_max == 2   ~ 0.25,
      sevnonfd_max == 2.5 ~ 0.5,
      sevnonfd_max == 3   ~ 1,
      sevnonfd_max == 3.5 ~ 1.5,
      sevnonfd_max == 4   ~ 2,
      TRUE ~ NA_real_
    )
  ) %>% 
  group_by(ID, OWC, DQ) %>%
  summarise(
    disaster_nonfd_sev_exposure_30 =
      if_else(any(!is.na(sevnonfd_sev_wt_30)),
              sum(sevnonfd_sev_wt_30, na.rm = TRUE),
              NA_real_),
    .groups = "drop"
  )

ds_sevnonfd <- full_join(sevnonfd, soc_master, by = c("ID","OWC", "DQ")) %>%
  mutate(
    disaster_nonfd_sev_exposure_30_IA = case_when(
      is.na(disaster_nonfd_sev_exposure_30) & (DQ == 1 | DQ == 2) ~ 0,
      is.na(disaster_nonfd_sev_exposure_30) & (DQ == 3 | DQ == 4) ~ NA_real_,
      TRUE ~ disaster_nonfd_sev_exposure_30)
  )

ds_FA_sevnonfd <- full_join(ds_FA, ds_sevnonfd, by = c("ID", "OWC", "DQ"))

write.csv(ds_FA_sevnonfd, "sharing-dataset-for-analysis.csv", row.names =  FALSE)
