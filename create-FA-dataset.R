library(tidyverse)

sh_gh <- read_csv("../minerva/Sharing/Upstream/DT-food-labor-sharing-combined-clean.csv")

owc_identifier <- read_csv("data/EA-societal-complexity-variables-qc-Imputed-Values-Data.csv") %>%
  mutate(
    OWC = str_trim(OWC, side = c("both"))
  )

owc_identifier <- owc_identifier[, -c(4:10)] 

hz <- read_csv("2025-sl-hazards-with-fd-freq-PC.csv")
hz_cat <- read_csv("../minerva/Hazards/Datasets/DT-hzcats-society-level.csv") %>%
  select(ID, OWC, DQ, allhazards_H9a_sev_exposure_30, disaster_h9a_sev_exposure_30, 
         allhazards_H9a_sev_exposure_30_IA, disaster_h9a_sev_exposure_30_IA, normative_est_count_30, normative_est_count_30_IA, allhazards_est_count_30_IA)

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

write.csv(ds_FA, "sharing-dataset-for-analysis.csv", row.names =  FALSE)
