library(readxl)
library(tidyverse)
sl_hz_ds_30_FA <- read_csv("society-level-hazard-measures-30TF/sl_hz_ds_30_FA.csv")
group <- read_csv("society-level-hazard-measures-30TF/sl_analyses_cleaned_data/combineddatasetforcarol.csv")
group <- group %>%
  select(group, ID)
sharing <- read_csv("society-level-hazard-measures-30TF/sl_analyses_cleaned_data/DT-sharing-OWC-clean.csv")
sharing <- sharing %>%
  mutate(
    OWC = str_trim(sharing$OWC)
  ) %>%
  select(-eHRAF.Name, -SCCS.ID)
soc_complex <- read_csv("society-level-hazard-measures-30TF/sl_analyses_cleaned_data/soc_complex.csv")
soc_complex_QC <- read_csv("society-level-hazard-measures-30TF/sl_analyses_cleaned_data/matched-ds-scases-qc.csv")
soc_complex_QC <- soc_complex_QC %>%
  mutate(
    OWC = str_trim(soc_complex_QC$OWC)
         ) %>%
  select(-Society)

test <- left_join(sl_hz_ds_30_FA, soc_complex_QC, by = c("ID", "OWC")) # %>% select(ID, eHRAF.Name, NAME, DQ, hier_beyond_com, intensity_agricult, class_strat, community_size)
test2 <- left_join(test, group, by = c("ID"))
sl_hz30_sharing_societalcomp_ds <- left_join(test2, sharing, by = c("OWC")) %>%
  relocate(group, .before = DQ)


write.csv(sl_hz30_sharing_societalcomp_ds, "society-level-hazard-measures-30TF/sl_hz30_sharing_NEWsocietalcomp_ds.csv")

test_114 <- left_join(sl_hz_ds_30_FA, soc_complex, by = c("ID")) # %>% select(ID, eHRAF.Name, NAME, DQ, hier_beyond_com, intensity_agricult, class_strat, community_size)
add_group_114 <- left_join(test_114, group, by = c("ID"))
sl_hz30_sharing_ORIGINALsocietalcomp_ds <- left_join(test, sharing, by = c("OWC"))


write.csv(sl_hz30_sharing_ORIGINALsocietalcomp_ds, "society-level-hazard-measures-30TF/sl_hz30_sharing_ORIGINALsocietalcomp_ds.csv")