library(tidyverse)

ds_FA <- read_csv("sharing-dataset-for-analysis.csv")

S3_ds <- ds_FA %>%
  select(ID, DQ, S3_Resolved_IA_TR, allhazards_H9a_sev_exposure_30, allhazards_H9a_sev_exposure_30_IA, 
         normative_est_count_30_IA, allhazards_est_count_30_IA, disaster_h9a_sev_exposure_30,
         disaster_h9a_sev_exposure_30_IA, fd_freq_30_IA) %>%
  mutate(
    normative_ind_special = case_when(
      normative_est_count_30_IA == 0 & allhazards_est_count_30_IA == 0 ~ NA_real_,
      normative_est_count_30_IA == 0 & allhazards_est_count_30_IA > 0 ~ 0,
      normative_est_count_30_IA > 0 ~ 1
      # normative_est_count_30_IA > 20 ~ 2
    ),
    normative_ind_special = as.ordered(normative_ind_special),
    normative_ind_IA = as.ordered(if_else(normative_est_count_30_IA > 0, 1, normative_est_count_30_IA))
  )

ggplot(S3_ds, aes(x = disaster_h9a_sev_exposure_30_IA,
                  y = allhazards_H9a_sev_exposure_30_IA)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    x = "Disaster H9a Severity Exposure (IA)",
    y = "All Hazards H9a Severity Exposure (IA)"
  ) +
  theme_minimal()


mod1_ds <- S3_ds %>%
  filter(!is.na(S3_Resolved_IA_TR),
         !is.na(disaster_h9a_sev_exposure_30),
         # !is.na(normative_ind_IA),
  )

m1 <- glm(S3_Resolved_IA_TR ~ allhazards_H9a_sev_exposure_30,
          data = S3_ds, family = binomial)

m2 <- glm(S3_Resolved_IA_TR ~ allhazards_H9a_sev_exposure_30_IA,
          data = S3_ds, family = binomial)

summary(m1)
summary(m2)

summary(mod1)

# ia versus non ia
IA_ds <- S3_ds %>%
  mutate(
    IA_flag = if_else(
      is.na(allhazards_H9a_sev_exposure_30) &
        !is.na(allhazards_H9a_sev_exposure_30_IA),
      1, 0
    )
  )

ia_s3_compare <- IA_ds %>%
  select(ID, S3_Resolved_IA_TR, disaster_h9a_sev_exposure_30_IA, fd_freq_30_IA, IA_flag) %>%
  filter(!is.na(S3_Resolved_IA_TR),
         !is.na(disaster_h9a_sev_exposure_30_IA))

ia <- glm(S3_Resolved_IA_TR ~ IA_flag, data = IA_ds, family = binomial)

summary(ia)


test <- glm(S3_Resolved_IA_TR ~ allhazards_H9a_sev_exposure_30,
            data = IA_ds %>% filter(IA_flag == 0),
            family = binomial)

summary(test)

