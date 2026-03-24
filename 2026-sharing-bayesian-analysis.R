library(tidyverse)
library(brms)

sh_gh <- read_csv("../minerva/Sharing/Upstream/DT-food-labor-sharing-combined-clean.csv")
owc_identifier <- read_csv("data/EA-societal-complexity-variables-qc-Imputed-Values-Data.csv")
owc_identifier <- owc_identifier[, -c(4:10)]

hz <- read_csv("2025-sl-hazards-with-fd-freq-PC.csv")
hz_cat <- read_csv("../minerva/Hazards/Datasets/DT-hzcats-society-level.csv") %>%
  select(ID, OWC, DQ, allhazards_H9a_sev_exposure_30, disaster_h9a_sev_exposure_30, 
         allhazards_H9a_sev_exposure_30_IA, disaster_h9a_sev_exposure_30_IA)

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

#-------------------------------------------------------------------------------
# Now we will conduct a binary logistic bayesian regression analysis on Seasonal food sharing
#-------------------------------------------------------------------------------
data_clean_S3 <- ds_FA[complete.cases(ds_FA[, c("fd_hazards_freq_PC", "S3_Resolved_IA_TR")]), ]

# quasi complete separation...
ggplot(data_clean_S3, aes(x = fd_hazards_freq_PC, y = S3_Resolved_IA_TR)) +
  geom_jitter(height = 0.05, alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "#66c2a4")

data_clean_S3 %>%
  mutate(bin = cut(PC1_hz_fd_freq, breaks = seq(-2, 2, 0.5))) %>%
  group_by(bin) %>%
  summarise(mean_outcome = mean(S3_Resolved_IA_TR), n = n())

mod_S3 <- brm(
  formula = as.factor(S3_Resolved_IA_TR) ~ fd_hazards_freq_PC,
  data = data_clean_S3,
  family = bernoulli(link = "logit"),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  prior = set_prior("student_t(1, 0, 2.5)", class = "b")
)

summary(mod_S3)

mod_glm <- glm(
  as.factor(S3_Resolved_IA_TR) ~ fd_hazards_freq_PC,
  data = data_clean_S3,
  family = binomial(link = "logit")
)

summary(mod_glm)

pp_S3 <- pp_check(mod_S3, type = "bars", ndraws = 200) +
  scale_fill_manual(
    name = "Outcome Type",
    values = c("y" = "#1b9e77", "yrep" = "#d95f02"),
    labels = c("Observed (Real)", "Predicted (Replicated)")
  ) +
  ggtitle("Posterior predictive check: Seasonal food sharing") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  labs(x = "Food Destroying Hazards", y = "Density")

pp_S3

pp_check(mod_S3, type='error_scatter_avg', ndraws = 200)
hypothesis(mod_S3, "fd_hazards_freq_PC = 0")

# backed by previous results, we're curious to see if food destroying, severe droughts, and LAPD hazard frequencies increase the odds of seasonal food sharing
hypothesis(mod_S3, "PC1_hz_fd_freq > 0") # they do!

# however, model is over confident....weak prior is not helping the integrity of the results

draws1 <- as_draws_df(mod_S3)

# Plot for Sev
pS3 <- draws1 %>%
  ggplot(aes(x = b_fd_hazards_freq_PC)) +
  geom_density(fill = "purple", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = expression(beta ~ "(Food destroying hazards factor score)"),
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold")
  )

pS3



#-------------------------------------------------------------------------------
# binary logistic regression on daily food sharing
#-------------------------------------------------------------------------------
data_clean_S2 <- ds_FA[complete.cases(ds_FA[, c("fd_hazards_freq_PC", "S2_Resolved_IA_TR")]), ]

ggplot(data_clean_S2, aes(x = fd_hazards_freq_PC, y = S2_Resolved_IA_TR)) +
  geom_jitter(height = 0.05, alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "#66c2a4")

mod_S2 <- brm(
  formula = as.factor(S2_Resolved_IA_TR) ~ fd_hazards_freq_PC,
  data = data_clean_S2,
  family = bernoulli(link = "logit"),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  prior = set_prior("student_t(1, 0, 2.5)", class = "b")
)

summary(mod_S2)

pp_S2 <- pp_check(mod_S2, type = "bars", ndraws = 200) +
  scale_fill_manual(
    name = "Outcome Type",
    values = c("y" = "#1b9e77", "yrep" = "#d95f02"),
    labels = c("Observed (Real)", "Predicted (Replicated)")
  ) +
  ggtitle("Posterior predictive check: Daily food sharing") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  labs(x = "Food Destroying Hazards", y = "Density")

pp_S2

pp_check(mod_S2, type='error_scatter_avg', ndraws = 200)
pp_check(mod_S2, type = "loo_pit")       # calibration check

hypothesis(mod_S2, "fd_hazards_freq_PC = 0")

# we're curious to see if food destroying, severe droughts, and LAPD hazard frequencies decrease the odds of daily food sharing
# this was a new result from Carol's tau correlations
hypothesis(mod_S2, "fd_hazards_freq_PC < 0") # they do!

# however, model is over confident....weak prior is not helping the integrity of the results

prior_reg <- c(
  set_prior("normal(0, 0.5)", class = "b"),
  set_prior("student_t(3, 0, 2.5)", class = "Intercept")
)

mod_S2_reg <- brm(
  S2_Resolved_IA_TR ~ fd_hazards_freq_PC,
  data = data_clean_S2, family = bernoulli(link = "logit"),
  prior = prior_reg,
  chains = 4, iter = 4000, warmup = 2000, seed = 123,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

# basic diagnostics
summary(mod_S2_reg)   # check R-hat ~ 1, n_eff decent, no divergences
plot(mod_S2_reg)      # trace + density
pp_check(mod_S2_reg)                               # bars / density
pp_check(mod_S2_reg, type = "error_scatter_avg", ndraws = 200)   

hypothesis(mod_S2_reg, "fd_hazards_freq_PC < 0", digits = 6)

drawS2 <- as_draws_df(mod_S2_reg)

# Plot for Sev
pS2 <- drawS2 %>%
  ggplot(aes(x = b_fd_hazards_freq_PC)) +
  geom_density(fill = "purple", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = expression(beta ~ "(Food destroying hazards factor score)"),
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold")
  )

pS2
