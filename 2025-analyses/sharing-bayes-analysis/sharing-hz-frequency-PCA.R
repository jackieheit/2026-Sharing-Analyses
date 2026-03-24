library(tidyverse)
library(polycor)
library(psych)
library(brms)

ds <- read.csv("data/sl_hz30_sharing_NEWsocietalcomp_ds.csv")
ds <- ds %>%
  select(ID, OWC, all_freq_30_IA, sevnonfd_freq_30_IA, fd_freq_30_IA, freqdr_30_IA,
         freqsevdr_30_IA, freqLAPD_30_IA, freqfl_30_IA, S2_Resolved_IA_TR, S3_Resolved_IA_TR)

pca_vars <- c("all_freq_30_IA", "sevnonfd_freq_30_IA", "fd_freq_30_IA",
              "freqdr_30_IA", "freqsevdr_30_IA", "freqLAPD_30_IA", "freqfl_30_IA")

X_all <- ds %>%
  transmute(
    ID,
    all_freq_30_IA = factor(all_freq_30_IA, ordered = TRUE),
    sevnonfd_freq_30_IA = factor(sevnonfd_freq_30_IA, ordered = TRUE),
    fd_freq_30_IA = factor(fd_freq_30_IA, ordered = TRUE),
    freqdr_30_IA = factor(freqdr_30_IA, ordered = TRUE),
    freqsevdr_30_IA = factor(freqsevdr_30_IA, ordered = TRUE),
    freqLAPD_30_IA = factor(freqLAPD_30_IA, ordered = TRUE),
    freqfl_30_IA = factor(freqfl_30_IA, ordered = TRUE)
  )

# Complete-case subset for PCA
X_cc <- X_all %>% drop_na(all_of(pca_vars)) %>%
  mutate(across(where(is.list), ~unlist(., use.names = FALSE))) %>%
  mutate(across(where(is.factor), droplevels))

rownames_df_all <- X_cc$ID
X_cc <- as.data.frame(X_cc %>% select(all_of(pca_vars)))

# Main PCA
hc <- polycor::hetcor(X_cc, use = "complete.obs", std.err = FALSE)
R  <- hc$correlations
ev <- eigen(R, symmetric = TRUE)$values
kaiser_n <- sum(ev > 1)
kaiser_n
n_obs <- nrow(X_cc)
pa <- psych::fa.parallel(R, n.obs = n_obs, fa = "pc", main = "Parallel Analysis (PCA)")
pc <- psych::principal(R, nfactors = 2, rotate = "none")
pc

# -------------------------------------------------------------------
# PC1 subset
pc1_vars <- c("fd_freq_30_IA", "freqsevdr_30_IA", "freqLAPD_30_IA")

X_pc1 <- ds %>%
  transmute(
    ID,
    fd_freq_30_IA = factor(fd_freq_30_IA, ordered = TRUE),
    freqsevdr_30_IA = factor(freqsevdr_30_IA, ordered = TRUE),
    freqLAPD_30_IA = factor(freqLAPD_30_IA, ordered = TRUE)
  )

X_cc1 <- X_pc1 %>% drop_na(all_of(pc1_vars)) %>%
  mutate(across(where(is.list), ~unlist(., use.names = FALSE))) %>%
  mutate(across(where(is.factor), droplevels))

rownames_df1 <- X_cc1$ID  # <- keep unique name here
X_cc1 <- as.data.frame(X_cc1 %>% select(all_of(pc1_vars)))

hc1 <- polycor::hetcor(X_cc1, use = "complete.obs", std.err = FALSE)
R1  <- hc1$correlations
ev <- eigen(R1, symmetric = TRUE)$values
kaiser_n <- sum(ev > 1)
kaiser_n
n_obs <- nrow(X_cc1)
pa1 <- psych::fa.parallel(R1, n.obs = n_obs, fa = "pc", main = "Parallel Analysis (PCA)")
pc1 <- psych::principal(R1, nfactors = 1, rotate = "none")
pc1

# -------------------------------------------------------------------
# PC2 subset
pc2_vars <- c("all_freq_30_IA", "sevnonfd_freq_30_IA", "freqfl_30_IA")

X_pc2 <- ds %>%
  transmute(
    ID,
    all_freq_30_IA = factor(all_freq_30_IA, ordered = TRUE),
    sevnonfd_freq_30_IA = factor(sevnonfd_freq_30_IA, ordered = TRUE),
    freqfl_30_IA = factor(freqfl_30_IA, ordered = TRUE)
  )

X_cc2 <- X_pc2 %>% drop_na(all_of(pc2_vars)) %>%
  mutate(across(where(is.list), ~unlist(., use.names = FALSE))) %>%
  mutate(across(where(is.factor), droplevels))

rownames_df2 <- X_cc2$ID  # <- also unique
X_cc2 <- as.data.frame(X_cc2 %>% select(all_of(pc2_vars)))

hc2 <- polycor::hetcor(X_cc2, use = "complete.obs", std.err = FALSE)
R2  <- hc2$correlations
ev <- eigen(R2, symmetric = TRUE)$values
kaiser_n <- sum(ev > 1)
kaiser_n
n_obs <- nrow(X_cc2)
pa2 <- psych::fa.parallel(R2, n.obs = n_obs, fa = "pc", main = "Parallel Analysis (PCA)")
pc2 <- psych::principal(R2, nfactors = 1, rotate = "none")
pc2

# -------------------------------------------------------------------
# Build PC1 component scores
L   <- as.matrix(unclass(pc1$loadings))
W   <- solve(R1) %*% L

Z <- scale(cbind(
  fd_freq_30_IA    = as.numeric(X_cc1$fd_freq_30_IA),
  freqsevdr_30_IA  = as.numeric(X_cc1$freqsevdr_30_IA),
  freqLAPD_30_IA   = as.numeric(X_cc1$freqLAPD_30_IA)
))
rownames(Z) <- rownames_df1  # use PC1’s IDs

PC1_freq <- as.numeric(scale(Z %*% W))
names(PC1_freq) <- rownames(Z)  # consistent naming

scores_df <- tibble(
  ID = as.character(names(PC1_freq)),
  PC1_hz_fd_freq = as.numeric(PC1_freq)
)

data_with_pc1 <- ds %>%
  mutate(ID = as.character(ID)) %>%
  left_join(scores_df, by = "ID")


#-------------------------------------------------------------------------------
# Now we will conduct a binary logistic bayesian regression analysis on Seasonal food sharing
#-------------------------------------------------------------------------------
data_clean_S3 <- data_with_pc1[complete.cases(data_with_pc1[, c("PC1_hz_fd_freq", "S3_Resolved_IA_TR")]), ]

# quasi complete seperation...
ggplot(data_clean_S3, aes(x = PC1_hz_fd_freq, y = S3_Resolved_IA_TR)) +
  geom_jitter(height = 0.05, alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "#66c2a4")

data_clean_S3 %>%
  mutate(bin = cut(PC1_hz_fd_freq, breaks = seq(-2, 2, 0.5))) %>%
  group_by(bin) %>%
  summarise(mean_outcome = mean(S3_Resolved_IA_TR), n = n())

data_clean_S3 <- data_clean_S3 %>%
  mutate(PC1_hz_fd_freq_z = scale(PC1_hz_fd_freq))

prior_new <- c(
  set_prior("normal(0, 1)", class = "b"),  # slope
  set_prior("student_t(3, 0, 2.5)", class = "Intercept")
)


mod_S3 <- brm(
  formula = as.factor(S3_Resolved_IA_TR) ~ PC1_hz_fd_freq,
  data = data_clean_S3,
  family = bernoulli(link = "logit"),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  prior = prior_new
)

summary(mod_S3)
hypothesis(mod_S3, "PC1_hz_fd_freq > 0")

prior_tighter <- c(
  prior(normal(0, 0.5), class = "b"),
  prior(normal(0, 1), class = "Intercept")
)

mod_S3_z <- brm(S3_Resolved_IA_TR ~ PC1_hz_fd_freq_z, 
                data = data_clean_S3, family = bernoulli(), 
                prior = prior_tighter)
summary(mod_S3_z)
hypothesis(mod_S3_z, "PC1_hz_fd_freq_z > 0")

test <- glm(S3_Resolved_IA_TR ~ PC1_hz_fd_freq, 
         data = data_clean_S3, family = binomial)

plot(test)

  
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
hypothesis(mod_S3, "PC1_hz_fd_freq = 0")

# backed by previous results, we're curious to see if food destroying, severe droughts, and LAPD hazard frequencies increase the odds of seasonal food sharing
hypothesis(mod_S3, "PC1_hz_fd_freq > 0") # they do!

# however, model is over confident....weak prior is not helping the integrity of the results

draws1 <- as_draws_df(mod_S3)

# Plot for Sev
pS3 <- draws1 %>%
  ggplot(aes(x = b_PC1_hz_fd_freq)) +
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
data_clean_S2 <- data_with_pc1[complete.cases(data_with_pc1[, c("PC1_hz_fd_freq", "S2_Resolved_IA_TR")]), ]

mod_S2 <- brm(
  formula = as.factor(S2_Resolved_IA_TR) ~ PC1_hz_fd_freq,
  data = data_clean_S2,
  family = bernoulli(link = "logit"),
  chains = 4,
  iter = 2000,
  warmup = 1000,
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

hypothesis(mod_S2, "PC1_hz_fd_freq = 0")

# we're curious to see if food destroying, severe droughts, and LAPD hazard frequencies decrease the odds of daily food sharing
# this was a new result from Carol's tau correlations
hypothesis(mod_S2, "PC1_hz_fd_freq < 0") # they do!

# however, model is over confident....weak prior is not helping the integrity of the results

prior_reg <- c(
  set_prior("normal(0, 0.5)", class = "b"),
  set_prior("student_t(3, 0, 2.5)", class = "Intercept")
)

mod_S2_reg <- brm(
  S2_Resolved_IA_TR ~ PC1_hz_fd_freq,
  data = data_clean_S3, family = bernoulli(link = "logit"),
  prior = prior_reg,
  chains = 4, iter = 4000, warmup = 2000, seed = 123,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

# basic diagnostics
summary(mod_S2_reg)   # check R-hat ~ 1, n_eff decent, no divergences
plot(mod_S2_reg)      # trace + density
pp_check(mod_S2_reg)                               # bars / density
pp_check(mod_S2_reg, type = "error_scatter_avg", ndraws = 200)   

hypothesis(mod_S2_reg, "PC1_hz_fd_freq < 0", digits = 6)

drawS2 <- as_draws_df(mod_S2_reg)

# Plot for Sev
pS2 <- drawS2 %>%
  ggplot(aes(x = b_PC1_hz_fd_freq)) +
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











