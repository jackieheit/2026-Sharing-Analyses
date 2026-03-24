library(tidyverse)
library(brms)
library(bayesplot)
library(bayestestR)

pnas <- read_csv("C:/Users/heitm/OneDrive - St. Lawrence University/HRAF_Internship/jackies-mind-hub/datasets/PNAS_hypothesis_variables.csv")

hz <- read_csv("C:/Users/heitm/OneDrive - St. Lawrence University/HRAF_Internship/jackies-mind-hub/datasets/DT-hz-society-level-30-FA.csv")

sharing <- read_csv("data/sl_hz30_sharing_NEWsocietalcomp_ds.csv")
colnames(sharing)

# in this bayes analysis we want to investigate types of food destroying hazards and whether or not
# some types may be more important than others in terms of impact to both daily and seasonal food sharing
# are there some types of food destroying hazards that affect these mechanisms more or less?


ds <- sharing %>%
  filter(!is.na(S2_Resolved_IA_TR) & !is.na(fd_freq_30_IA))




fd_mod1 <- brm(S2_Resolved_IA_TR ~ factor(fd_freq_30_IA),
    family = bernoulli(),
    data = ds,
    prior = c(
      prior(normal(0,1), class = "b"),
      prior(normal(0,1), class = "Intercept")
    ),
    chains = 4,
    iter = 4000,
    cores = 4
)

summary(fd_mod1)
mcmc_trace(fd_mod1)
