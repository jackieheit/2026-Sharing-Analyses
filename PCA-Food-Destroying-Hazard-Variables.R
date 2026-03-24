library(tidyverse)
library(polycor)
library(psych)

# dataset is consistent with 3/24/2026 version of the 2025-calc-wide-SL-FA.R in the minerva GitHub
ds <- read.csv("../minerva/Hazards/Datasets/DT-hz-society-level-FA.csv")
ds <- ds %>%
  select(ID, OWC, all_freq_30_IA, sevnonfd_freq_30_IA, fd_freq_30_IA, all_freqdr_30_IA,
         all_freqsevdr_30_IA, all_freqLAPD_30_IA, all_freqfl_30_IA)

pca_vars <- c("all_freq_30_IA", "sevnonfd_freq_30_IA", "fd_freq_30_IA",
              "all_freqdr_30_IA", "all_freqsevdr_30_IA", "all_freqLAPD_30_IA", "all_freqfl_30_IA")

X_all <- ds %>%
  transmute(
    ID,
    all_freq_30_IA = factor(all_freq_30_IA, ordered = TRUE),
    sevnonfd_freq_30_IA = factor(sevnonfd_freq_30_IA, ordered = TRUE),
    fd_freq_30_IA = factor(fd_freq_30_IA, ordered = TRUE),
    all_freqdr_30_IA = factor(all_freqdr_30_IA, ordered = TRUE),
    all_freqsevdr_30_IA = factor(all_freqsevdr_30_IA, ordered = TRUE),
    all_freqLAPD_30_IA = factor(all_freqLAPD_30_IA, ordered = TRUE),
    all_freqfl_30_IA = factor(all_freqfl_30_IA, ordered = TRUE)
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
pc1_vars <- c("fd_freq_30_IA",#"all_freqdr_30_IA", 
              "all_freqsevdr_30_IA", "all_freqLAPD_30_IA")

X_pc1 <- ds %>%
  transmute(
    ID,
    fd_freq_30_IA = factor(fd_freq_30_IA, ordered = TRUE),
    # all_freqdr_30_IA = factor(all_freqdr_30_IA, ordered = TRUE),
    all_freqsevdr_30_IA = factor(all_freqsevdr_30_IA, ordered = TRUE),
    all_freqLAPD_30_IA = factor(all_freqLAPD_30_IA, ordered = TRUE)
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
pc2_vars <- c("all_freq_30_IA", "sevnonfd_freq_30_IA", "all_freqfl_30_IA")

X_pc2 <- ds %>%
  transmute(
    ID,
    all_freq_30_IA = factor(all_freq_30_IA, ordered = TRUE),
    sevnonfd_freq_30_IA = factor(sevnonfd_freq_30_IA, ordered = TRUE),
    all_freqfl_30_IA = factor(all_freqfl_30_IA, ordered = TRUE)
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
  all_freqsevdr_30_IA  = as.numeric(X_cc1$all_freqsevdr_30_IA),
  all_freqLAPD_30_IA   = as.numeric(X_cc1$all_freqLAPD_30_IA)
))
rownames(Z) <- rownames_df1  # use PC1’s IDs

PC1_freq <- as.numeric(scale(Z %*% W))
names(PC1_freq) <- rownames(Z)  # consistent naming

scores_df <- tibble(
  ID = as.character(names(PC1_freq)),
  fd_hazards_freq_PC = as.numeric(PC1_freq)
)

data_with_pc1 <- ds %>%
  mutate(ID = as.character(ID)) %>%
  left_join(scores_df, by = "ID")

write.csv(data_with_pc1, "2025-sl-hazards-with-fd-freq-PC.csv", row.names = FALSE)
