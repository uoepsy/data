# simulate psmu_darktriad.csv
# Based on: https://link.springer.com/article/10.1007/s11469-018-9900-1

library(tidyverse)

set.seed(1)

n_obs <- 80

# correls with PSMU = same or more extreme versions of path coefs in Fig 2
corr_MachPSMU  <-  0.23
corr_PsycPSMU  <- -0.10
corr_NarcPSMU  <-  0.40

# inter-predictor correlations from Table 2 in paper
corr_MachPsyc  <-  0.57
corr_MachNarc  <-  0.47
corr_PsycNarc  <-  0.38

corr_df <- MASS::mvrnorm(
  n = n_obs,
  mu = c(-0.8, 0, 0, 0),
  Sigma = Matrix::nearPD(
    matrix(
      c(1,             corr_MachPSMU, corr_PsycPSMU, corr_NarcPSMU,
        corr_MachPSMU, 1,             corr_MachPsyc, corr_MachNarc,
        corr_PsycPSMU, corr_MachPsyc, 1,             corr_PsycNarc,
        corr_NarcPSMU, corr_MachNarc, corr_PsycNarc, 1
      ),
      nrow = 4)
  )$mat,
  empirical = TRUE
)

colnames(corr_df) <- c('PSMU_logodds', 'Mach_z', 'Psyc_z', 'Narc_z')

psmu_data <- as_tibble(corr_df) |>
  rownames_to_column(var = 'subjID') |>
  mutate(
     PSMU = ifelse(PSMU_logodds > 0, 1, 0),
  ) |>
  mutate(across(Mach_z:Narc_z, \(x) round(x, 3))) |>
  select(-PSMU_logodds)

write_csv(psmu_data, 'psmu_darktriad.csv')

# table(psmu_data$PSMU) / 80
# m1 <- glm(PSMU ~ Mach_z + Psyc_z + Narc_z, data = psmu_data, family = binomial)
# summary(m1)
# car::vif(m1)
# rstandard(m1, type = 'deviance') |> plot()
# plot(m1, which = 4)
