# simulate psmu_darktriad.csv
# Based on: https://link.springer.com/article/10.1007/s11469-018-9900-1

library(tidyverse)

set.seed(1)

n_obs <- 80

# correls with PSMU from path coefs in Fig 2
corr_MachPSMU  <-  0.23
corr_PsycPSMU  <- -0.20
corr_NarcPSMU  <-  0.30

# inter-predictor correlations from Table 2 in paper
corr_MachPsyc  <-  0.57
corr_MachNarc  <-  0.47
corr_PsycNarc  <-  0.38

corr_df <- MASS::mvrnorm(
  n = n_obs,
  mu = c(0.00613, 0, 0, 0),  # average is just some random small number that's not 0
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

colnames(corr_df) <- c('PSMU', 'Mach', 'Psyc', 'Narc')
psmu_data <- as_tibble(corr_df) |>
  rownames_to_column(var = 'subjID')
write_csv(psmu_data, 'psmu_darktriad.csv')

# m1 <- glm(PSMU ~ Mach + Psyc + Narc, data = psmu_data)
# summary(m1)
# car::vif(m1)
# rstandard(m1, type = 'deviance') |> plot()
# plot(m1, which = 4)
