library(tidyverse)

set.seed(1)

mu    <- 266
sigma <- 16
n     <- 49863

df <- tibble(
    id = 1:n,
    gest_period = rnorm(n, mu, sigma)
)

write_csv(df, file = '../../data/pregnancies.csv')
