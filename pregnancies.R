library(tidyverse)

mu    <- 266
sigma <- 16
n     <- 121650

df <- tibble(
    id = 1:n,
    length_preg = rnorm(n, mu, sigma)
)

write_csv(df, file = '../../data/pregnancies.csv')