library(tidyverse)

set.seed(1)

mu    <- 266
sigma <- 16
n     <- 49863

df <- tibble(
    id = 1:n,
    length_preg = rnorm(n, mu, sigma)
)

write_csv(df, file = '../../data/pregnancies.csv')