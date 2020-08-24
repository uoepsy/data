library(tidyverse)

n = 30

df <- tibble(
    stress = runif(n, 3, 10),
    performance = -(1.2 * stress - 15)^2 + rnorm(n, 0, 3)
)
ggplot(df, aes(stress, performance)) +
    geom_point()

write_csv(df, path = 'stress_performance.csv')
