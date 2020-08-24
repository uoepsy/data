library(tidyverse)

set.seed(1)

n <- 30
df <- tibble(
    stress = runif(n, 30, 100) %>% round(1),
    performance = 110 - (0.8 * stress - 120)^2 / 90 + rnorm(n, 0, 3)
)

ggplot(df, aes(stress, performance)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE)

plot(lm(performance ~ stress, data = df), which = 1)

write_csv(df, path = '~/R/uoepsy/data/stress_performance.csv')
