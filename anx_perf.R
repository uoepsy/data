library(tidyverse)

set.seed(1)

n <- 100
df <- tibble(
    x = runif(n, 0, 1) %>% round(2),
    y = exp(2.4 + 1.82 * x + rnorm(n, 0, 0.2)) %>% round(2)
) %>%
    mutate(
        x = 10 + x * (30 - 10),
        x = x %>% round(2),
    )

df[99, ] <- tibble(x = 33, y = 8)
df[100, ] <- tibble(x = 15, y = 51)
df <- slice_sample(df, n = nrow(df), replace = FALSE)

ggplot(df, aes(x, y)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE)

par(mfrow = c(2,2))
plot(lm(y ~ x, data = df))
plot(lm(log(y) ~ x, data = df))

names(df) <- c('anxiety', 'performance')

# par(mfrow = c(1,1))
# hist(df$y, breaks = 30)
# hist(log(df$y), breaks = 30)

write_csv(df, path = '~/R/uoepsy/data/anx_perf.csv')
