library(tidyverse)

set.seed(1)

# 765 + 2.995
# 6.639876 + 1.096944

n <- 100
df <- tibble(
    x = runif(n, 0, 1.3) %>% round(2),
    y = exp(6.339876 + 1.056944 * x + rnorm(n, 0, 0.15)) %>% round(2)
)
df <- df %>%
    mutate( x = x / 1.3 ) %>%
    mutate( x = 0 + (200 - 0) * x )

ggplot(df, aes(x, y)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = lm, se = FALSE)

df[99, ] <- tibble(x = 200, y = 508)
df[100, ] <- tibble(x = 53, y = 1532)



par(mfrow = c(2,2))
m1 = lm(y ~ x, data = df)
plot(m1)

tmp = df[-99, ]
m1 = lm(y ~ x, data = tmp)
plot(m1)
shapiro.test(resid(m1))

m2 = lm(log(y) ~ x, data = tmp)
plot(m2)
shapiro.test(resid(m2))

tmp = tmp[-99, ]
m3 = lm(log(y) ~ x, data = tmp)
plot(m3)
shapiro.test(resid(m3))



names(df) <- c('angle_degrees', 'rt_ms')


df <- slice_sample(df, n = nrow(df), replace = FALSE)

write_csv(df, path = '~/R/uoepsy/data/mental_rotation.csv')
