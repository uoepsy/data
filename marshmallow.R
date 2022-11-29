library(tidyverse)
set.seed(nchar("i don't evssen like marshmallows"))

tibble(
  agemonths = rdunif(200, 39, 84),
  visibility = rep(c("hidden","visible"),e=100),
  sweetrating = round(rnorm(200),2),
  lp = 15 -3*(visibility=="visible") - 1.6*sweetrating + .1*agemonths + .2*(visibility=="visible")*agemonths,
  # lp = 15 -4*(visibility=="visible") - 1.6*sweetrating + .15*(agemonths-39) + .1*(visibility=="visible")*(agemonths-39),
  lp2 = -2*(visibility=="hidden") - .1*(agemonths-39) + .07*(visibility=="hidden")*(agemonths-39),
  time = lp + rnorm(200,0,4)
) %>%
  mutate(
    time = round(time,1),
    taken = rbinom(200,1,plogis(scale(lp2))),
    time = ifelse(taken==0,NA,time),
    time = pmin(40,time)
  ) -> df

lm(time ~ sweetrating + agemonths*visibility,df) %>%  summary
#  sjPlot::plot_model(m,type="int")

glm(taken ~ agemonths*visibility,df,family="binomial") -> m
summary(m)

sjPlot::plot_model(m,type="int")


df$sweetrating[sample(1:200,2)] <- NA
df$agemonths[sample(1:200,1)] <- 0
df$taken[sample(1:200,1)] <- -5
df %>% select(-lp,-lp2) %>% write_csv("../../data/marshmallow.csv")
