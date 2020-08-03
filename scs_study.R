require(tidyverse)

set.seed(9863)

# pslab5 is from old rms2 labs
read_csv("~/Desktop/pslab5.csv") %>% mutate(
  zn = ageyint,
  scs = round(BMI+10),
  dass = round(45+(-ZFEV1*15))
) %>% filter(zn>36) %>%
  mutate(
    zo = rnorm(n(),0,1),
    za = scale(rnorm(n(),0,1)+(dass/50))[,1],
    zn = scale(zn)[,1],
    ze = scale(rnorm(n(),0,1)+(dass/100)+za)[,1],
    zc = rnorm(n(),0,1)
  ) %>% select(zo,zc,ze,za,zn, scs, dass) -> scs_study
summary(scs_study)
scs_study$scs[sample(scs_study$scs, 2)]<-c(50,54)

write.csv(scs_study, "scs_study.csv", row.names = FALSE)