library(tidyverse)

set.seed(2763)
tibble(
    participant = paste0("ppt_",1:120),
    iq = round(rnorm(120,100,15)),
    age = round(runif(120, 20, 50)),
    test1 = round(rnorm(120,50,7)),
    test2 = round(rnorm(120,50,14)),
) -> wechsler
wechsler$age[sample(1:nrow(wechsler),size=1)]<-71

write.csv(wechsler, "wechsler.csv", row.names = FALSE)