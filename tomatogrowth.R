library(tidyverse)
set.seed(986)
tibble(
    height = c(rnorm(500,115,10),rnorm(500,116.5,10)),
    group = rep(c("control","treatment"),each=500)
) %>%
    write.csv("tomatogrowth.csv",row.names=FALSE)