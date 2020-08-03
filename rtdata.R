library(tidyverse)
set.seed(934)
rtdat <- tibble(
    ppt = 1:500,
    rt = round(sn::rsn(500,370,200,8),1),
    height = round(rnorm(500,170,7))
)

write.csv(rtdat, "rtdata.csv",row.names=FALSE)