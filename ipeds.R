library(tidyverse)

df = read_csv("../../data/dataset-ipeds-2012-subset2.csv")

summary(df)
tail(df)
dim(df)

head(order(df$gradratew, decreasing = TRUE))
df[c(854, 1136), c("gradrate", "gradratem")] <- NA

df = add_row(df, .after = 51, 
             type = 2, region = 4, gradrate = 41, gradratem = NA, gradratew = NA)

summary(df)
dim(df)

write_csv(df, "../../data/ipeds-2012.csv")
