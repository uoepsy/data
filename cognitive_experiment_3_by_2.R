library(tidyverse)

cog <- read_csv("https://uoepsy.github.io/data/cognitive_experiment.csv")
cog

newcog <- cog %>%
    filter(Task != 2) %>%
    mutate(Task = ifelse(Task == 3, 2, 1))

write_csv(newcog, 'cognitive_experiment_3_by_2.csv')
