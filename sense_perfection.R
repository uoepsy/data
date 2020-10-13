library(tidyverse)
library(kableExtra)
set.seed(093)
perfection <- read_csv('https://uoepsy.github.io/data/perfection.csv') %>%
    mutate(
        time = ifelse(type=="Colour",time+abs(round(rnorm(20,0,1),2)),time+round(rnorm(20,0,1),2))
    )
tibble(
    time = round(perfection$time[perfection$type=="Standard"]+rnorm(20,0.7,1),2),
    type = "Sound"
) %>% bind_rows(perfection, .) -> perfection

kable(head(perfection), align='c') %>% kable_styling(full_width = FALSE)
sample_n(perfection, n()) %>% mutate(student_id = 1:n()) -> perfection
write.csv(perfection, "sense_perfection.csv", row.names = FALSE)