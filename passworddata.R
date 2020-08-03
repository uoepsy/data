pwords <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv")[1:500,] %>% 
    select(rank, password, category, value, strength) %>%
    rename(cracked = "value", type="category") %>%
    mutate(strength_cat = ifelse(strength < 5, "weak", 
                                 ifelse(strength < 10, "medium", "strong")),
           strength = ifelse(strength>10, 10, 
                             ifelse(strength<1,1, strength)))

write.csv(pwords, "data/passworddata.csv", row.names=FALSE)