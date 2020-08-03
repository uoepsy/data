library(tidyverse)

starwars2 <- dplyr::starwars %>% 
    select(-c(mass, -skin_color, -films, -vehicles, -starships, -gender, -birth_year))

starwars2[87, ] <- list("Homer Simpson", 180, NA, NA, "Springfield", "unknown")
starwars2[40, ] <- list("Marge Simpson", 170, "Blue", NA, "Springfield", "unknown")
starwars2[2, "species"] <- "Human"
starwars2$height <- as.numeric(starwars2$height)
starwars2 <- starwars2 %>% 
    filter(!is.na(homeworld), !is.na(species))

write.csv(starwars2, "data/starwars2.csv", row.names = FALSE)