require(tidyverse)
    
riverview %>% mutate(
    wellbeing = round(income*0.8),
    outdoor_time = seniority,
    social_int = education,
    location = fct_recode(factor(party),City="Independent", Suburb="Democrat",Rural="Republican"),
    routine = fct_recode(factor(gender), "Routine" = "male","No Routine"="female")
) %>% select(wellbeing, outdoor_time, social_int, location, routine) -> mwdata