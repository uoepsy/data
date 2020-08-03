require(tidyverse)

tibble(
    area = c("Developmental",
             "Differential",
             "Cognitive Neuroscience",
             "Language",
             "Social"),
    n=c(10,20,24,9,11)
) %>% uncount(n) %>% sample_n(size=n()) %>% 
    mutate(
        participant = paste0("respondent_",1:n())
    ) %>% select(participant, area) -> psych_disciplines 

write.csv(psych_disciplines, "psych_survey.csv", row.names = F)

set.seed(3853)
psych_disciplines %>%
    mutate(
        happiness = sample(1:5, size = n(), replace=TRUE, prob = c(.1,.2,.3,.3,.1)),
        job_sat = sample(1:5, size = n(), replace=TRUE, prob = c(.1,.1,.1,.3,.4))
    ) %>% select(-area) -> psych_survey
write.csv(psych_disciplines2,"psych_survey2.csv",row.names=FALSE)
