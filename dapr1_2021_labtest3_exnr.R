# dapr1_2021_labtest3_data.R

library(tidyverse)

tryCatch(params$examnumber, 
         error = function(x) {
             print("ERROR! You MUST write your examnumber where you are asked to") 
         }, 
         finally = "")

if( !is.numeric(params$examnumber) ) {
    stop("Did you write your examnumber where you are asked to? examnumber should only have numbers, no spaces, no letters. Remove any trailing B.")
}

#######

library(tidyverse)

set.seed(params$examnumber)

#######


voting = tibble(
    experimental_group = c('control', 'civic_duty', 'hawthorne', 'self', 'neighbours'),
    prop_voted = c(29.7, 31.5, 32.2, 34.5, 37.8) / 100,
    counts = c(180240, 38200, 38207, 38211, 38209)
)

voting = voting %>%
    rowwise() %>%
    mutate(
        voted = rbinom(1, counts, prop_voted)
    ) %>%
    mutate(
        did_not_vote = counts - voted
    ) %>%
    select(-prop_voted, -counts) %>%
    pivot_longer(c(voted, did_not_vote), 
                 names_to = 'outcome', 
                 values_to = 'n') %>%
    uncount(n) %>%
    mutate(
        subject_id = paste0('MS', 100000 + (1:nrow(.)))
    ) %>%
    select(subject_id, everything())

