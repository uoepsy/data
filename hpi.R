# HappyPlanet data

library(tidyverse)
library(readxl)
library(janitor)

df = read_excel('~/R/uoepsy/data/hpi-data-2016.xlsx', 
                 sheet = 'Complete HPI data', range = 'B6:O146')
head(df)

df = clean_names(df)

names(df)

names(df) <- names(df) %>%
    str_replace('gdp_capita_ppp', 'gdp_per_capita') %>%
    str_replace('average_wellbeing_0_10', 'wellbeing') %>%
    str_replace('average_life_expectancy', 'life_expectancy') %>%
    str_replace('footprint_gha_capita', 'footprint')

df <- df %>%
    relocate(hpi_rank, .after = last_col())

head(df)

write_csv(df, '~/R/uoepsy/data/hpi.csv')
