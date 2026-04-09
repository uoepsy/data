library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # "Source" to run
source('simfns.R')

half_ppts <- 18

journal <- sim_int_1grp_1pred(seed=2, b0 = 1, b1=.1, N = half_ppts*7, n_groups = half_ppts) |>
  mutate(
    condition = 'journal',
    ppt_id = paste0('j', g)
  ) |>
  select(-y_bin, -g)

meditate <- sim_int_1grp_1pred(seed=4, b0 = 1, b1=.5, N = half_ppts*7, n_groups = half_ppts) |>
  mutate(
    condition = 'meditate',
    ppt_id = paste0('m', g)
  ) |>
  select(-y_bin, -g)

lifesat_week <- bind_rows(journal, meditate) |>
  rename(day = x1) |>
  mutate(lifesat = as.numeric(datawizard::rescale(y, to = c(15, 75)))) |>
  select(-y)

write_csv(lifesat_week, 'lifesat_mindful.csv')