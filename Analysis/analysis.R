library(tidyverse)
library(tidylog)
library(sf)

setwd("C:/Users/brenner/Downloads")


# Import Data -------------------------------------------------------------
results <- readr::read_csv("C:/Users/brenner/Downloads/weighting-results_2026-06-30.csv")

results_city <- results %>% 
  dplyr::summarize(
    .by = c(year_census, year_flood),
    total_pop = sum(pop_lot, na.rm = TRUE),
    flood_pop_01 = sum(pop_lot * flag01, na.rm = TRUE),
    flood_pop_10 = sum(pop_lot * flag10, na.rm = TRUE),
    flood_pop_50 = sum(pop_lot * flag50, na.rm = TRUE),
    flood_pop_90 = sum(pop_lot * flag90, na.rm = TRUE),
    flood_pop_99 = sum(pop_lot * flag99, na.rm = TRUE)
  ) %>% 
  dplyr::filter(year_census %in% c(2010, 2020), year_flood %in% c(2020, 2050))

results_city <- 
  dplyr::tibble(
    group = c("development", "climate"),
    year_start = c(2010, 2020),
    year_end = c(2020, 2050)
  ) %>% 
  dplyr::inner_join(
    results_city %>% 
      dplyr::filter(year_flood == 2020),
    by = c("year_start" = "year_census")
  ) %>% 
  dplyr::transmute(
    group,
    year_start,
    year_end, 
    pop_total_start = total_pop,
    pop_flood_01_start = flood_pop_01,
    pop_flood_50_start = flood_pop_50,
    pop_flood_99_start = flood_pop_99
  ) %>% 
  dplyr::inner_join(
    results_city %>% 
      dplyr::filter(year_census == 2020),
    by = c("year_end" = "year_flood")
  ) %>% 
  dplyr::transmute(
    group,
    year_start,
    year_end,
    pop_flood_01_start,
    pop_flood_01_end = flood_pop_01,
    pop_flood_01_rate = ((pop_flood_01_end / pop_flood_01_start)^(1/(year_end - year_start))) - 1,
    pop_flood_50_start,
    pop_flood_50_end = flood_pop_50,
    pop_flood_50_rate = ((pop_flood_50_end / pop_flood_50_start)^(1/(year_end - year_start))) - 1,
    pop_flood_99_start,
    pop_flood_99_end = flood_pop_99,
    pop_flood_99_rate = ((pop_flood_99_end / pop_flood_99_start)^(1/(year_end - year_start))) - 1
  )




