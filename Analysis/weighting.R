library(tidyverse)
library(tidylog)
library(sf)

setwd("C:/Users/brenner/Downloads")


# Import Data -------------------------------------------------------------
#Lots
lots_2000 <- 
  sf::read_sf("C:/Users/brenner/Downloads/pluto2005.gpkg") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(
    bbl = as.character(bbl),
    block,
    year = vintage - 5,
    area_lot_total = total_area
  ) %>% 
  dplyr::group_by(block) %>% 
  dplyr::mutate(
    area_block_res = sum(area_lot_total, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    lot_shr_block = area_lot_total / area_block_res
  )

lots_2010 <- sf::read_sf("C:/Users/brenner/Downloads/pluto2015.gpkg") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(
    bbl = as.character(bbl),
    block,
    year = vintage - 5,
    area_lot_total = total_area
  ) %>% 
  dplyr::group_by(block) %>% 
  dplyr::mutate(
    area_block_res = sum(area_lot_total, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    lot_shr_block = area_lot_total / area_block_res
  )

lots_2020 <- sf::read_sf("C:/Users/brenner/Downloads/pluto2025.gpkg") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::transmute(
    bbl = as.character(bbl),
    block,
    year = vintage - 5,
    area_lot_total = total_area
  ) %>% 
  dplyr::group_by(block) %>% 
  dplyr::mutate(
    area_block_res = sum(area_lot_total, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    lot_shr_block = area_lot_total / area_block_res
  )

#Demographics
census <- 
  readr::read_csv("https://drive.google.com/uc?export=download&id=1Ssqc1vEtSIlhBt4FI6ty7mo_0cfdMQ1H") %>% 
  dplyr::filter(geo_level == "block") %>% 
  dplyr::transmute(
    block = as.character(geo_id),
    year,
    pop,
    hhs
  )

#Flooding 
###Needed to download for some reason
flooding <- 
  readr::read_csv("intersection-results_2026-06-29.csv") %>% 
  dplyr::filter(level == "lot") %>% 
  dplyr::transmute(
    bbl = as.character(geo_id),
    vintage,
    flood,
    area_lot_flood = flood_area,
    flood_share,
    flag01 = flood_share >= 0.01,
    flag10 = flood_share >= 0.1,
    flag50 = flood_share >= 0.5,
    flag90 = flood_share >= 0.9,
    flag99 = flood_share >= 0.99
  )


# Combining ---------------------------------------------------------------
results_2000 <- 
  lots_2000 %>% 
  tidylog::filter(
    #removed 681 rows (<1%), 745,036 rows remaining
    !is.na(block)
  ) %>% 
  tidylog::left_join(
    #rows only in lots_2000        0
    #rows only in census    (  8,593) <- 0 or low/error population blocks
    #matched rows            745,036
    census %>% 
      dplyr::filter(year == 2000),
    by = "block"
  ) %>% 
  tidylog::left_join(
    #rows only in lots_2000    27,091
    #rows only in flooding   (      0)
    #matched rows             717,945
    flooding %>% 
      dplyr::filter(
        vintage == 2005,
        flood == 2020
      ),
    by = "bbl"
  ) %>% 
  dplyr::transmute(
    bbl, 
    block,
    year_census = year.x,
    year_pluto = year.x + 5,
    year_flood = 2020,
    area_lot_total, 
    area_lot_flood = tidyr::replace_na(area_lot_flood, 0),
    flood_shr_lot = tidyr::replace_na(flood_share, 0),
    area_block_res,
    lot_shr_block,
    pop_block = pop,
    pop_lot = pop * lot_shr_block,
    hhs_block = hhs,
    hhs_lot = hhs * lot_shr_block,
    flag01 = tidyr::replace_na(flag01, 0),
    flag10 = tidyr::replace_na(flag10, 0),
    flag50 = tidyr::replace_na(flag50, 0),
    flag90 = tidyr::replace_na(flag90, 0),
    flag99 = tidyr::replace_na(flag99, 0)
  )

results_2010 <- 
  lots_2010 %>% 
  tidylog::filter(
    #removed 118 rows (<1%), 760,105 rows remaining
    !is.na(block)
  ) %>% 
  tidylog::left_join(
    #rows only in lots_2000        0
    #rows only in census    ( 10,134) <- 0 or low/error population blocks
    #matched rows            760,105
    census %>% 
      dplyr::filter(year == 2010),
    by = "block"
  ) %>% 
  tidylog::left_join(
    #rows only in lots_2000    26,161
    #rows only in flooding   (      0)
    #matched rows             733,944
    flooding %>% 
      dplyr::filter(
        vintage == 2015,
        flood == 2020
      ),
    by = "bbl"
  ) %>% 
  dplyr::transmute(
    bbl, 
    block,
    year_census = year.x,
    year_pluto = year.x + 5,
    year_flood = 2020,
    area_lot_total, 
    area_lot_flood = tidyr::replace_na(area_lot_flood, 0),
    flood_shr_lot = tidyr::replace_na(flood_share, 0),
    area_block_res,
    lot_shr_block,
    pop_block = pop,
    pop_lot = pop * lot_shr_block,
    hhs_block = hhs,
    hhs_lot = hhs * lot_shr_block,
    flag01 = tidyr::replace_na(flag01, 0),
    flag10 = tidyr::replace_na(flag10, 0),
    flag50 = tidyr::replace_na(flag50, 0),
    flag90 = tidyr::replace_na(flag90, 0),
    flag99 = tidyr::replace_na(flag99, 0)
  )

results_2020_2020 <- 
  lots_2020 %>% 
  tidylog::filter(
    #removed 2 rows (<1%), 767,216 rows remaining
    !is.na(block)
  ) %>% 
  tidylog::left_join(
    #rows only in lots_2000        0
    #rows only in census    (  8,787) <- 0 or low/error population blocks
    #matched rows            767,216
    census %>% 
      dplyr::filter(year == 2020),
    by = "block"
  ) %>% 
  tidylog::left_join(
    #rows only in lots_2000    27,135
    #rows only in flooding   (      0)
    #matched rows             740,081
    flooding %>% 
      dplyr::filter(
        vintage == 2025,
        flood == 2020
      ),
    by = "bbl"
  ) %>% 
  dplyr::transmute(
    bbl, 
    block,
    year_census = year.x,
    year_pluto = year.x + 5,
    year_flood = 2020,
    area_lot_total, 
    area_lot_flood = tidyr::replace_na(area_lot_flood, 0),
    flood_shr_lot = tidyr::replace_na(flood_share, 0),
    area_block_res,
    lot_shr_block,
    pop_block = pop,
    pop_lot = pop * lot_shr_block,
    hhs_block = hhs,
    hhs_lot = hhs * lot_shr_block,
    flag01 = tidyr::replace_na(flag01, 0),
    flag10 = tidyr::replace_na(flag10, 0),
    flag50 = tidyr::replace_na(flag50, 0),
    flag90 = tidyr::replace_na(flag90, 0),
    flag99 = tidyr::replace_na(flag99, 0)
  )

results_2020_2050 <- 
  lots_2020 %>% 
  tidylog::filter(
    #removed 2 rows (<1%), 767,216 rows remaining
    !is.na(block)
  ) %>% 
  tidylog::left_join(
    #rows only in lots_2000        0
    #rows only in census    (  8,787) <- 0 or low/error population blocks
    #matched rows            767,216
    census %>% 
      dplyr::filter(year == 2020),
    by = "block"
  ) %>% 
  tidylog::left_join(
    #rows only in lots_2000    28,601
    #rows only in flooding   (      0)
    #matched rows             738,615
    flooding %>% 
      dplyr::filter(
        vintage == 2025,
        flood == 2050
      ),
    by = "bbl"
  ) %>% 
  dplyr::transmute(
    bbl, 
    block,
    year_census = year.x,
    year_pluto = year.x + 5,
    year_flood = 2050,
    area_lot_total, 
    area_lot_flood = tidyr::replace_na(area_lot_flood, 0),
    flood_shr_lot = tidyr::replace_na(flood_share, 0),
    area_block_res,
    lot_shr_block,
    pop_block = pop,
    pop_lot = pop * lot_shr_block,
    hhs_block = hhs,
    hhs_lot = hhs * lot_shr_block,
    flag01 = tidyr::replace_na(flag01, 0),
    flag10 = tidyr::replace_na(flag10, 0),
    flag50 = tidyr::replace_na(flag50, 0),
    flag90 = tidyr::replace_na(flag90, 0),
    flag99 = tidyr::replace_na(flag99, 0)
  )

results_2020_2080 <- 
  lots_2020 %>% 
  tidylog::filter(
    #removed 2 rows (<1%), 767,216 rows remaining
    !is.na(block)
  ) %>% 
  tidylog::left_join(
    #rows only in lots_2000        0
    #rows only in census    (  8,787) <- 0 or low/error population blocks
    #matched rows            767,216
    census %>% 
      dplyr::filter(year == 2020),
    by = "block"
  ) %>% 
  tidylog::left_join(
    #rows only in lots_2000    31,675
    #rows only in flooding   (      0)
    #matched rows             735,541
    flooding %>% 
      dplyr::filter(
        vintage == 2025,
        flood == 2080
      ),
    by = "bbl"
  ) %>% 
  dplyr::transmute(
    bbl, 
    block,
    year_census = year.x,
    year_pluto = year.x + 5,
    year_flood = 2080,
    area_lot_total, 
    area_lot_flood = tidyr::replace_na(area_lot_flood, 0),
    flood_shr_lot = tidyr::replace_na(flood_share, 0),
    area_block_res,
    lot_shr_block,
    pop_block = pop,
    pop_lot = pop * lot_shr_block,
    hhs_block = hhs,
    hhs_lot = hhs * lot_shr_block,
    flag01 = tidyr::replace_na(flag01, 0),
    flag10 = tidyr::replace_na(flag10, 0),
    flag50 = tidyr::replace_na(flag50, 0),
    flag90 = tidyr::replace_na(flag90, 0),
    flag99 = tidyr::replace_na(flag99, 0)
  )

results_2000 %>% 
  dplyr::bind_rows(results_2010) %>% 
  dplyr::bind_rows(results_2020_2020) %>% 
  dplyr::bind_rows(results_2020_2050) %>% 
  dplyr::bind_rows(results_2020_2080) %>% 
  readr::write_csv(stringr::str_glue("weighting-results_{Sys.Date()}.csv"))




