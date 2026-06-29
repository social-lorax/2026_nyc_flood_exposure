library(tidyverse)
library(tidylog)
library(sf)

setwd("C:/Users/brenner/Downloads/")


# Boroughs ----------------------------------------------------------------
boroughs <- sf::read_sf("https://drive.google.com/uc?export=download&id=1gI1inptvJqL8fPM7JFxUBVopw_B9ETjl")


# Floodplains -------------------------------------------------------------
fp_2020 <- 
  sf::read_sf("2020/Coastal_Surge_Flooding__2020s_100-Year_Floodplain.shp") %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs = 2263)

fp_2050 <- 
  sf::read_sf("2050/Coastal_Surge_Flooding__2050s_100-Year_Floodplain.shp") %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs = 2263)

fp_2080 <- 
  sf::read_sf("2080/Coastal_Surge_Flooding__2080s_100-Year_Floodplain.shp") %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs = 2263)


# Intersections -----------------------------------------------------------
#2020
boroughs %>% 
  sf::st_intersection(fp_2020) %>% 
  dplyr::summarise(
    .by = geo_id,
    geometry = sf::st_union(geom)
  ) %>%
  sf::st_make_valid() %>% 
  sf::write_sf(
    "flood2020.gpkg", 
    layer = "flood2020", 
    append = FALSE
  )

#2050
boroughs %>% 
  sf::st_intersection(fp_2050) %>% 
  dplyr::summarise(
    .by = geo_id,
    geometry = sf::st_union(geom)
  ) %>%
  sf::st_make_valid() %>% 
  sf::write_sf(
    "flood2050.gpkg", 
    layer = "flood2050", 
    append = FALSE
  )

#2080
boroughs %>% 
  sf::st_intersection(fp_2080) %>% 
  dplyr::summarise(
    .by = geo_id,
    geometry = sf::st_union(geom)
  ) %>%
  sf::st_make_valid() %>% 
  sf::write_sf(
    "flood2080.gpkg", 
    layer = "flood2080", 
    append = FALSE
  )