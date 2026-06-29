library(tidyverse)
library(tidylog)
library(sf)

setwd("C:/Users/brenner/Downloads/")

boroughs <- 
  sf::read_sf("nybb_25d/nybb_25d") %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs = 2263) %>% 
  dplyr::transmute(
    geo_id = dplyr::case_when(
      BoroName == "Bronx" ~ "36005", 
      BoroName == "Brooklyn" ~ "36047", 
      BoroName == "Manhattan" ~ "36061", 
      BoroName == "Queens" ~ "36081", 
      BoroName == "Staten Island" ~ "36085"
    ),
    total_area = sf::st_area(geometry),
    geometry
  )

boroughs %>% 
  sf::write_sf(
    "boroughs.gpkg", 
    layer = "boroughs", 
    append = FALSE
  )
