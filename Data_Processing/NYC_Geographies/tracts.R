library(tidyverse)
library(tidylog)
library(sf)

setwd("C:/Users/brenner/Downloads/")

# 2000 --------------------------------------------------------------------
tracts_2000 <- 
  sf::read_sf("nyct2000_21b/nyct2000_21b/nyct2000.shp") %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs = 2263) %>% 
  dplyr::transmute(
    geo_id = dplyr::case_when(
      BoroName == "Bronx"         ~ stringr::str_c("36005", CT2000),
      BoroName == "Brooklyn"      ~ stringr::str_c("36047", CT2000), 
      BoroName == "Manhattan"     ~ stringr::str_c("36061", CT2000), 
      BoroName == "Queens"        ~ stringr::str_c("36081", CT2000), 
      BoroName == "Staten Island" ~ stringr::str_c("36085", CT2000)
    ),
    vintage = 2000,
    total_area = sf::st_area(geometry),
    geometry
  )

tracts_2000 %>% 
  sf::write_sf(
    "tracts2000.gpkg", 
    layer = "tracts2000", 
    append = FALSE
  )


# 2010 --------------------------------------------------------------------
tracts_2010 <- 
  sf::read_sf("nyct2010_26b/nyct2010_26b/nyct2010.shp") %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs = 2263) %>% 
  dplyr::transmute(
    geo_id = dplyr::case_when(
      BoroName == "Bronx"         ~ stringr::str_c("36005", CT2010),
      BoroName == "Brooklyn"      ~ stringr::str_c("36047", CT2010), 
      BoroName == "Manhattan"     ~ stringr::str_c("36061", CT2010), 
      BoroName == "Queens"        ~ stringr::str_c("36081", CT2010), 
      BoroName == "Staten Island" ~ stringr::str_c("36085", CT2010)
    ),
    vintage = 2010,
    total_area = sf::st_area(geometry),
    geometry
  )

tracts_2010 %>% 
  sf::write_sf(
    "tracts2010.gpkg", 
    layer = "tracts2010", 
    append = FALSE
  )


# 2020 --------------------------------------------------------------------
tracts_2020 <- 
  sf::read_sf("nyct2020_26b/nyct2020_26b/nyct2020.shp") %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs = 2263) %>% 
  dplyr::transmute(
    geo_id = GEOID,
    vintage = 2020,
    total_area = sf::st_area(geometry),
    geometry
  )

tracts_2020 %>% 
  sf::write_sf(
    "tracts2020.gpkg", 
    layer = "tracts2020", 
    append = FALSE
  )

