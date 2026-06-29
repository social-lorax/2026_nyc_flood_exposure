library(tidyverse)
library(tidylog)
library(sf)

setwd("C:/Users/brenner/Downloads/")


# 2000 --------------------------------------------------------------------
blocks_2000 <- 
  sf::read_sf("nycb2000_21b/nycb2000_21b/nycb2000.shp") %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs = 2263) %>% 
  dplyr::transmute(
    geo_id = dplyr::case_when(
      BoroName == "Bronx"         ~ stringr::str_c("36005", CT2000, CB2000),
      BoroName == "Brooklyn"      ~ stringr::str_c("36047", CT2000, CB2000), 
      BoroName == "Manhattan"     ~ stringr::str_c("36061", CT2000, CB2000), 
      BoroName == "Queens"        ~ stringr::str_c("36081", CT2000, CB2000), 
      BoroName == "Staten Island" ~ stringr::str_c("36085", CT2000, CB2000)
    ),
    tract = dplyr::case_when(
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

blocks_2000 %>% 
  sf::write_sf(
    "blocks2000.gpkg", 
    layer = "blocks2000", 
    append = FALSE
  )

# 2010 --------------------------------------------------------------------
blocks_2010 <- 
  sf::read_sf("nycb2010_26b/nycb2010_26b") %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs = 2263) %>% 
  dplyr::transmute(
    geo_id = dplyr::case_when(
      BoroName == "Bronx"         ~ stringr::str_c("36005", CT2010, CB2010),
      BoroName == "Brooklyn"      ~ stringr::str_c("36047", CT2010, CB2010), 
      BoroName == "Manhattan"     ~ stringr::str_c("36061", CT2010, CB2010), 
      BoroName == "Queens"        ~ stringr::str_c("36081", CT2010, CB2010), 
      BoroName == "Staten Island" ~ stringr::str_c("36085", CT2010, CB2010)
    ),
    tract = dplyr::case_when(
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

blocks_2010 %>% 
  sf::write_sf(
    "blocks2010.gpkg", 
    layer = "blocks2010", 
    append = FALSE
  )


# 2020 --------------------------------------------------------------------
blocks_2020 <- 
  sf::read_sf("nycb2020_26b/nycb2020_26b/nycb2020.shp") %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs = 2263) %>% 
  dplyr::transmute(
    geo_id = GEOID,
    tract = stringr::str_sub(GEOID, 1, 11),
    vintage = 2020,
    total_area = sf::st_area(geometry),
    geometry
  )

blocks_2020 %>% 
  sf::write_sf(
    "blocks2020.gpkg", 
    layer = "blocks2020", 
    append = FALSE
  )
