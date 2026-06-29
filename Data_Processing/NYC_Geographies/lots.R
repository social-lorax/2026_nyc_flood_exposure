library(tidyverse)
library(tidylog)
library(sf)

setwd("C:/Users/brenner/Downloads/")


# 2005 --------------------------------------------------------------------
pluto_2005 <- 
  sf::read_sf("mappluto_05d/MapPLUTO_05D/Bronx/bxmappluto.shp") %>% 
  dplyr::filter(UnitsRes > 0) %>% 
  dplyr::bind_rows(
    sf::read_sf("mappluto_05d/MapPLUTO_05D/Brooklyn/bkmappluto.shp") %>% 
      dplyr::filter(UnitsRes > 0)
  ) %>%
  dplyr::bind_rows(
    sf::read_sf("mappluto_05d/MapPLUTO_05D/Manhattan/mnmappluto.shp") %>% 
      dplyr::filter(UnitsRes > 0)
  ) %>% 
  dplyr::bind_rows(
    sf::read_sf("mappluto_05d/MapPLUTO_05D/Queens/qnmappluto.shp") %>% 
      dplyr::filter(UnitsRes > 0)
  ) %>% 
  dplyr::bind_rows(
    sf::read_sf("mappluto_05d/MapPLUTO_05D/Staten_Island/simappluto.shp") %>% 
      dplyr::filter(UnitsRes > 0)
  ) %>% 
  dplyr::transmute(
    bbl = BBL, 
    res_units = UnitsRes,
    tract = dplyr::case_when(
      Borough == "BX" ~ stringr::str_c("36005", stringr::str_pad(as.double(CT2000) * 100, width = 6, pad = "0", side = "left")),
      Borough == "BK" ~ stringr::str_c("36047", stringr::str_pad(as.double(CT2000) * 100, width = 6, pad = "0", side = "left")),
      Borough == "MN" ~ stringr::str_c("36061", stringr::str_pad(as.double(CT2000) * 100, width = 6, pad = "0", side = "left")),
      Borough == "QN" ~ stringr::str_c("36081", stringr::str_pad(as.double(CT2000) * 100, width = 6, pad = "0", side = "left")),
      Borough == "SI" ~ stringr::str_c("36085", stringr::str_pad(as.double(CT2000) * 100, width = 6, pad = "0", side = "left"))
    ),
    block = stringr::str_c(tract, CB2000),
    vintage = 2005, 
    total_area = sf::st_area(geometry),
    geometry
  )

pluto_2005 %>% 
  sf::write_sf(
    "pluto2005.gpkg", 
    layer = "pluto2005", 
    append = FALSE
  )


# 2015 --------------------------------------------------------------------
pluto_2015 <- 
  sf::read_sf("mappluto_15v1/Bronx/BXMapPLUTO.shp") %>% 
  dplyr::filter(UnitsRes > 0) %>% 
  dplyr::bind_rows(
    sf::read_sf("mappluto_15v1/Brooklyn/BKMapPLUTO.shp") %>% 
      dplyr::filter(UnitsRes > 0)
  ) %>%
  dplyr::bind_rows(
    sf::read_sf("mappluto_15v1/Manhattan/MNMapPLUTO.shp") %>% 
      dplyr::filter(UnitsRes > 0)
  ) %>% 
  dplyr::bind_rows(
    sf::read_sf("mappluto_15v1/Queens/QNMapPLUTO.shp") %>% 
      dplyr::filter(UnitsRes > 0)
  ) %>% 
  dplyr::bind_rows(
    sf::read_sf("mappluto_15v1/Staten_Island/SIMapPLUTO.shp") %>% 
      dplyr::filter(UnitsRes > 0)
  ) %>% 
  dplyr::transmute(
    bbl = BBL, 
    res_units = UnitsRes,
    tract = dplyr::case_when(
      Borough == "BX" ~ stringr::str_c("36005", stringr::str_pad(as.double(CT2010) * 100, width = 6, pad = "0", side = "left")),
      Borough == "BK" ~ stringr::str_c("36047", stringr::str_pad(as.double(CT2010) * 100, width = 6, pad = "0", side = "left")),
      Borough == "MN" ~ stringr::str_c("36061", stringr::str_pad(as.double(CT2010) * 100, width = 6, pad = "0", side = "left")),
      Borough == "QN" ~ stringr::str_c("36081", stringr::str_pad(as.double(CT2010) * 100, width = 6, pad = "0", side = "left")),
      Borough == "SI" ~ stringr::str_c("36085", stringr::str_pad(as.double(CT2010) * 100, width = 6, pad = "0", side = "left"))
    ),
    block = stringr::str_c(tract, CB2010),
    vintage = 2015, 
    total_area = sf::st_area(geometry),
    geometry
  )

pluto_2015 %>% 
  sf::write_sf(
    "pluto2015.gpkg", 
    layer = "pluto2015", 
    append = FALSE
  )

# 2025 --------------------------------------------------------------------
pluto_2025 <- 
  sf::read_sf("nyc_mappluto_25v4_arc_shp/MapPLUTO.shp") %>%
  dplyr::filter(UnitsRes > 0) %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(crs = 2263) %>% 
  dplyr::transmute(
    bbl = BBL,
    res_units = UnitsRes,
    tract = dplyr::case_when(
      Borough == "BX" ~ stringr::str_c("36005", stringr::str_sub(BCT2020, 2, -1)),
      Borough == "BK" ~ stringr::str_c("36047", stringr::str_sub(BCT2020, 2, -1)), 
      Borough == "MN" ~ stringr::str_c("36061", stringr::str_sub(BCT2020, 2, -1)), 
      Borough == "QN" ~ stringr::str_c("36081", stringr::str_sub(BCT2020, 2, -1)), 
      Borough == "SI" ~ stringr::str_c("36085", stringr::str_sub(BCT2020, 2, -1))
    ),
    block = dplyr::case_when(
      Borough == "BX" ~ stringr::str_c("36005", stringr::str_sub(BCTCB2020, 2, -1)),
      Borough == "BK" ~ stringr::str_c("36047", stringr::str_sub(BCTCB2020, 2, -1)), 
      Borough == "MN" ~ stringr::str_c("36061", stringr::str_sub(BCTCB2020, 2, -1)), 
      Borough == "QN" ~ stringr::str_c("36081", stringr::str_sub(BCTCB2020, 2, -1)), 
      Borough == "SI" ~ stringr::str_c("36085", stringr::str_sub(BCTCB2020, 2, -1))
    ),
    vintage = 2025,
    total_area = sf::st_area(geometry),
    geometry
  )

pluto_2025 %>% 
  sf::write_sf(
    "pluto2025.gpkg", 
    layer = "pluto2025", 
    append = FALSE
  )

