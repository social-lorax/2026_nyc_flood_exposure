library(tidyverse)
library(tidylog)
library(sf)


# Data --------------------------------------------------------------------
#Floodplains
fp_2020 <- sf::read_sf("https://drive.google.com/uc?export=download&id=11cbV3f1CYyWehSVKRe09xn3iHQJb0PW9")
fp_2050 <- sf::read_sf("https://drive.google.com/uc?export=download&id=1osoQVFWMD29oKNKUIM5HbPR9ByKxWVhV")
fp_2080 <- sf::read_sf("https://drive.google.com/uc?export=download&id=1s4nTzCIdeaJSTNo69a2R-tAliDRbyUwz")

#Tracts
tracts_2000 <- sf::read_sf("https://drive.google.com/uc?export=download&id=1KzstGi-iZd_xBfr3gGYgdGzQzR91D7LL")
tracts_2010 <- sf::read_sf("https://drive.google.com/uc?export=download&id=1tJp-0EpXE72tzP1XiksZwiiNhqsWFmC3")
tracts_2020 <- sf::read_sf("https://drive.google.com/uc?export=download&id=1xrIOnO0jND8Bw5oPk6NMdojA9p5fmks_")

#Blocks
blocks_2000 <- sf::read_sf("https://drive.google.com/uc?export=download&id=1xWYyjuXGjm-SMiCgmxe_bBDTXaAu0CLU")
blocks_2010 <- sf::read_sf("https://drive.google.com/uc?export=download&id=1swP3d0MCsXuvc8t9D__DFzM4rEEnyeog")
blocks_2020 <- sf::read_sf("https://drive.google.com/uc?export=download&id=1kE2tlekfibU7IhkYu8FGYo7mJMt0RELF")

#Lots
###First download from Google Drive and then access from Downloads
lots_2000 <- 
  sf::read_sf("C:/Users/brenner/Downloads/pluto2005.gpkg") %>% 
  dplyr::mutate(bbl = as.character(bbl))
  
lots_2010 <- sf::read_sf("C:/Users/brenner/Downloads/pluto2015.gpkg") %>% 
  dplyr::mutate(bbl = as.character(bbl))

lots_2020 <- sf::read_sf("C:/Users/brenner/Downloads/pluto2025.gpkg") %>% 
  dplyr::mutate(bbl = as.character(bbl))

# First Level Functions ---------------------------------------------------
#Function 1: Clips a borough-level floodplain shapefile to a specific Census tract
clip_tract <- function(flood_shp, tract_shp, tract_id){
  #Select correct floodplain 
  flood_shp_boro <- 
    flood_shp %>% 
    dplyr::filter(geo_id == stringr::str_sub(tract_id, 1, 5))
  #Start with a shapefile of all tracts
  tract_shp %>%
    #Filter to a specific tract
    dplyr::filter(geo_id == tract_id) %>% 
    #Clip the borough-level floodplain shapefile to the tract
    sf::st_intersection(flood_shp_boro) %>%
    #Combine any fragments into a single unit
    sf::st_union(by_feature = TRUE) %>% 
    #Fix for the next geospatial analysis 
    sf::st_make_valid() %>% 
    sf::st_as_sf() 
}

#Function 2: Clips a tract-level floodplain shapefile to a specific Census block
clip_block <- function(flood_shp_tract, block_shp, block_id){
  #Start with a shapefile of all blocks
  block_shp %>% 
    #Filter to a specific block
    dplyr::filter(geo_id == block_id) %>% 
    #Clip the tract-level floodplain shapefile to the block
    sf::st_intersection(flood_shp_tract) %>%
    #Combine any fragments into a single unit
    sf::st_union(by_feature = TRUE) %>% 
    #Fix for the next geospatial analysis 
    sf::st_make_valid() %>% 
    sf::st_as_sf() 
}

#Function 3: Compares all lots in a given Census block to the block-level floodplain shapefile
calc_lots <- function(flood_shp_block, lot_shp, block_id){
  #Start with a shapefile of all lots
  lot_shp %>% 
    #Filter to all of the lots in a specific block
    dplyr::filter(block == block_id) %>% 
    #Clip the block-level floodplain shapefile to each lot in the block
    sf::st_intersection(flood_shp_block) %>%
    #Combine any fragments into a single unit
    sf::st_union(by_feature = TRUE) %>% 
    #Fix for the next geospatial analysis 
    sf::st_make_valid() %>% 
    sf::st_as_sf() %>% 
    #Calculate the area in the floodplain
    dplyr::transmute(
      bbl,
      tract,
      block,
      vintage, 
      total_area = as.double(total_area),
      flooded_area = as.double(sf::st_area(geom)),
      flooded_shr = flooded_area / total_area
    ) %>% 
    #Drop the geometry to save memory
    sf::st_drop_geometry()
}

# Second Level Function ---------------------------------------------------
#Function 4: Runs the full analysis for a specific borough
calc_full <- function(flood_shp, tract_shp, block_shp, lot_shp){
  #Pull all combinations of tract, block, and BBL to set the universe
  universe <- lot_shp %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct(tract, block, bbl)
  #Pull all tracts as a list to iterate over
  tract_ids <- universe %>%
    dplyr::distinct(tract) %>%
    dplyr::pull(tract)
  #Iterate over all tracts, applying first level functions
  purrr::map_dfr(tract_ids, function(tract_id){
    #Clip borough-level floodplain shapefile to the specific Census tract
    flood_shp_tract <- clip_tract(flood_shp, tract_shp, tract_id)
    #If there is no overlap with the floodplain, mark everything zero...
    if (nrow(flood_shp_tract) == 0){
      tract_row <- tract_shp %>% 
        dplyr::filter(geo_id == tract_id) %>% 
        sf::st_drop_geometry() %>% 
        dplyr::transmute(
          level = "tract",
          geo_id, 
          vintage,
          area_total = as.double(total_area),
          flood_area = 0.0,
          flood_share = 0.0
        ) 
      #...else calculate the area in the floodplain
    } else {
      tract_row <- flood_shp_tract %>% 
        dplyr::transmute(
          level = "tract",
          geo_id, 
          vintage,
          area_total = as.double(total_area),
          flood_area = as.double(sf::st_area(geom)),
          flood_share = flood_area / area_total
        ) %>% 
        sf::st_drop_geometry()
    }
    #Pull all blocks from the universe in the tract as a list to iterate over
    block_ids <- universe %>%
      dplyr::filter(tract == tract_id) %>%
      dplyr::distinct(block) %>%
      dplyr::pull(block)
    #Iterate over all blocks, applying first level functions
    block_and_lot_rows <- purrr::map_dfr(block_ids, function(block_id){
      #Clip tract-level floodplain shapefile to the specific Census block
      flood_shp_block <- clip_block(flood_shp_tract, block_shp, block_id)
      #If there is no overlap with the floodplain, mark everything zero...
      if (nrow(flood_shp_block) == 0){
        #Block
        block_row <- block_shp %>% 
          dplyr::filter(geo_id == block_id) %>% 
          sf::st_drop_geometry() %>% 
          dplyr::transmute(
            level = "block",
            geo_id, 
            tract,
            vintage,
            area_total = as.double(total_area),
            flood_area = 0.0,
            flood_share = 0.0
          )
        #Lots in block
        lot_rows <- lot_shp %>%
          dplyr::filter(block == block_id) %>% 
          sf::st_drop_geometry() %>%
          dplyr::transmute(
            level = "lot",
            geo_id = bbl,
            tract,
            block,
            vintage,
            area_total = total_area,
            flood_area = 0.0,
            flood_share = 0.0
          ) 
        #...else calculate the area in the floodplain
      } else {
        #Block
        block_row <- flood_shp_block %>% 
          dplyr::transmute(
            level = "block",
            geo_id, 
            tract,
            vintage,
            area_total = as.double(total_area),
            flood_area = as.double(sf::st_area(geom)),
            flood_share = flood_area / area_total
          ) %>% 
          sf::st_drop_geometry()
        #Compare all lots the block to the block-level floodplain shapefile
        lot_rows <- calc_lots(flood_shp_block, lot_shp, block_id) %>%
          dplyr::transmute(
            level = "lot",
            geo_id = bbl,
            tract,
            block,
            vintage,
            area_total = total_area,
            flood_area = flooded_area,
            flood_share = flooded_shr
          ) 
      }
      #Combine results for all lots with the results for the block
      dplyr::bind_rows(lot_rows, block_row)
    })
    #Combine results for all blocks with the results for the tract
    dplyr::bind_rows(block_and_lot_rows, tract_row)
  }) 
  #purrr::map_dfr() combines results for all tracts
}


# Applying ----------------------------------------------------------------
fp_2020_vin_2000 <- 
  calc_full(
    flood_shp = fp_2020, 
    tract_shp = tracts_2000, 
    block_shp = blocks_2000, 
    lot_shp   = lots_2000
  )

fp_2020_vin_2010 <- 
  calc_full(
    flood_shp = fp_2020, 
    tract_shp = tracts_2010, 
    block_shp = blocks_2010, 
    lot_shp   = lots_2010
  )

fp_2020_vin_2020 <- 
  calc_full(
    flood_shp = fp_2020, 
    tract_shp = tracts_2020, 
    block_shp = blocks_2020, 
    lot_shp   = lots_2020
  )

fp_2050_vin_2020 <- 
  calc_full(
    flood_shp = fp_2050, 
    tract_shp = tracts_2020, 
    block_shp = blocks_2020, 
    lot_shp   = lots_2020
  )

fp_2080_vin_2020 <- 
  calc_full(
    flood_shp = fp_2080, 
    tract_shp = tracts_2020, 
    block_shp = blocks_2020, 
    lot_shp   = lots_2020
  )

