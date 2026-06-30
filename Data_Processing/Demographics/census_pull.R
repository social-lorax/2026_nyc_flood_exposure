library(tidyverse)
library(tidylog)
library(tidycensus)

setwd("C:/Users/brenner/Downloads")

# Functions ---------------------------------------------------------------
#2000
get_2000 <- 
  function(geo_level){
    tidycensus::get_decennial(
      geography = geo_level,
      state = "36",
      county = c("005", "047", "061", "081", "085"),
      year = 2000,
      sumfile = "sf1",
      variables = c(
        pop = "P001001",
        hhs = "H001001"
      ),
      output = "wide"
    ) %>%
    dplyr::transmute(
      geo_id = GEOID,
      geo_level = geo_level,
      year = 2000,
      pop,
      hhs
    )
}

#2010
get_2010 <- 
  function(geo_level){
    tidycensus::get_decennial(
      geography = geo_level,
      state = "36",
      county = c("005", "047", "061", "081", "085"),
      year = 2010,
      sumfile = "sf1",
      variables = c(
        pop = "P001001",
        hhs = "H001001"
      ),
      output = "wide"
    ) %>%
    dplyr::transmute(
      geo_id = GEOID,
      geo_level = geo_level,
      year = 2010,
      pop,
      hhs
    )
  }

#2020
get_2020 <- 
  function(geo_level){
    tidycensus::get_decennial(
      geography = geo_level,
      state = "36",
      county = c("005", "047", "061", "081", "085"),
      year = 2020,
      sumfile = "pl",
      variables = c(
        pop = "P1_001N",
        hhs = "H1_002N"
      ),
      output = "wide"
    ) %>%
    dplyr::transmute(
      geo_id = GEOID,
      geo_level = geo_level,
      year = 2020,
      pop,
      hhs
    )
  }

get_all <- 
  function(geo_level){
    get_2000(geo_level = geo_level) %>% 
      dplyr::bind_rows(
        get_2010(geo_level = geo_level)
      ) %>% 
      dplyr::bind_rows(
        get_2020(geo_level = geo_level)
      )
  }


# Compilation -------------------------------------------------------------
get_all(geo_level = "county") %>% 
  summarize(
    .by = c("year"),
    geo_id = "3651000",
    geo_level = "city",
    pop = sum(pop),
    hhs = sum(hhs)
  ) %>% 
  dplyr::bind_rows(
    get_all(geo_level = "county") %>% 
      dplyr::mutate(geo_level = "borough")
  ) %>% 
  dplyr::bind_rows(
    get_all(geo_level = "tract")
  ) %>% 
  dplyr::bind_rows(
    get_all(geo_level = "block")
  ) %>% 
  readr::write_csv("decennial_00-10-20.csv")
