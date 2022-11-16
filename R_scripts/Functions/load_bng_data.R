## load_bng_data.R
## ===============
##
## Authors: Mattia Mancini, Rebecca Collins
## Created: 10-Nov-2022
## ----------------------------------------
##
## DESCRIPTION:
## Function called in the calc_bio_offset_greedy_ordered.R script to select 
## offset locations based on a set of policy options.
## Load the base land cover map and the urbanisation data from F. Eigenbrod's
## model of urban sprawl. Merge them together and return a spatial dataframe
## containing baseline and urbanisation landuses.
## ----
## Arguments:
##  1 - base_lcm: full path of the baseline land cover map
##  2 - new_urban_lcm: full path of the urbanisation scenario
## ==========================================================================
load_housing_locations <- function(base_lcm, new_urban_lcm){
  
  city_base <- read.csv(base_lcm) %>% 
    dplyr::rename(farm_ha_base = farm_ha, 
                  wood_ha_base = wood_ha, 
                  sng_ha_base = sng_ha, 
                  urban_ha_base = urban_ha, 
                  water_ha_base = water_ha)
  
  city_scenario <- read.csv(new_urban_lcm)%>% 
    dplyr::rename(farm_ha_scenario = farm_ha, 
                  wood_ha_scenario = wood_ha, 
                  sng_ha_scenario = sng_ha, 
                  urban_ha_scenario = urban_ha, 
                  water_ha_scenario = water_ha)
  
  city_spread <- dplyr::full_join(city_base, city_scenario, by = "new2kid") %>% 
    mutate_if(is.numeric, round, digits=4) %>% # round to 4 as base cover is 4 dp
    dplyr::mutate(area_new_builds = ifelse(urban_ha_scenario-urban_ha_base <= 0, 0, urban_ha_scenario-urban_ha_base), 
                  farmland_area = farm_ha_scenario, 
                  percent_frm = farm_ha_scenario / 400, 
                  percent_grs = sng_ha_scenario / 400, 
                  percent_wod = wood_ha_scenario / 400) 
  
  city_spread <- merge(seer_2km, city_spread, by = 'new2kid')
  return(city_spread)
}

load_seer_grid <- function(seer_path){
  seer_2km <- st_read(paste0(seer_path, "SEER_net2km.shp"))[, "new2kid"]
  
  # filter to England 
  conn <- dbConnect(Postgres(), 
                    dbname = "nev",
                    host = "localhost",
                    port = 5432,
                    user="postgres",
                    password="postgres")
  
  df <- dbGetQuery(conn, "SELECT * FROM regions_keys.key_grid_countries_england")
  cell_id <- df$new2kid
  seer_2km <- seer_2km[seer_2km$new2kid %in% cell_id, 'new2kid']
  
  # Alternative when not on the network 
  Eng_2kid <- read.csv(paste0(seer_path, "England_new_2kid.csv"))
  
  seer_2km <- seer_2km %>% 
    dplyr::filter(new2kid %in% Eng_2kid$new2kid)
  return(seer_2km)
}