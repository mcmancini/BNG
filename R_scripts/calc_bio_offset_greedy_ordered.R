## calc_bio_offset_equal_projects.R
## ================================
##
## Author: Mattia Mancini and Rebecca Collins 
## Created: 10-Mar-2021
## Last modified: 05-June-2021
## ------------------------------------------
##
## Script to locate compensation areas and to compute costs, biodiversity and 
## welfare trade-offs based on a series of criteria: 
##   1) Local offset - locations for compensation near building sites.
##   2) Offest based on max biodiversity improvements - locations for 
##      compensation in areas with the highest potential biodiversity gains.
##   3) Offset based on max Ecosystem Services - locations for compensation in 
##      areas with the highest potential for ecosystem services gains.
##   4) Offset based on max Ecosystem Servicesequity weighted by recreation
## =============================================================================

## (0) SET-UP
## ==========
rm(list=ls())
library(sf)
library(dplyr)
library(RPostgres)

#update path for different machines 
gitpath <- "C:/Code/BNG/" 
datapath <- "C:/Data/BNG/"

## (1) LOAD THE DATA AND PREPROCESS IT
##     - 1. SEER 2km grid
##     - 2. new housing locations
## ===================================

## 1.1. SEER 2km grid
## ------------------

setwd(paste0(datapath, "Data/SEER_GRID/")) 
seer_2km <- st_read('./SEER_net2km.shp')
seer_2km <- seer_2km[, "new2kid"]

# filter to England 
conn <- dbConnect(Postgres(), 
                  dbname = "NEV",
                  host = "localhost",
                  port = 5432,
                  user="postgres",
                  password="postgres")

df <- dbGetQuery(conn, "SELECT * FROM regions_keys.key_grid_countries_england")
cell_id <- df$new2kid
seer_2km <- seer_2km[seer_2km$new2kid %in% cell_id, 'new2kid']

## 1.2. New housing locations
## --------------------------
setwd(paste0(datapath, "Data/Urban Sprawl - F.Eigenbrod/")) 

city_base <- read.csv(paste0(datapath, "Data/LCM/LCM_2km/lcm_aggr_2000.csv")) %>% 
  dplyr::rename(farm_ha_base = farm_ha, 
                wood_ha_base = wood_ha, 
                sng_ha_base = sng_ha, 
                urban_ha_base = urban_ha, 
                water_ha_base = water_ha)

city_scenario <- read.csv('./urban_sprawl_2031_sprawl.csv')%>% 
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

## (2) OFFSET MECHANISMS
##     - Local compensation: new high biodiversity areas are created near the 
##       housing development sites
## ==========================================================================

## 2.1. Local offset
## -----------------

# we know for each cell the area in m2 converted into new buildings. Is there an
# equal amount of farmland to be converted into high biodiversity land? If so, 
# do the conversion; otherwise, convert all the available farmland left (if any)
# and do the remaining conversion in the nearest cell with available farmland.
# The conversion land is a mix of 50% sng and 50% woodland

## Case 1: there is more farmland than the required land for biodiversity 
## compensation
city_spread$local_offset <- 0

idx <- (city_spread$area_new_builds > 0) & (city_spread$farmland_area >= city_spread$area_new_builds)
city_spread$local_offset[idx] <- city_spread$area_new_builds[idx]
city_spread$farmland_area[idx] <- city_spread$farmland_area[idx] - city_spread$local_offset[idx]

bio_area_perc <- city_spread$local_offset[idx] / (400)
city_spread$percent_frm[idx] <- round(city_spread$percent_frm[idx] - bio_area_perc, 9)
city_spread$percent_grs[idx] <- round(city_spread$percent_grs[idx] + (bio_area_perc / 2), 9)
city_spread$percent_wod[idx] <- round(city_spread$percent_wod[idx] + (bio_area_perc / 2), 9)

## Case 2: There is some farmland available but not enough for biodiversity 
## compensation
idx <- (city_spread$area_new_builds > city_spread$local_offset) &
  (city_spread$farmland_area > 0)

city_spread$local_offset[idx] <- city_spread$farmland_area[idx]
city_spread$farmland_area[idx] <- 0

bio_area_perc <- city_spread$local_offset[idx] / (400)
city_spread$percent_frm[idx] <- 0
city_spread$percent_grs[idx] <- round(city_spread$percent_grs[idx] + (bio_area_perc / 2), 9)
city_spread$percent_wod[idx] <- round(city_spread$percent_wod[idx] + (bio_area_perc / 2), 9)

## Case 3: There is no farmland available for biodiversity compensation
## In this case, select the closest cell with available farmland, if possible in
## the same LPA. I use a radius of 30km just to make sure London is covered:
## some areas in London are central and far from any farmland. Such a large
## radius is only useful to find available areas around London; for all other
## locations, having a large buffer only slows computation, but the results are
## always adjacent or close cells (minimum distance to the cell from a subset
## of cells whose size depends on radius)


idx <- (city_spread$area_new_builds > city_spread$local_offset)

lpa_greater_london <- c("Barking and Dagenham", "Barnet", "Bexley", "Brent",
                        "Bromley", "Camden", "City of London", "Croydon",
                        "Ealing", "Enfield", "Greenwich", "Hackney",
                        "Hammersmith and Fulham", "Haringey", "Harrow",
                        "Havering", "Hillingdon", "Hounslow", "Islington",
                        "Kensington and Chelsea", "Kingston upon Thames",
                        "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge",
                        "Richmond upon Thames", "Southwark", "Sutton",
                        "Tower Hamlets", "Waltham Forest", "Wandsworth",
                        "Westminster")

for (cell in city_spread$new2kid[idx]){
  frm_needed <- city_spread$area_new_builds[city_spread$new2kid == cell] - 
    city_spread$local_offset[city_spread$new2kid == cell]
  self_cell <- city_spread[city_spread$new2kid == cell, ]
  closest_cells <- st_buffer(city_spread[city_spread$new2kid == cell, ], 
                             dist = 30000)
  ind_cells <- st_intersection(seer_2km, closest_cells)[,1]
  closest_cells <- city_spread[city_spread$new2kid %in% ind_cells$new2kid, ]
  # remove Greater London area, if applicable
  is_in_gr_london <- length(which(closest_cells$lad19nm %in% lpa_greater_london))
  if(is_in_gr_london != 0){
    closest_cells_idx <- which(closest_cells$lad19nm %in% lpa_greater_london)
    closest_cells <- closest_cells[-closest_cells_idx, ]
  }
  # is self_cell contained into closest_cells?
  is_contained <- length(which(closest_cells$new2kid %in% cell))
  if (is_contained != 0){
    closest_cells <- closest_cells[-which(closest_cells$new2kid %in% cell), ]
  }
  avail_cells <- closest_cells[(closest_cells$farmland_area > frm_needed), ]
  lpa_avail_cells <- avail_cells[avail_cells$lad19nm %in% self_cell$lad19nm, ]
  if (nrow(lpa_avail_cells) > 0){
    avail_cells <- lpa_avail_cells
  }
  
  avail_cells <- st_centroid(avail_cells)
  self_cell <- st_centroid(self_cell)
  # ind <- which(city_spread$new2kid %in% avail_cells$new2kid)
  # avail_cells <- st_centroid(city_spread[ind, ])
  # self_cell <- st_centroid(city_spread[city_spread$new2kid == cell, ])
  
  dist <- st_distance(avail_cells, self_cell)
  avail_cells <- avail_cells[which(dist == min(dist)), ]
  new_bio_cell <- avail_cells$new2kid[sample(nrow(avail_cells), 1)]
  
  bio_area_perc <- frm_needed / (400)
  ind <- city_spread$new2kid == new_bio_cell
  city_spread$local_offset[ind] <- city_spread$local_offset[ind] + frm_needed
  city_spread$farmland_area[ind] <- city_spread$farmland_area[ind] - frm_needed
  city_spread$percent_frm[ind] <- round(city_spread$percent_frm[ind] - bio_area_perc, 9)
  city_spread$percent_grs[ind] <- round(city_spread$percent_grs[ind] + (bio_area_perc / 2), 9)
  city_spread$percent_wod[ind] <- round(city_spread$percent_wod[ind] + (bio_area_perc / 2), 9)
}

local_bio_offset_ha <- city_spread %>%
  dplyr::mutate(
    wood_ha = wood_ha_scenario + (local_offset/2), 
    sng_ha = sng_ha_scenario + (local_offset/2)) %>% 
  rename(
    farm_ha = farmland_area,
    urban_ha = urban_ha_scenario, 
    water_ha = water_ha_scenario) %>% 
  dplyr::select(
    new2kid, 
    farm_ha,
    wood_ha, 
    sng_ha, 
    urban_ha, 
    water_ha, 
    local_offset) %>% 
  dplyr::rename(offset_area_ha = local_offset)


# check cell areas
local_bio_offset_ha %>% 
  group_by(new2kid) %>% 
  mutate(sum = farm_ha + wood_ha + sng_ha + urban_ha +water_ha) %>% 
  filter(sum != 400) # some small rounding errors 

# check all offset is allocated 
(c(sum(city_spread$local_offset), sum(city_spread$area_new_builds))) 


setwd(paste0(gitpath,"Output/"))
st_write(local_bio_offset_ha, 'local_bio_offset_urban_sprawl.csv')


## 2.2. Offest based on max biodiversity improvements
## --------------------------------------------------
# Load new housing locations
setwd(paste0(datapath,"Data/Urban Sprawl - F.Eigenbrod/"))

city_base <- read.csv(paste0(datapath,"Data/LCM/LCM_2km/lcm_aggr_2000.csv")) %>%
  dplyr::rename(farm_ha_base = farm_ha, 
                wood_ha_base = wood_ha, 
                sng_ha_base = sng_ha, 
                urban_ha_base = urban_ha, 
                water_ha_base = water_ha)

city_scenario <- read.csv('./urban_sprawl_2031_sprawl.csv')%>% 
  dplyr::rename(farm_ha_scenario = farm_ha, 
                wood_ha_scenario = wood_ha, 
                sng_ha_scenario = sng_ha, 
                urban_ha_scenario = urban_ha, 
                water_ha_scenario = water_ha)

max_bio_offset <- dplyr::full_join(city_base, city_scenario, by = "new2kid") %>% 
  mutate_if(is.numeric, round, digits=4) %>% # round to 4 as base cover is 4 dp
  dplyr::mutate(area_new_builds = ifelse(urban_ha_scenario-urban_ha_base <= 0, 0, urban_ha_scenario-urban_ha_base), 
                farmland_area = farm_ha_scenario, 
                percent_frm = farm_ha_scenario / 400, 
                percent_grs = sng_ha_scenario / 400, 
                percent_wod = wood_ha_scenario / 400)

max_bio_offset <- merge(seer_2km, max_bio_offset, by = 'new2kid')

max_bio_offset$max_bio_offset <- 0

# Load biodiversity data
setwd(paste0(gitpath, "Output/"))
max_bio <- read.csv('all_farm2mixed_bio_sprawl_2031.csv')[, c('new2kid', 
                                                              'sr_chg_ha')]

max_bio <- merge(seer_2km, max_bio, by = 'new2kid')

# reorder the cells from the highest species richness increases to the lowest
sort_idx <- sort(max_bio$sr_chg_ha, decreasing = TRUE, index.return = TRUE)[[2]]
max_bio <- max_bio[sort_idx, ]
idx_positive_bio <- sum(max_bio$sr_chg_ha > 0)
reshuffle_vector <- c(idx_positive_bio:nrow(max_bio))
resampled_vector <- sample(reshuffle_vector)
max_bio[reshuffle_vector, ] <- max_bio[resampled_vector,]

# Tot offset area = tot area of new buildings
tot_offset <- sum(max_bio_offset$area_new_builds)
offset_proj <- max_bio_offset$area_new_builds[max_bio_offset$area_new_builds > 0]
sort_idx <- sort(offset_proj, decreasing = TRUE, index.return = TRUE)[[2]]
offset_proj <- offset_proj[sort_idx]

allocated_land <- 0
i <- 1
j <- 1

while(round(allocated_land, 5) < round(tot_offset, 5)){
  offset_cell <- max_bio$new2kid[i]
  offset_land <- offset_proj[j]
  idx <- which(max_bio_offset$new2kid == offset_cell)
  avail_land <- max_bio_offset$farmland_area[idx]
  if(avail_land > offset_land){
    max_bio_offset$max_bio_offset[idx] <- offset_land
    max_bio_offset$farmland_area[idx] <- max_bio_offset$farmland_area[idx] - offset_land
    offset_area_perc <- offset_land / 400 # CORRECT TO /400 TO CORRECT THE % ? 
    max_bio_offset$percent_frm[idx] <- max_bio_offset$percent_frm[idx] - offset_area_perc
    max_bio_offset$percent_grs[idx] <- max_bio_offset$percent_grs[idx] + 0.5 * offset_area_perc
    max_bio_offset$percent_wod[idx] <- max_bio_offset$percent_wod[idx] + 0.5 * offset_area_perc
    allocated_land <- allocated_land + offset_land
    i <- i + 1
    j <- j + 1
  } else {
    i <- i + 1
  }
}

max_bio_offset_ha <- max_bio_offset %>% 
  dplyr::mutate(
    wood_ha = wood_ha_scenario + (max_bio_offset/2), 
    sng_ha = sng_ha_scenario + (max_bio_offset/2)) %>% 
  rename(
    farm_ha = farmland_area,
    urban_ha = urban_ha_scenario, 
    water_ha = water_ha_scenario) %>% 
  dplyr::select(
    new2kid, 
    farm_ha,
    wood_ha, 
    sng_ha, 
    urban_ha, 
    water_ha,
    max_bio_offset) %>% 
  dplyr::rename(offset_area_ha = max_bio_offset)

# check cell areas
max_bio_offset_ha %>% 
  group_by(new2kid) %>% 
  mutate(sum = farm_ha + wood_ha + sng_ha + urban_ha +water_ha) %>% 
  filter(sum != 400) # some small rounding errors 

# check all offset is allocated 
(c(sum(max_bio_offset$max_bio_offset), sum(max_bio_offset$area_new_builds))) 

#save 
setwd(paste0(gitpath,"Output/"))
st_write(max_bio_offset_ha, 'max_bio_offset_urban_sprawl_2031.csv')


## 2.3. Offset based on max Ecosystem Services
## -------------------------------------------
# Load new housing locations
setwd(paste0(datapath, '/Data/Urban Sprawl - F.Eigenbrod/'))

city_base <- read.csv(paste0(datapath, "Data/LCM/LCM_2km/lcm_aggr_2000.csv")) %>% 
  dplyr::rename(farm_ha_base = farm_ha, 
                wood_ha_base = wood_ha, 
                sng_ha_base = sng_ha, 
                urban_ha_base = urban_ha, 
                water_ha_base = water_ha)

city_scenario <- read.csv('./urban_sprawl_2031_sprawl.csv')%>% 
  dplyr::rename(farm_ha_scenario = farm_ha, 
                wood_ha_scenario = wood_ha, 
                sng_ha_scenario = sng_ha, 
                urban_ha_scenario = urban_ha, 
                water_ha_scenario = water_ha)

max_es_offset <- dplyr::full_join(city_base, city_scenario, by = "new2kid") %>% 
  mutate_if(is.numeric, round, digits=4) %>% # round to 4 as base cover is 4 dp
  dplyr::mutate(area_new_builds = ifelse(urban_ha_scenario-urban_ha_base <= 0, 0, urban_ha_scenario-urban_ha_base), 
                farmland_area = farm_ha_scenario, 
                percent_frm = farm_ha_scenario / 400, 
                percent_grs = sng_ha_scenario / 400, 
                percent_wod = wood_ha_scenario / 400)

max_es_offset <- merge(seer_2km, max_es_offset, by = 'new2kid')

max_es_offset$max_es_offset <- 0

# Load ecosystem service data
setwd(paste0(gitpath,'Output/'))
max_es <- read.csv('all_farm2mixed_tot_es_sprawl_2031.csv')[, c('new2kid',
                                                                   'tot_es_ha')]

max_es$tot_es_ha[is.na(max_es$tot_es_ha)] <- 0

max_es <- merge(seer_2km, max_es, by = 'new2kid')

# reorder the cells from the highest species richness increases to the lowest
sort_idx <- sort(max_es$tot_es_ha, decreasing = TRUE, index.return = TRUE)[[2]]
max_es <- max_es[sort_idx, ]
idx_positive_es <- sum(max_es$tot_es_ha > 0)
reshuffle_vector <- c(idx_positive_es:nrow(max_es))
resampled_vector <- sample(reshuffle_vector)
max_es[reshuffle_vector, ] <- max_es[resampled_vector,]

# Tot offset area = tot area of new buildings
tot_offset <- sum(max_es_offset$area_new_builds)
offset_proj <- max_es_offset$area_new_builds[max_es_offset$area_new_builds > 0]
sort_idx <- sort(offset_proj, decreasing = TRUE, index.return = TRUE)[[2]]
offset_proj <- offset_proj[sort_idx]

allocated_land <- 0
i <- 1
j <- 1

while(round(allocated_land, 5) < round(tot_offset, 5)){
  offset_cell <- max_es$new2kid[i]
  offset_land <- offset_proj[j]
  idx <- which(max_es_offset$new2kid == offset_cell)
  avail_land <- max_es_offset$farmland_area[idx]
  if(avail_land > offset_land){
    max_es_offset$max_es_offset[idx] <- offset_land
    max_es_offset$farmland_area[idx] <- max_es_offset$farmland_area[idx] - offset_land
    offset_area_perc <- offset_land / 400 # CORRECT TO /400 TO CORRECT THE % ? 
    max_es_offset$percent_frm[idx] <- max_es_offset$percent_frm[idx] - offset_area_perc
    max_es_offset$percent_grs[idx] <- max_es_offset$percent_grs[idx] + 0.5 * offset_area_perc
    max_es_offset$percent_wod[idx] <- max_es_offset$percent_wod[idx] + 0.5 * offset_area_perc
    allocated_land <- allocated_land + offset_land
    i <- i + 1
    j <- j + 1
  } else {
    i <- i + 1
  }
}

max_es_offset_ha <- max_es_offset %>% 
  dplyr::mutate(
    wood_ha = wood_ha_scenario + (max_es_offset/2), 
    sng_ha = sng_ha_scenario + (max_es_offset/2)) %>% 
  rename(
    farm_ha = farmland_area,
    urban_ha = urban_ha_scenario, 
    water_ha = water_ha_scenario) %>% 
  dplyr::select(
    new2kid, 
    farm_ha,
    wood_ha, 
    sng_ha, 
    urban_ha, 
    water_ha, 
    max_es_offset) %>% 
  dplyr::rename(offset_area_ha = max_es_offset)

# check cell areas
max_es_offset_ha %>% 
  group_by(new2kid) %>% 
  mutate(sum = farm_ha + wood_ha + sng_ha + urban_ha +water_ha) %>% 
  filter(sum != 400) # some small rounding errors 

# check all offset is allocated 
(c(sum(max_es_offset$max_es_offset), sum(max_es_offset$area_new_builds))) 

#save 
setwd(paste0(gitpath,"Output/"))
st_write(max_es_offset_ha, 'max_es_offset_urban_sprawl.csv')


## 2.4. offset based max es, equity weighted for recreation 
## --------------------------------------------------------

# Load new housing locations
setwd(paste0(datapath, '/Data/Urban Sprawl - F.Eigenbrod/'))

city_base <- read.csv(paste0(datapath, "Data/LCM/LCM_2km/lcm_aggr_2000.csv")) %>% 
  dplyr::rename(farm_ha_base = farm_ha, 
                wood_ha_base = wood_ha, 
                sng_ha_base = sng_ha, 
                urban_ha_base = urban_ha, 
                water_ha_base = water_ha)

city_scenario <- read.csv('./urban_sprawl_2031_sprawl.csv')%>% 
  dplyr::rename(farm_ha_scenario = farm_ha, 
                wood_ha_scenario = wood_ha, 
                sng_ha_scenario = sng_ha, 
                urban_ha_scenario = urban_ha, 
                water_ha_scenario = water_ha)

max_es_offset <- dplyr::full_join(city_base, city_scenario, by = "new2kid") %>% 
  mutate_if(is.numeric, round, digits=4) %>% # round to 4 as base cover is 4 dp
  dplyr::mutate(area_new_builds = ifelse(urban_ha_scenario-urban_ha_base <= 0, 0, urban_ha_scenario-urban_ha_base), 
                farmland_area = farm_ha_scenario, 
                percent_frm = farm_ha_scenario / 400, 
                percent_grs = sng_ha_scenario / 400, 
                percent_wod = wood_ha_scenario / 400)

max_es_offset <- merge(seer_2km, max_es_offset, by = 'new2kid')

max_es_offset$max_es_offset <- 0

# Load ecosystem service data
setwd(paste0(gitpath,'Output/'))
max_es <- read.csv('all_farm2mixed_all_es_sprawl_2031.csv')

max_es <- merge(seer_2km, max_es, by = 'new2kid')

# load medium hh income 
med_income <- read.csv(paste0(datapath,'/Data/SEER_SOCIO_ECON/SEER_2k_socioecon_eng.csv')) %>% 
  dplyr::select(new2kid, Med_hh_inc)

max_es <- merge(max_es, med_income, by = 'new2kid')

# equity weighting 
base_income <- median(max_es$Med_hh_inc)
mui <- 1.3
cell_income <- max_es$Med_hh_inc
mui_weights <- base_income^mui * cell_income^(-mui)

max_es$rec <- max_es$rec*mui_weights 

max_es <- max_es %>%
  dplyr::mutate(tot_es = (ghg + rec + flooding + totn + totp), 
                tot_es_ha = tot_es/hectares_chg)

max_es$tot_es_ha[is.na(max_es$tot_es_ha)] <- 0

# reorder the cells from the highest species richness increases to the lowest
sort_idx <- sort(max_es$tot_es_ha, decreasing = TRUE, index.return = TRUE)[[2]]
max_es <- max_es[sort_idx, ]
idx_positive_es <- sum(max_es$tot_es_ha > 0)
reshuffle_vector <- c(idx_positive_es:nrow(max_es))
resampled_vector <- sample(reshuffle_vector)
max_es[reshuffle_vector, ] <- max_es[resampled_vector,]

# Tot offset area = tot area of new buildings
tot_offset <- sum(max_es_offset$area_new_builds)
offset_proj <- max_es_offset$area_new_builds[max_es_offset$area_new_builds > 0]
sort_idx <- sort(offset_proj, decreasing = TRUE, index.return = TRUE)[[2]]
offset_proj <- offset_proj[sort_idx]

allocated_land <- 0
i <- 1
j <- 1

while(round(allocated_land, 5) < round(tot_offset, 5)){
  offset_cell <- max_es$new2kid[i]
  offset_land <- offset_proj[j]
  idx <- which(max_es_offset$new2kid == offset_cell)
  avail_land <- max_es_offset$farmland_area[idx]
  if(avail_land > offset_land){
    max_es_offset$max_es_offset[idx] <- offset_land
    max_es_offset$farmland_area[idx] <- max_es_offset$farmland_area[idx] - offset_land
    offset_area_perc <- offset_land / 400 # CORRECT TO /400 TO CORRECT THE % ? 
    max_es_offset$percent_frm[idx] <- max_es_offset$percent_frm[idx] - offset_area_perc
    max_es_offset$percent_grs[idx] <- max_es_offset$percent_grs[idx] + 0.5 * offset_area_perc
    max_es_offset$percent_wod[idx] <- max_es_offset$percent_wod[idx] + 0.5 * offset_area_perc
    allocated_land <- allocated_land + offset_land
    i <- i + 1
    j <- j + 1
  } else {
    i <- i + 1
  }
}

max_es_offset_ha_equity_weighted <- max_es_offset %>% 
  dplyr::mutate(
    wood_ha = wood_ha_scenario + (max_es_offset/2), 
    sng_ha = sng_ha_scenario + (max_es_offset/2)) %>% 
  rename(
    farm_ha = farmland_area,
    urban_ha = urban_ha_scenario, 
    water_ha = water_ha_scenario) %>% 
  dplyr::select(
    new2kid, 
    farm_ha,
    wood_ha, 
    sng_ha, 
    urban_ha, 
    water_ha, 
    max_es_offset) %>% 
  dplyr::rename(offset_area_ha = max_es_offset)

# check cell areas
max_es_offset_ha_equity_weighted %>% 
  group_by(new2kid) %>% 
  mutate(sum = farm_ha + wood_ha + sng_ha + urban_ha +water_ha) %>% 
  filter(sum != 400) # some small rounding errors 

# check all offset is allocated 
(c(sum(max_es_offset$max_es_offset), sum(max_es_offset$area_new_builds)))

# save 
setwd(paste0(gitpath,"Output/"))
st_write(max_es_offset_ha_equity_weighted, 'max_es_offset_urban_sprawl_equity_weighted.csv')


## 2.5. offset based max rec, equity weighted for recreation 
## ---------------------------------------------------------

# Load new housing locations
setwd(paste0(datapath, '/Data/Urban Sprawl - F.Eigenbrod/'))

city_base <- read.csv(paste0(datapath, "Data/LCM/LCM_2km/lcm_aggr_2000.csv")) %>% 
  dplyr::rename(farm_ha_base = farm_ha, 
                wood_ha_base = wood_ha, 
                sng_ha_base = sng_ha, 
                urban_ha_base = urban_ha, 
                water_ha_base = water_ha)

city_scenario <- read.csv('./urban_sprawl_2031_sprawl.csv')%>% 
  dplyr::rename(farm_ha_scenario = farm_ha, 
                wood_ha_scenario = wood_ha, 
                sng_ha_scenario = sng_ha, 
                urban_ha_scenario = urban_ha, 
                water_ha_scenario = water_ha)

max_rec_offset <- dplyr::full_join(city_base, city_scenario, by = "new2kid") %>% 
  mutate_if(is.numeric, round, digits=4) %>% # round to 4 as base cover is 4 dp
  dplyr::mutate(area_new_builds = ifelse(urban_ha_scenario-urban_ha_base <= 0, 0, urban_ha_scenario-urban_ha_base), 
                farmland_area = farm_ha_scenario, 
                percent_frm = farm_ha_scenario / 400, 
                percent_grs = sng_ha_scenario / 400, 
                percent_wod = wood_ha_scenario / 400)

max_rec_offset <- merge(seer_2km, max_rec_offset, by = 'new2kid')

max_rec_offset$max_rec_offset <- 0

# Load ecosystem service data
setwd(paste0(gitpath,'Output/'))
max_rec <- read.csv('all_farm2mixed_all_es_sprawl_2031.csv')

max_rec <- merge(seer_2km, max_rec, by = 'new2kid')

# load medium hh income 
med_income <- read.csv(paste0(datapath,'/Data/SEER_SOCIO_ECON/SEER_2k_socioecon_eng.csv')) %>% 
  dplyr::select(new2kid, Med_hh_inc)

max_rec <- merge(max_rec, med_income, by = 'new2kid')

# equity weighting 
base_income <- median(max_rec$Med_hh_inc)
mui <- 1.3
cell_income <- max_rec$Med_hh_inc
mui_weights <- base_income^mui * cell_income^(-mui)

max_rec$rec <- max_rec$rec*mui_weights 

max_rec <- max_rec %>%
  dplyr::mutate(rec_ha = rec/hectares_chg)

max_rec$rec_ha[is.na(max_rec$rec_ha)] <- 0

# reorder the cells from the highest species richness increases to the lowest
sort_idx <- sort(max_rec$rec_ha, decreasing = TRUE, index.return = TRUE)[[2]]
max_rec <- max_rec[sort_idx, ]
idx_positive_rec <- sum(max_rec$rec_ha > 0)
reshuffle_vector <- c(idx_positive_rec:nrow(max_rec))
resampled_vector <- sample(reshuffle_vector)
max_rec[reshuffle_vector, ] <- max_rec[resampled_vector,]

# Tot offset area = tot area of new buildings
tot_offset <- sum(max_rec_offset$area_new_builds)
offset_proj <- max_rec_offset$area_new_builds[max_rec_offset$area_new_builds > 0]
sort_idx <- sort(offset_proj, decreasing = TRUE, index.return = TRUE)[[2]]
offset_proj <- offset_proj[sort_idx]

allocated_land <- 0
i <- 1
j <- 1

while(round(allocated_land, 5) < round(tot_offset, 5)){
  offset_cell <- max_rec$new2kid[i]
  offset_land <- offset_proj[j]
  idx <- which(max_rec_offset$new2kid == offset_cell)
  avail_land <- max_rec_offset$farmland_area[idx]
  if(avail_land > offset_land){
    max_rec_offset$max_rec_offset[idx] <- offset_land
    max_rec_offset$farmland_area[idx] <- max_rec_offset$farmland_area[idx] - offset_land
    offset_area_perc <- offset_land / 400 # CORRECT TO /400 TO CORRECT THE % ? 
    max_rec_offset$percent_frm[idx] <- max_rec_offset$percent_frm[idx] - offset_area_perc
    max_rec_offset$percent_grs[idx] <- max_rec_offset$percent_grs[idx] + 0.5 * offset_area_perc
    max_rec_offset$percent_wod[idx] <- max_rec_offset$percent_wod[idx] + 0.5 * offset_area_perc
    allocated_land <- allocated_land + offset_land
    i <- i + 1
    j <- j + 1
  } else {
    i <- i + 1
  }
}

max_rec_offset_ha_equity_weighted <- max_rec_offset %>% 
  dplyr::mutate(
    wood_ha = wood_ha_scenario + (max_rec_offset/2), 
    sng_ha = sng_ha_scenario + (max_rec_offset/2)) %>% 
  rename(
    farm_ha = farmland_area,
    urban_ha = urban_ha_scenario, 
    water_ha = water_ha_scenario) %>% 
  dplyr::select(
    new2kid, 
    farm_ha,
    wood_ha, 
    sng_ha, 
    urban_ha, 
    water_ha, 
    max_rec_offset) %>% 
  dplyr::rename(offset_area_ha = max_rec_offset)

# check cell areas
max_rec_offset_ha_equity_weighted %>% 
  group_by(new2kid) %>% 
  mutate(sum = farm_ha + wood_ha + sng_ha + urban_ha +water_ha) %>% 
  filter(sum != 400) # some small rounding errors 

# check all offset is allocated 
(c(sum(max_rec_offset$max_es_offset), sum(max_rec_offset$area_new_builds)))

# save 
setwd(paste0(gitpath,"Output/"))
st_write(max_rec_offset_ha_equity_weighted, 'max_rec_offset_urban_sprawl_equity_weighted.csv')

## 2.6. offset based max rec
## -------------------------

# Load new housing locations
setwd(paste0(datapath, '/Data/Urban Sprawl - F.Eigenbrod/'))

city_base <- read.csv(paste0(datapath, "Data/LCM/LCM_2km/lcm_aggr_2000.csv")) %>% 
  dplyr::rename(farm_ha_base = farm_ha, 
                wood_ha_base = wood_ha, 
                sng_ha_base = sng_ha, 
                urban_ha_base = urban_ha, 
                water_ha_base = water_ha)

city_scenario <- read.csv('./urban_sprawl_2031_sprawl.csv')%>% 
  dplyr::rename(farm_ha_scenario = farm_ha, 
                wood_ha_scenario = wood_ha, 
                sng_ha_scenario = sng_ha, 
                urban_ha_scenario = urban_ha, 
                water_ha_scenario = water_ha)

max_rec_offset <- dplyr::full_join(city_base, city_scenario, by = "new2kid") %>% 
  mutate_if(is.numeric, round, digits=4) %>% # round to 4 as base cover is 4 dp
  dplyr::mutate(area_new_builds = ifelse(urban_ha_scenario-urban_ha_base <= 0, 0, urban_ha_scenario-urban_ha_base), 
                farmland_area = farm_ha_scenario, 
                percent_frm = farm_ha_scenario / 400, 
                percent_grs = sng_ha_scenario / 400, 
                percent_wod = wood_ha_scenario / 400)

max_rec_offset <- merge(seer_2km, max_rec_offset, by = 'new2kid')

max_rec_offset$max_rec_offset <- 0

# Load ecosystem service data
setwd(paste0(gitpath,'Output/'))
max_rec <- read.csv('all_farm2mixed_all_es_sprawl_2031.csv')

max_rec <- merge(seer_2km, max_rec, by = 'new2kid')

max_rec <- max_rec %>%
  dplyr::mutate(rec_ha = rec/hectares_chg)

max_rec$rec_ha[is.na(max_rec$rec_ha)] <- 0

# reorder the cells from the highest species richness increases to the lowest
sort_idx <- sort(max_rec$rec_ha, decreasing = TRUE, index.return = TRUE)[[2]]
max_rec <- max_rec[sort_idx, ]
idx_positive_rec <- sum(max_rec$rec_ha > 0)
reshuffle_vector <- c(idx_positive_rec:nrow(max_rec))
resampled_vector <- sample(reshuffle_vector)
max_rec[reshuffle_vector, ] <- max_rec[resampled_vector,]

# Tot offset area = tot area of new buildings
tot_offset <- sum(max_rec_offset$area_new_builds)
offset_proj <- max_rec_offset$area_new_builds[max_rec_offset$area_new_builds > 0]
sort_idx <- sort(offset_proj, decreasing = TRUE, index.return = TRUE)[[2]]
offset_proj <- offset_proj[sort_idx]

allocated_land <- 0
i <- 1
j <- 1

while(round(allocated_land, 5) < round(tot_offset, 5)){
  offset_cell <- max_rec$new2kid[i]
  offset_land <- offset_proj[j]
  idx <- which(max_rec_offset$new2kid == offset_cell)
  avail_land <- max_rec_offset$farmland_area[idx]
  if(avail_land > offset_land){
    max_rec_offset$max_rec_offset[idx] <- offset_land
    max_rec_offset$farmland_area[idx] <- max_rec_offset$farmland_area[idx] - offset_land
    offset_area_perc <- offset_land / 400 # CORRECT TO /400 TO CORRECT THE % ? 
    max_rec_offset$percent_frm[idx] <- max_rec_offset$percent_frm[idx] - offset_area_perc
    max_rec_offset$percent_grs[idx] <- max_rec_offset$percent_grs[idx] + 0.5 * offset_area_perc
    max_rec_offset$percent_wod[idx] <- max_rec_offset$percent_wod[idx] + 0.5 * offset_area_perc
    allocated_land <- allocated_land + offset_land
    i <- i + 1
    j <- j + 1
  } else {
    i <- i + 1
  }
}

max_rec_offset_ha <- max_rec_offset %>% 
  dplyr::mutate(
    wood_ha = wood_ha_scenario + (max_rec_offset/2), 
    sng_ha = sng_ha_scenario + (max_rec_offset/2)) %>% 
  rename(
    farm_ha = farmland_area,
    urban_ha = urban_ha_scenario, 
    water_ha = water_ha_scenario) %>% 
  dplyr::select(
    new2kid, 
    farm_ha,
    wood_ha, 
    sng_ha, 
    urban_ha, 
    water_ha, 
    max_rec_offset) %>% 
  dplyr::rename(offset_area_ha = max_rec_offset)

# check cell areas
max_rec_offset_ha %>% 
  group_by(new2kid) %>% 
  mutate(sum = farm_ha + wood_ha + sng_ha + urban_ha +water_ha) %>% 
  filter(sum != 400) # some small rounding errors 

# check all offset is allocated 
(c(sum(max_rec_offset$max_es_offset), sum(max_rec_offset$area_new_builds)))

# save 
setwd(paste0(gitpath,"Output/"))
st_write(max_rec_offset_ha, 'max_rec_offset_urban_sprawl.csv')


## 2.7. Offest based on min. cost
## ------------------------------
# Load new housing locations
setwd(paste0(datapath,"Data/Urban Sprawl - F.Eigenbrod/"))

city_base <- read.csv(paste0(datapath,"Data/LCM/LCM_2km/lcm_aggr_2000.csv")) %>%
  dplyr::rename(farm_ha_base = farm_ha, 
                wood_ha_base = wood_ha, 
                sng_ha_base = sng_ha, 
                urban_ha_base = urban_ha, 
                water_ha_base = water_ha)

city_scenario <- read.csv('./urban_sprawl_2031_sprawl.csv')%>% 
  dplyr::rename(farm_ha_scenario = farm_ha, 
                wood_ha_scenario = wood_ha, 
                sng_ha_scenario = sng_ha, 
                urban_ha_scenario = urban_ha, 
                water_ha_scenario = water_ha)

min_cost_offset <- dplyr::full_join(city_base, city_scenario, by = "new2kid") %>% 
  mutate_if(is.numeric, round, digits=4) %>% # round to 4 as base cover is 4 dp
  dplyr::mutate(area_new_builds = ifelse(urban_ha_scenario-urban_ha_base <= 0, 0, urban_ha_scenario-urban_ha_base), 
                farmland_area = farm_ha_scenario, 
                percent_frm = farm_ha_scenario / 400, 
                percent_grs = sng_ha_scenario / 400, 
                percent_wod = wood_ha_scenario / 400)

min_cost_offset <- merge(seer_2km, min_cost_offset, by = 'new2kid')

min_cost_offset$min_cost_offset <- 0

# Load biodiversity data
setwd(paste0(gitpath, "Output/"))
min_cost <- read.csv('all_farm2mixed_OC_sprawl_2031.csv')[, c('new2kid', 
                                                              'farm_oc_ha')]

min_cost <- merge(seer_2km, min_cost, by = 'new2kid') 
# remove cost = 0 for selection 
min_cost <- min_cost %>% filter(farm_oc_ha != 0)


# reorder the cells from the highest species richness increases to the lowest
sort_idx <- sort(min_cost$farm_oc_ha, decreasing = FALSE, index.return = TRUE)[[2]]
min_cost <- min_cost[sort_idx, ]
# idx_positive_bio <- sum(min_cost$farm_oc_ha > 0)
# reshuffle_vector <- c(idx_positive_bio:nrow(min_cost))
# resampled_vector <- sample(reshuffle_vector)
# min_cost[reshuffle_vector, ] <- min_cost[resampled_vector,]

# Tot offset area = tot area of new buildings
tot_offset <- sum(min_cost_offset$area_new_builds)
offset_proj <- min_cost_offset$area_new_builds[min_cost_offset$area_new_builds > 0]
sort_idx <- sort(offset_proj, decreasing = FALSE, index.return = TRUE)[[2]]
offset_proj <- offset_proj[sort_idx]

allocated_land <- 0
i <- 1
j <- 1

while(round(allocated_land, 5) < round(tot_offset, 5)){
  offset_cell <- min_cost$new2kid[i]
  offset_land <- offset_proj[j]
  idx <- which(min_cost_offset$new2kid == offset_cell)
  avail_land <- min_cost_offset$farmland_area[idx]
  if(avail_land > offset_land){
    min_cost_offset$min_cost_offset[idx] <- offset_land
    min_cost_offset$farmland_area[idx] <- min_cost_offset$farmland_area[idx] - offset_land
    offset_area_perc <- offset_land / 400 # CORRECT TO /400 TO CORRECT THE % ? 
    min_cost_offset$percent_frm[idx] <- min_cost_offset$percent_frm[idx] - offset_area_perc
    min_cost_offset$percent_grs[idx] <- min_cost_offset$percent_grs[idx] + 0.5 * offset_area_perc
    min_cost_offset$percent_wod[idx] <- min_cost_offset$percent_wod[idx] + 0.5 * offset_area_perc
    allocated_land <- allocated_land + offset_land
    i <- i + 1
    j <- j + 1
  } else {
    i <- i + 1
  }
}

min_cost_offset <- min_cost_offset %>% 
  st_drop_geometry()

min_cost_offset <- merge(seer_2km, min_cost_offset, by = 'new2kid')

min_cost_offset_ha <- min_cost_offset %>% 
  dplyr::mutate(
    wood_ha = wood_ha_scenario + (min_cost_offset/2), 
    sng_ha = sng_ha_scenario + (min_cost_offset/2)) %>% 
  rename(
    farm_ha = farmland_area,
    urban_ha = urban_ha_scenario, 
    water_ha = water_ha_scenario) %>% 
  dplyr::select(
    new2kid, 
    farm_ha,
    wood_ha, 
    sng_ha, 
    urban_ha, 
    water_ha,
    min_cost_offset) %>% 
  dplyr::rename(offset_area_ha = min_cost_offset)

# check cell areas
min_cost_offset_ha %>% 
  group_by(new2kid) %>% 
  mutate(sum = farm_ha + wood_ha + sng_ha + urban_ha +water_ha) %>% 
  filter(sum != 400) # some small rounding errors 

# check all offset is allocated 
(c(sum(min_cost_offset$min_cost_offset), sum(min_cost_offset$area_new_builds))) 


#save 
setwd(paste0(gitpath,"Output/"))
st_write(min_cost_offset_ha, 'min_cost_offset_urban_sprawl_2031.csv')
