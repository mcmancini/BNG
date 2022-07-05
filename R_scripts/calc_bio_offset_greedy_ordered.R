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
##   1) locations for compensation near building sites
##   2) locations for compensation in areas with the highest potential
##      biodiversity gains
##   4) locations for compensation in areas where biodiversity is valued the
##      the most. This is computed for two cases: a base case where values are
##      not equity weighed; a second case where values are adjusted based on 
##      decreasing marginal utility of income. Valuation is computed at the
##      household and at the population level.
## =============================================================================

## (0) SET-UP
## ==========
rm(list=ls())
library(sf)
library(fields)       # rdist function
library(ggplot2)
library(viridis)      # scale_fill_viridis
library(gridExtra)    # grid_arrange
library(ggpubr)       # annotate_figure
library(dplyr)

source('D:/Documents/GitHub/biodiversity-net-gain/R Scripts/Functions/fcn_plt_map.R') # what is this? 

#update path for different machines 
path <- "C:/Code/BNG/" 
  
## (1) LOAD THE DATA AND PREPROCESS IT
##     - 1. SEER 2km grid
##     - 2. new housing locations
##     - 3. Census data
##     - 4. Distance matrix
## ===================================

## 1.1. SEER 2km grid
## ------------------
setwd(paste0(path,'/Data/SEER_GRID/'))
seer_2km <- st_read('./SEER_net2km.shp')
seer_2km <- seer_2km[, "new2kid"]

## 1.2. New housing locations
## --------------------------
setwd(paste0(path,'Data/Urban Sprawl - F.Eigenbrod/'))

city_base <- read.csv(paste0(path, "Data/LCM/LCM_2km/lcm_aggr_2000.csv")) %>% 
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

## 1.3. Load census data
## ---------------------
setwd(paste0(path, '/Data/SEER_SOCIO_ECON'))
num_hh <- read.csv('./SEER_2k_socioecon_eng.csv')
sp_df <- city_spread %>% dplyr::select(new2kid)
num_hh <- num_hh[num_hh$new2kid %in% sp_df$new2kid,]
num_hh <- merge(sp_df, num_hh, by = 'new2kid', all = TRUE)

## 1.4. Distance matrix - only required for WTP 
## --------------------
# a <- st_coordinates(st_centroid(city_spread))
# dist_matrix <- rdist(a, a)
# dist_matrix <- dist_matrix / 1600 # convert distance in miles (it is in meters)
# dist_matrix[dist_matrix > 0] <- log(dist_matrix[dist_matrix > 0]) # take the log of distance
  

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

bio_area_perc <- city_spread$local_offset[idx] / (400 * 10000)
city_spread$percent_frm[idx] <- round(city_spread$percent_frm[idx] - bio_area_perc, 9)
city_spread$percent_grs[idx] <- round(city_spread$percent_grs[idx] + (bio_area_perc / 2), 9)
city_spread$percent_wod[idx] <- round(city_spread$percent_wod[idx] + (bio_area_perc / 2), 9)

## Case 2: There is some farmland available but not enough for biodiversity 
## compensation
idx <- (city_spread$area_new_builds > city_spread$local_offset) &
       (city_spread$farmland_area > 0)

city_spread$local_offset[idx] <- city_spread$farmland_area[idx]
city_spread$farmland_area[idx] <- 0

bio_area_perc <- city_spread$local_offset[idx] / (400 * 10000)
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
    water_ha)
  

# check cell areas
local_bio_offset_ha %>% 
  group_by(new2kid) %>% 
  mutate(sum = farm_ha + wood_ha + sng_ha + urban_ha +water_ha) %>% 
  filter(sum != 400) # some small rounding errors 

# check all offset is allocated 
(c(sum(city_spread$local_offset), sum(city_spread$area_new_builds))) 


setwd(paste0(path,"Data/"))
st_write(local_bio_offset_ha, 'local_bio_offset_ha_lc.csv')


## 2.2. Offest based on max biodiversity improvements
## --------------------------------------------------
# Load new housing locations
setwd(paste0(path, '/Data/Urban Sprawl - F.Eigenbrod/'))

city_base <- read.csv(paste0(path, "Data/LCM/LCM_2km/lcm_aggr_2000.csv")) %>% 
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
setwd(paste0(path,'Data/'))
max_bio <- read.csv('bio_results_jncc_england.csv')[, c('new2kid', 
                                                        'farm2mixed_sr_chg_perha')]

max_bio <- merge(seer_2km, max_bio, by = 'new2kid')

# reorder the cells from the highest species richness increases to the lowest
sort_idx <- sort(max_bio$farm2mixed_sr_chg_perha, decreasing = TRUE, index.return = TRUE)[[2]]
max_bio <- max_bio[sort_idx, ]
idx_positive_bio <- sum(max_bio$farm2mixed_sr_chg_perha > 0)
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


max_bio_offset_perc <- max_bio_offset %>%  
  # convert urban and water to % 
  mutate(
    percent_urban = urban_ha_scenario / 400, 
    percent_water = water_ha_scenario / 400) %>% 
  #select the 5 LU classes for NEV
  dplyr::select( 
    new2kid,
    percent_frm,   
    percent_grs, 
    percent_wod, 
    percent_urban, 
    percent_water)

# check percentatge per cell = 1 
test <- max_bio_offset_perc %>% 
  group_by(new2kid) %>% 
  mutate(sum = percent_frm + percent_grs + percent_wod + percent_urban + percent_water) %>% 
  filter(sum != 1) # small rounding errors 


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
    water_ha)

# check cell areas
max_bio_offset_ha %>% 
  group_by(new2kid) %>% 
  mutate(sum = farm_ha + wood_ha + sng_ha + urban_ha +water_ha) %>% 
  filter(sum != 400) # some small rounding errors 

# check all offset is allocated 
(c(sum(max_bio_offset$max_bio_offset), sum(max_bio_offset$area_new_builds))) 

#save 
setwd(paste0(path,"Data/"))
st_write(max_bio_offset_perc, 'max_bio_offset_perc_lc.csv')
st_write(max_bio_offset_ha, 'max_bio_offset_ha_lc.csv')

## 2.3. offset based on max household wtp 
## --------------------------------------

# Load new housing locations
setwd('D:/Documents/GitHub/biodiversity-net-gain/Data/Urban Sprawl - F.Eigenbrod/')
wtp_hh_offset <- read.csv('./urban_sprawl_2km_farm.csv')
wtp_hh_offset <- merge(seer_2km, wtp_hh_offset, by = 'new2kid')
wtp_hh_offset$wtp_hh_offset <- 0
wtp_hh_offset <- wtp_hh_offset[, c(1:11, 13, 12)]

# calculate max WTP based on tot available size
# set coefficients
coef_dist <- -4.23
coef_size <- 0.03
coef_country <- 2.38
coef_bird <- 12.45

# set land size matrix
land_size <- wtp_hh_offset$farmland_area / 1e4
start_time <- Sys.time()
land_size <- t(matrix(land_size, 
                    nrow = nrow(wtp_hh_offset), 
                    ncol = nrow(wtp_hh_offset)))
end_time <- Sys.time()
end_time - start_time

no_farm_idx <- wtp_hh_offset$farmland_area == 0

# compute WTP for conversion of all land available in each cell
wtp <- coef_country + dist_matrix * coef_dist + coef_bird + coef_size * land_size
wtp[, no_farm_idx] <- 0

# find max wtp for each household and from which cell it is generated
max_wtp_vals <- apply(wtp, 1, FUN=max)
max_wtp_cell <- apply(wtp, 1, FUN=which.max)

# for each cell, identify how much WTP is generated from the conversion
wtp_max <- wtp_hh_offset[, c('new2kid', 'farmland_area')]
wtp_max$wtp <- 0
wtp_max <- wtp_max[, c(1, 2, 4, 3)]

for (i in unique(max_wtp_cell)){
  idx <- max_wtp_cell == i
  aggr_wtp <- sum(max_wtp_vals[idx])
  wtp_max$wtp[i] <- aggr_wtp
}

# reorder the cells based on decreasing WTP generated
wtp_max$wtp_ha <- wtp_max$wtp / (wtp_max$farmland_area / 1e4)
wtp_max$wtp_ha[is.na(wtp_max$wtp_ha)] <- 0
wtp_max$wtp_ha[is.infinite(wtp_max$wtp_ha)] <- 0
sort_idx <- sort(wtp_max$wtp_ha, decreasing = TRUE, index.return = TRUE)[[2]]
wtp_max <- wtp_max[sort_idx, ]
idx_positive_wtp <- sum(wtp_max$wtp_ha > 0)
reshuffle_vector <- c(idx_positive_wtp:nrow(wtp_max))
resampled_vector <- sample(reshuffle_vector)
wtp_max[reshuffle_vector, ] <- wtp_max[resampled_vector,]

# Tot offset area = tot area of new buildings
tot_offset <- sum(wtp_hh_offset$area_new_builds)
offset_proj <- wtp_hh_offset$area_new_builds[wtp_hh_offset$area_new_builds > 0]
sort_idx <- sort(offset_proj, decreasing = TRUE, index.return = TRUE)[[2]]
offset_proj <- offset_proj[sort_idx]

allocated_land <- 0
i <- 1
j <- 1

while(round(allocated_land, 5) < round(tot_offset, 5)){
  offset_cell <- wtp_max$new2kid[i]
  offset_land <- offset_proj[j]
  avail_land <- wtp_max$farmland_area[i]
  idx <- which(wtp_hh_offset$new2kid == offset_cell)
  if(avail_land > offset_land){
    wtp_hh_offset$wtp_hh_offset[idx] <- offset_land
    wtp_hh_offset$farmland_area[idx] <- wtp_hh_offset$farmland_area[idx] - offset_land
    offset_area_perc <- offset_land / 4e6
    wtp_hh_offset$percent_frm[idx] <- wtp_hh_offset$percent_frm[idx] - offset_area_perc
    wtp_hh_offset$percent_grs[idx] <- wtp_hh_offset$percent_grs[idx] + 0.5 * offset_area_perc
    wtp_hh_offset$percent_wod[idx] <- wtp_hh_offset$percent_wod[idx] + 0.5 * offset_area_perc
    allocated_land <- allocated_land + offset_land
    i <- i + 1
    j <- j + 1
  } else {
    i <- i + 1
  }
}

setwd('D:/Documents/GitHub/biodiversity-net-gain/CSV Files/bio_offset_landuses/delta_birds')
st_write(wtp_hh_offset, 'wtp_hh_offset.csv')


## 2.4. offset based on max population wtp 
## ---------------------------------------

# Load new housing locations
setwd('D:/Documents/GitHub/biodiversity-net-gain/Data/Urban Sprawl - F.Eigenbrod/')
wtp_pop_offset <- read.csv('./urban_sprawl_2km_farm.csv')
wtp_pop_offset <- merge(seer_2km, wtp_pop_offset, by = 'new2kid')
wtp_pop_offset$wtp_pop_offset <- 0
wtp_pop_offset <- wtp_pop_offset[, c(1:11, 13, 12)]

# calculate max WTP based on available size and population
# set coefficients
coef_dist <- -4.23
coef_size <- 0.03
coef_country <- 2.38
coef_bird <- 12.45

# set land size matrix (AAA: rows: households; columns: offset cells)
land_size <- wtp_pop_offset$farmland_area / 1e4
start_time <- Sys.time()
land_size <- t(matrix(land_size, 
                      nrow = nrow(wtp_pop_offset), 
                      ncol = nrow(wtp_pop_offset)))
end_time <- Sys.time()
end_time - start_time

# set household size matrix. AAA: this is not transposed because WTP is
# multiplied by household size of the household willing to pay!!!
hh_size <- num_hh$Households
hh_size <- matrix(hh_size, 
                  nrow = nrow(wtp_pop_offset), 
                  ncol = nrow(wtp_pop_offset))

# compute WTP for conversion of all land available in each cell
wtp <- hh_size * (coef_country + dist_matrix * coef_dist + coef_bird + coef_size * land_size)

# find max wtp for each household and from which cell it is generated
max_wtp_vals <- apply(wtp, 1, FUN=max)
max_wtp_cell <- apply(wtp, 1, FUN=which.max)

# for each cell, identify how much WTP is generated from the conversion
wtp_max <- wtp_pop_offset[, c('new2kid', 'farmland_area')]
wtp_max$wtp <- 0
wtp_max <- wtp_max[, c(1, 2, 4, 3)]

for (i in unique(max_wtp_cell)){
  idx <- max_wtp_cell == i
  aggr_wtp <- sum(max_wtp_vals[idx])
  wtp_max$wtp[i] <- aggr_wtp
}

# reorder the cells based on decreasing WTP generated
wtp_max$wtp_ha <- wtp_max$wtp / (wtp_max$farmland_area / 1e4)
wtp_max$wtp_ha[is.na(wtp_max$wtp_ha)] <- 0
wtp_max$wtp_ha[is.infinite(wtp_max$wtp_ha)] <- 0
sort_idx <- sort(wtp_max$wtp_ha, decreasing = TRUE, index.return = TRUE)[[2]]
wtp_max <- wtp_max[sort_idx, ]
idx_positive_wtp <- sum(wtp_max$wtp_ha > 0)
reshuffle_vector <- c(idx_positive_wtp:nrow(wtp_max))
resampled_vector <- sample(reshuffle_vector)
wtp_max[reshuffle_vector, ] <- wtp_max[resampled_vector,]

# Tot offset area = tot area of new buildings
tot_offset <- sum(wtp_pop_offset$area_new_builds)
offset_proj <- wtp_pop_offset$area_new_builds[wtp_pop_offset$area_new_builds > 0]
sort_idx <- sort(offset_proj, decreasing = TRUE, index.return = TRUE)[[2]]
offset_proj <- offset_proj[sort_idx]

allocated_land <- 0
i <- 1
j <- 1

while(round(allocated_land, 5) < round(tot_offset, 5)){
  offset_cell <- wtp_max$new2kid[i]
  offset_land <- offset_proj[j]
  avail_land <- wtp_max$farmland_area[i]
  idx <- which(wtp_pop_offset$new2kid == offset_cell)
  if(avail_land > offset_land){
    wtp_pop_offset$wtp_pop_offset[idx] <- offset_land
    wtp_pop_offset$farmland_area[idx] <- wtp_pop_offset$farmland_area[idx] - offset_land
    offset_area_perc <- offset_land / 4e6
    wtp_pop_offset$percent_frm[idx] <- wtp_pop_offset$percent_frm[idx] - offset_area_perc
    wtp_pop_offset$percent_grs[idx] <- wtp_pop_offset$percent_grs[idx] + 0.5 * offset_area_perc
    wtp_pop_offset$percent_wod[idx] <- wtp_pop_offset$percent_wod[idx] + 0.5 * offset_area_perc
    allocated_land <- allocated_land + offset_land
    i <- i + 1
    j <- j + 1
  } else {
    i <- i + 1
  }
}

setwd('D:/Documents/GitHub/biodiversity-net-gain/CSV Files/bio_offset_landuses/delta_birds')
st_write(wtp_pop_offset, 'wtp_pop_offset.csv')

## 2.5. offset based on max household wtp, equity weighted 
## -------------------------------------------------------

# Load new housing locations
setwd('D:/Documents/GitHub/biodiversity-net-gain/Data/Urban Sprawl - F.Eigenbrod/')
wtp_hh_mui_offset <- read.csv('./urban_sprawl_2km_farm.csv')
wtp_hh_mui_offset <- merge(seer_2km, wtp_hh_mui_offset, by = 'new2kid')
wtp_hh_mui_offset$wtp_hh_mui_offset <- 0
wtp_hh_mui_offset <- wtp_hh_mui_offset[, c(1:11, 13, 12)]

# calculate max WTP based on tot available size
# set coefficients
coef_dist <- -4.23
coef_size <- 0.03
coef_country <- 2.38
coef_bird <- 12.45

# set land size matrix
land_size <- wtp_hh_mui_offset$farmland_area / 1e4
start_time <- Sys.time()
land_size <- t(matrix(land_size, 
                      nrow = nrow(wtp_hh_mui_offset), 
                      ncol = nrow(wtp_hh_mui_offset)))
end_time <- Sys.time()
end_time - start_time

# Set income weight matrix. AAA: this is not transposed because WTP is
# multiplied by mui of the household willing to pay!!!
base_income <- 29823 #Office from National Statistics 2018, after spatial aggregation
mui <- 1.3
cell_income <- num_hh$Med_hh_inc
mui_weights <- base_income^mui * cell_income^(-mui)

mui_weights <- matrix(mui_weights, 
                      nrow = nrow(wtp_hh_mui_offset), 
                      ncol = nrow(wtp_hh_mui_offset))


# compute WTP for conversion of all land available in each cell
wtp <- mui_weights * (coef_country + dist_matrix * coef_dist + coef_bird + coef_size * land_size)

# find max wtp for each household and from which cell it is generated
max_wtp_vals <- apply(wtp, 1, FUN=max)
max_wtp_cell <- apply(wtp, 1, FUN=which.max)

# for each cell, identify how much WTP is generated from the conversion
wtp_max <- wtp_hh_mui_offset[, c('new2kid', 'farmland_area')]
wtp_max$wtp <- 0
wtp_max <- wtp_max[, c(1, 2, 4, 3)]

for (i in unique(max_wtp_cell)){
  idx <- max_wtp_cell == i
  aggr_wtp <- sum(max_wtp_vals[idx])
  wtp_max$wtp[i] <- aggr_wtp
}

# reorder the cells based on decreasing WTP generated
wtp_max$wtp_ha <- wtp_max$wtp / (wtp_max$farmland_area / 1e4)
wtp_max$wtp_ha[is.na(wtp_max$wtp_ha)] <- 0
wtp_max$wtp_ha[is.infinite(wtp_max$wtp_ha)] <- 0
sort_idx <- sort(wtp_max$wtp_ha, decreasing = TRUE, index.return = TRUE)[[2]]
wtp_max <- wtp_max[sort_idx, ]
idx_positive_wtp <- sum(wtp_max$wtp_ha > 0)
reshuffle_vector <- c(idx_positive_wtp:nrow(wtp_max))
resampled_vector <- sample(reshuffle_vector)
wtp_max[reshuffle_vector, ] <- wtp_max[resampled_vector,]

# # reorder the cells based on decreasing WTP generated
# sort_idx <- sort(wtp_max$wtp, decreasing = TRUE, index.return = TRUE)[[2]]
# wtp_max <- wtp_max[sort_idx, ]
# idx_positive_wtp <- sum(wtp_max$wtp > 0)
# reshuffle_vector <- c(idx_positive_wtp:nrow(wtp_max))
# resampled_vector <- sample(reshuffle_vector)
# wtp_max[reshuffle_vector, ] <- wtp_max[resampled_vector,]

# Tot offset area = tot area of new buildings
tot_offset <- sum(wtp_hh_mui_offset$area_new_builds)
offset_proj <- wtp_hh_mui_offset$area_new_builds[wtp_hh_mui_offset$area_new_builds > 0]
sort_idx <- sort(offset_proj, decreasing = TRUE, index.return = TRUE)[[2]]
offset_proj <- offset_proj[sort_idx]

allocated_land <- 0
i <- 1
j <- 1

while(round(allocated_land, 5) < round(tot_offset, 5)){
  offset_cell <- wtp_max$new2kid[i]
  offset_land <- offset_proj[j]
  avail_land <- wtp_max$farmland_area[i]
  idx <- which(wtp_hh_mui_offset$new2kid == offset_cell)
  if(avail_land > offset_land){
    wtp_hh_mui_offset$wtp_hh_mui_offset[idx] <- offset_land
    wtp_hh_mui_offset$farmland_area[idx] <- wtp_hh_mui_offset$farmland_area[idx] - offset_land
    offset_area_perc <- offset_land / 4e6
    wtp_hh_mui_offset$percent_frm[idx] <- wtp_hh_mui_offset$percent_frm[idx] - offset_area_perc
    wtp_hh_mui_offset$percent_grs[idx] <- wtp_hh_mui_offset$percent_grs[idx] + 0.5 * offset_area_perc
    wtp_hh_mui_offset$percent_wod[idx] <- wtp_hh_mui_offset$percent_wod[idx] + 0.5 * offset_area_perc
    allocated_land <- allocated_land + offset_land
    i <- i + 1
    j <- j + 1
  } else {
    i <- i + 1
  }
}

setwd('D:/Documents/GitHub/biodiversity-net-gain/CSV Files/bio_offset_landuses/delta_birds')
st_write(wtp_hh_mui_offset, 'wtp_hh_mui_offset.csv')


## 2.6. offset based on max population wtp, equity weighted 
## --------------------------------------------------------

# Load new housing locations
setwd('D:/Documents/GitHub/biodiversity-net-gain/Data/Urban Sprawl - F.Eigenbrod/')
wtp_pop_mui_offset <- read.csv('./urban_sprawl_2km_farm.csv')
wtp_pop_mui_offset <- merge(seer_2km, wtp_pop_mui_offset, by = 'new2kid')
wtp_pop_mui_offset$wtp_pop_mui_offset <- 0
wtp_pop_mui_offset <- wtp_pop_mui_offset[, c(1:11, 13, 12)]

# calculate max WTP based on tot available size
# set coefficients
coef_dist <- -4.23
coef_size <- 0.03
coef_country <- 2.38
coef_bird <- 12.45

# set land size matrix
land_size <- wtp_pop_mui_offset$farmland_area / 1e4
start_time <- Sys.time()
land_size <- t(matrix(land_size, 
                      nrow = nrow(wtp_pop_mui_offset), 
                      ncol = nrow(wtp_pop_mui_offset)))
end_time <- Sys.time()
end_time - start_time

# set household size matrix. AAA: this is not transposed because WTP is
# multiplied by household size of the household willing to pay!!!
hh_size <- num_hh$Households
hh_size <- matrix(hh_size, 
                  nrow = nrow(wtp_pop_mui_offset), 
                  ncol = nrow(wtp_pop_mui_offset))

# Set income weight matrix. AAA: this is not transposed because WTP is
# multiplied by mui of the household willing to pay!!!
base_income <- 29823 #Office from National Statistics 2018
mui <- 1.3
cell_income <- num_hh$Med_hh_inc
mui_weights <- base_income^mui * cell_income^(-mui)

mui_weights <- matrix(mui_weights, 
                      nrow = nrow(wtp_pop_mui_offset), 
                      ncol = nrow(wtp_pop_mui_offset))


# compute WTP for conversion of all land available in each cell
wtp <- hh_size * mui_weights * (coef_country + dist_matrix * coef_dist + coef_bird + coef_size * land_size)

# find max wtp for each household and from which cell it is generated
max_wtp_vals <- apply(wtp, 1, FUN=max)
max_wtp_cell <- apply(wtp, 1, FUN=which.max)

# for each cell, identify how much WTP is generated from the conversion
wtp_max <- wtp_pop_mui_offset[, c('new2kid', 'farmland_area')]
wtp_max$wtp <- 0
wtp_max <- wtp_max[, c(1, 2, 4, 3)]

for (i in unique(max_wtp_cell)){
  idx <- max_wtp_cell == i
  aggr_wtp <- sum(max_wtp_vals[idx])
  wtp_max$wtp[i] <- aggr_wtp
}

# reorder the cells based on decreasing WTP generated
wtp_max$wtp_ha <- wtp_max$wtp / (wtp_max$farmland_area / 1e4)
wtp_max$wtp_ha[is.na(wtp_max$wtp_ha)] <- 0
wtp_max$wtp_ha[is.infinite(wtp_max$wtp_ha)] <- 0
sort_idx <- sort(wtp_max$wtp_ha, decreasing = TRUE, index.return = TRUE)[[2]]
wtp_max <- wtp_max[sort_idx, ]
idx_positive_wtp <- sum(wtp_max$wtp_ha > 0)
reshuffle_vector <- c(idx_positive_wtp:nrow(wtp_max))
resampled_vector <- sample(reshuffle_vector)
wtp_max[reshuffle_vector, ] <- wtp_max[resampled_vector,]

# Tot offset area = tot area of new buildings
tot_offset <- sum(wtp_pop_mui_offset$area_new_builds)
offset_proj <- wtp_pop_mui_offset$area_new_builds[wtp_pop_mui_offset$area_new_builds > 0]
sort_idx <- sort(offset_proj, decreasing = TRUE, index.return = TRUE)[[2]]
offset_proj <- offset_proj[sort_idx]

allocated_land <- 0
i <- 1
j <- 1

while(round(allocated_land, 5) < round(tot_offset, 5)){
  offset_cell <- wtp_max$new2kid[i]
  offset_land <- offset_proj[j]
  avail_land <- wtp_max$farmland_area[i]
  idx <- which(wtp_pop_mui_offset$new2kid == offset_cell)
  if(avail_land > offset_land){
    wtp_pop_mui_offset$wtp_pop_mui_offset[idx] <- offset_land
    wtp_pop_mui_offset$farmland_area[idx] <- wtp_pop_mui_offset$farmland_area[idx] - offset_land
    offset_area_perc <- offset_land / 4e6
    wtp_pop_mui_offset$percent_frm[idx] <- wtp_pop_mui_offset$percent_frm[idx] - offset_area_perc
    wtp_pop_mui_offset$percent_grs[idx] <- wtp_pop_mui_offset$percent_grs[idx] + 0.5 * offset_area_perc
    wtp_pop_mui_offset$percent_wod[idx] <- wtp_pop_mui_offset$percent_wod[idx] + 0.5 * offset_area_perc
    allocated_land <- allocated_land + offset_land
    i <- i + 1
    j <- j + 1
  } else {
    i <- i + 1
  }
}

setwd('D:/Documents/GitHub/biodiversity-net-gain/CSV Files/bio_offset_landuses/delta_birds')
st_write(wtp_pop_mui_offset, 'wtp_pop_mui_offset.csv')




