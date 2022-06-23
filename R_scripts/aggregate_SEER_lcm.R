## aggregate_SEER_lcm.R
## ====================
##
## Author: Mattia Mancini
## Created: 23-Jun-2022
## Last modified: 23-Jun-2022
## --------------------------
##
## DESCRIPTION
## 
## Script that takes the land cover maps on the SEER 2km grid obtained from the 
## script 'process_lcm_to_SEER.r' and aggregates classes in order to obtain the 
## five aggregated land uses used in NEV: farmland, seminatural grassland,
## urban, woodland and water
## =============================================================================

## (0) SETUP
## =========
rm(list=ls())
library(sf)
library(dplyr)


## (1) INPUT SPECIFICATION
## =======================
lcm_year <- 2020
lcm_folder <- 'D:/Documents/GitHub/BNG/Data/LCM/LCM_2km/'
SEER_folder <- 'D:/Documents/GitHub/BNG/Data/SEER_GRID/'

## (2) LOAD THE DATA
## =================

## 2.1) 2km SEER grid
## ------------------
seer_2km <- st_read(paste(SEER_folder, 'SEER_net2km.shp', sep=""))
seer_2km <- seer_2km[, "new2kid"]

## 2.2) 2km LCM
## ------------------------
lcm_2km <- read.csv(paste0(lcm_folder, 'lcm_all_classes_', lcm_year, '.csv'), check.names = FALSE)

## 2.3) Merge Seer grid and lcm
## ----------------------------
lcm_2km <- merge(seer_2km, lcm_2km, by='new2kid')

# Initialise a vector file containing lcm data aggregated at 2km resolution
lcm_aggr <- cbind(seer_2km, as.data.frame(matrix(0, nrow = nrow(seer_2km), ncol = 5)))
colnames(lcm_aggr) <- c('new2kid', 'farm_ha', 'wood_ha', 'sng_ha', 'urban_ha', 
                        'water_ha', 'geometry')

if(lcm_year == 2000){
  lcm_aggr$farm_ha <- as.numeric(rowSums(lcm_2km[,which(colnames(lcm_2km) %in% c(41:43,51,52)), drop=TRUE]) * 0.0625)
  lcm_aggr$water_ha <- as.numeric(rowSums(lcm_2km[,which(colnames(lcm_2km) %in% c(131,221,201,211,212,181,191)), drop=TRUE]) * 0.0625)
  lcm_aggr$sng_ha <- as.numeric(rowSums(lcm_2km[,which(colnames(lcm_2km) %in% c(101,102,121,151,61,71,81,91,111,161)), drop=TRUE]) * 0.0625)
  lcm_aggr$urban_ha <- as.numeric(rowSums(lcm_2km[,which(colnames(lcm_2km) %in% c(171,172)), drop=TRUE]) * 0.0625)
  lcm_aggr$wood_ha <- as.numeric(rowSums(lcm_2km[,which(colnames(lcm_2km) %in% c(11,21)), drop=TRUE]) * 0.0625)
} else if (lcm_year == 2007){
  lcm_aggr$farm_ha <- as.numeric(rowSums(lcm_2km[,which(colnames(lcm_2km) %in% c(3,4)), drop=TRUE]) * 0.0625)
  lcm_aggr$sng_ha <- as.numeric(rowSums(lcm_2km[,which(colnames(lcm_2km) %in% c(5:14)), drop=TRUE]) * 0.0625)
  lcm_aggr$wood_ha <- as.numeric(rowSums(lcm_2km[, which(colnames(lcm_2km) %in% c(1:2)), drop=TRUE]) * 0.0625)
  lcm_aggr$urban_ha <- as.numeric(rowSums(lcm_2km[, which(colnames(lcm_2km) %in% c(22:23)), drop=TRUE]) * 0.0625)
  lcm_aggr$water_ha <- as.numeric(rowSums(lcm_2km[, which(colnames(lcm_2km) %in% c(15:21)), drop=TRUE]) * 0.0625)
} else if (lcm_year == 2015 | lcm_year == 2019 | lcm_year == 2020){
  lcm_aggr$farm_ha <- as.numeric(rowSums(lcm_2km[,which(colnames(lcm_2km) %in% c(3,4)), drop=TRUE]) * 0.0625)
  lcm_aggr$sng_ha <- as.numeric(rowSums(lcm_2km[,which(colnames(lcm_2km) %in% c(5:12)), drop=TRUE]) * 0.0625)
  lcm_aggr$wood_ha <- as.numeric(rowSums(lcm_2km[, which(colnames(lcm_2km) %in% c(1:2)), drop=TRUE]) * 0.0625)
  lcm_aggr$urban_ha <- as.numeric(rowSums(lcm_2km[, which(colnames(lcm_2km) %in% c(20,21)), drop=TRUE]) * 0.0625)
  lcm_aggr$water_ha <- as.numeric(rowSums(lcm_2km[, which(colnames(lcm_2km) %in% c(13:19)), drop=TRUE]) * 0.0625)
} else {
  stop("Available LCM data only for years 2000, 2007, 2015, 2019, 2020")
}

# assume water to be all empty cells
uk_lcm_aggr <- lcm_aggr %>% 
  mutate(ocean_ha = 400 - (farm_ha + sng_ha + wood_ha + urban_ha + water_ha))


# check how many 2km cells have ocean 
uk_lcm_aggr %>% filter(ocean_ha > 0 ) %>% tally() # 2811 2km cells with ocean 

# add ocean to water
uk_lcm_aggr$water_ha <- uk_lcm_aggr$water_ha + uk_lcm_aggr$ocean_ha
uk_lcm_aggr <- uk_lcm_aggr[, -which(colnames(uk_lcm_aggr) %in% 'ocean_ha')]

# check all 2km cells = 400 
sum(rowSums(uk_lcm_aggr[,2:6, drop=TRUE]) != 400) == 0 


## (3) SAVE ON DISK
## ================
filename = paste0(lcm_folder, 'lcm_aggr_', lcm_year, '.csv')
st_write(uk_lcm_aggr, filename)
