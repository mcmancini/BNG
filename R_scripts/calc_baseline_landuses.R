## calc_baseline_landuses.R
## ========================
##
## Author: Mattia Mancini
## Created: 24-Feb-2022
## Last modified: 24-Feb-2022
## --------------------------
##
## DESCRIPTION
## 
## Script to retrieve and aggregate land uses that are suitable to be passed to 
## the NEV modelling suite to compute changes in ecosystem services and their 
## values. These land uses can be either baseline landuses (e.g. from land cover
## maps) or scenario land uses, e.g. new land uses from predictions of urban
## sprawl.
## =============================================================================

## (0) SETUP
## =========
rm(list=ls())
library(sf)
library(mapview)

path <- "C:/Users/rmc230/OneDrive - University of Exeter/Documents/GitHub/BNG"

## (1) LOAD REQUIRED DATA
##     1.1 - LCM_2007 land cover map on a 2km resolution (created in the script 
##           'process_lcm.R')
##     1.2 - SEER NEV 2km grid
## ===========================================================================

## 1.1. 2km LCM 2007
## -----------------
setwd(paste0(path, "/Data/LCM/LCM_2km"))
lcm_2km <- read.csv('lcm_all_classes_2007.csv', check.names = FALSE)

## 1.2. SEER 2km grid
## ------------------
setwd(paste0(path, "/Data/SEER_GRID"))
seer_2km <- st_read('./SEER_net2km.shp')[, 'new2kid']

lcm_2km <- merge(seer_2km, lcm_2km, by='new2kid')

## (2) DATA PROCESSING
##     2.1 - Aggregation of lcm classes to match NEV land uses
## ===========================================================

## 2.1. Lcm agregation and conversion to hectares to match NEV input land uses.
##      - improved grass: lcm classes 4
##      - Coastal: lcm classes 17 to 21
##      - Freshwater : lcm class 16
##      - Marine: lcm class 15
##      - Ocean: not available here (we classify as ocean the areas outside lcm
##        coverage but within SEER 2km along coastlines)
##      - Woodland: lcm classes 1 and 2
##      - Arable: lcm class 3 
##      - Improved grassland: lcm class 4
##      - seminatural grassland: lcm classes 5 to 14 (I added also Mountain,
##        heath and bog here, which migh tnot be appropriate on a nationwide
##        scale, but works here as no Mountain, heath and bog exist in the NEP
##        area)
##      - Urban: lcm classes 22 and 23
## After the conversion check that the sum of all land uses for each cell equals 
## 400ha
## ----------------------------------------------------------------------------

lcm_2007 <- lcm_2km[, 'new2kid']
lcm_2007$grass_ha <- as.numeric(lcm_2km[,which(colnames(lcm_2km) == 4), drop=TRUE] * 0.0625)
lcm_2007$sng_ha <- as.numeric(rowSums(lcm_2km[,which(colnames(lcm_2km) %in% c(5:14)), drop=TRUE]) * 0.0625)
lcm_2007$wood_ha <- as.numeric(rowSums(lcm_2km[, which(colnames(lcm_2km) %in% c(1:2)), drop=TRUE]) * 0.0625)
lcm_2007$urban_ha <- as.numeric(rowSums(lcm_2km[, which(colnames(lcm_2km) %in% c(22:23)), drop=TRUE]) * 0.0625)
lcm_2007$arable_ha <- as.numeric(lcm_2km[, which(colnames(lcm_2km) %in% c(3)), drop=TRUE] * 0.0625)
lcm_2007$freshwater_ha <- as.numeric(lcm_2km[, which(colnames(lcm_2km) %in% c(16)), drop=TRUE] * 0.0625)
lcm_2007$marine_ha <- as.numeric(lcm_2km[, which(colnames(lcm_2km) %in% c(15)), drop=TRUE] * 0.0625)
lcm_2007$coast_ha <- as.numeric(rowSums(lcm_2km[, which(colnames(lcm_2km) %in% c(17:21)), drop=TRUE]) * 0.0625)

# assume ocean to be all empty cells

uk_lcm_2007 <- lcm_2007 %>% 
  mutate(ocean_ha = 400 - (grass_ha + sng_ha + wood_ha + urban_ha + arable_ha + freshwater_ha + marine_ha + coast_ha))

# check how many 2km cells have ocean 
uk_lcm_2007 %>% filter(ocean_ha > 0 ) %>% tally() # 2811 2km cells with ocean 

# check all 2km cells = 400 
sum(rowSums(uk_lcm_2007[,3:11, drop=TRUE]) != 400) == 0 

## (3) SAVE ON DISK
## ================
setwd(paste0(path, "/Data/LCM/LCM_2km"))
st_write(uk_lcm_2007, 'uk_baseline_lu_2007.shp')

