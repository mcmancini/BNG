## calc_NEV_landuses.R
## ===================
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

## (1) LOAD REQUIRED DATA
##     1.1 - LCM_2020 land cover map on a 2km resolution (created in the script 
##           'process_lcm.R')
##     1.2 - SEER NEV 2km grid
## ===========================================================================

## 1.1. 2km LCM 2020
## -----------------
setwd('D:/Documents/OneDrive - University of Exeter/Github/BNG/Data/LCM/LCM_2km/')
lcm_2km <- read.csv('lcm_all_classes_2020.csv')

## 1.2. SEER 2km grid
## ------------------
setwd('D:/Documents/OneDrive - University of Exeter/Github/BNG/Data/SEER_GRID/')
seer_2km <- st_read('./SEER_net2km.shp')[, 'new2kid']

lcm_2km <- merge(seer_2km, lcm_2km, by='new2kid')

## (2) DATA PROCESSING
##     2.1 - Spatial join lcm and NEP
##     2.2 - Aggregation of lcm classes to match NEV land uses
## ===========================================================

## 2.1. Spatial join of NEP and 2km LCM
## ------------------------------------

# reproject NEP shapefile
nep <- st_transform(nep, "EPSG:27700")

# intersect SEER 2km grid and NEP to identify 2km cells within the NEP area
nep_2km <- st_intersection(seer_2km, nep)
cell_IDs <- unique(nep_2km$new2kid)
nep_2km <- seer_2km[seer_2km$new2kid %in% cell_IDs,]

# Crop lcm to match extent of NEP and remove NA values, if any
lcm_crop <- crop(lcm, nep_2km)
lcm_crop[lcm_crop < 1] <- NA

# convert cropped lcm raster to a dataframe of spatial coordinates and values
lcm_poly_df <- as.data.frame(cbind(xyFromCell(lcm_crop, 1:ncell(lcm_crop)), values(lcm_crop)))
colnames(lcm_poly_df) <- c('x', 'y', 'lcm_class')

# spatial intersection (for fun, done manually rather than using st_intersect. 
# In large datasets this is MUCH faster)
nep_lcm <- cbind(nep_2km, as.data.frame(matrix(0, nrow = nrow(nep_2km), ncol = 21)))
colnames(nep_lcm) <- c('new2kid', as.character(1:21), 'geometry')

for (i in nep_2km$new2kid){
  bbox <- st_bbox(nep_2km[nep_2km$new2kid == i,])
  idx <- which(lcm_poly_df$x >= bbox[1] &
                 lcm_poly_df$x < bbox[3] &
                 lcm_poly_df$y >= bbox[2] &
                 lcm_poly_df$y < bbox[4])
  tmp <- lcm_poly_df[idx, ]
  classes <- table(tmp$lcm_class)
  col_idx <- which(colnames(nep_lcm) %in% names(classes))
  nep_lcm[nep_lcm$new2kid == i, col_idx] <- classes
}

# check that land uses match areas after intersection
if(sum(rowSums(nep_lcm[,which(colnames(nep_lcm) %in% c(1:21)), drop=TRUE]) != 6400) > 0){
  warning('The area of all the 25m lcm cells within each 2km cell does not equal 400 hectares in all cases!!!')
}

# if there are any NAs, assign those to the class most represented (this is a
# bit convoluted but should work for both single or multiple cells having NAs)
# I am assuming that in almost all cases there won't be any NA values or only 
# a very few of them, to the extent that they won't significantly change the 
# proportions of land within any 2km cell (10 NAsin one 2km cell would be 0.15% 
# of the overall cell area). There should not be many NAs as the lcm has full 
# coverage of the UK. After these lines we should no longer get the warning
# from the code above.
which_na <- which(rowSums(nep_lcm[,which(colnames(nep_lcm) %in% c(1:21)), drop=TRUE]) != 6400)
num_na_vector <- 6400 - rowSums(nep_lcm[, which(colnames(nep_lcm) %in% c(1:21)), drop=TRUE])
num_na <- num_na_vector[num_na_vector != 0]
which_max <- which.max(nep_lcm[which_na, which(colnames(nep_lcm) %in% c(1:21)), drop=TRUE])
nep_lcm[which_na, which_max] <- nep_lcm[which_na, which_max, drop=TRUE] + num_na

## 2.2. Lcm agregation and conversion to hectares to match NEV input land uses.
##      - Coastal: lcm classes 15 to 19
##      - Freshwater : lcm class 14
##      - Marine: lcm class 13
##      - Ocean: not available here (we classify as ocean the areas outside lcm
##        coverage but within SEER 2km along coastlines)
##      - Woodland: lcm classes 1 and 2
##      - Arable: lcm class 3 
##      - Improved grassland: lcm class 4
##      - seminatural grassland: lcm classes 5 to 12 (I added also Mountain,
##        heath and bog here, which migh tnot be appropriate on a nationwide
##        scale, but works here as no Mountain, heath and bog exist in the NEP
##        area)
##      - Urban: lcm classes 20 and 21
## After the conversion check that the sum of all land uses for each cell equals 
## 400ha
## ----------------------------------------------------------------------------
nep_lcm_2020 <- nep_lcm[, 'new2kid']
nep_lcm_2020$imp_grs_ha <- nep_lcm[,which(colnames(nep_lcm) == 4), drop=TRUE] * 0.0625
nep_lcm_2020$sng_ha <- rowSums(nep_lcm[,which(colnames(nep_lcm) %in% c(5:12)), drop=TRUE]) * 0.0625
nep_lcm_2020$wood_ha <- rowSums(nep_lcm[, which(colnames(nep_lcm) %in% c(1:2)), drop=TRUE]) * 0.0625
nep_lcm_2020$urbn_ha <- rowSums(nep_lcm[, which(colnames(nep_lcm) %in% c(20:21)), drop=TRUE]) * 0.0625
nep_lcm_2020$arbl_ha <- nep_lcm[, which(colnames(nep_lcm) %in% c(3)), drop=TRUE] * 0.0625
nep_lcm_2020$frshwtr_ha <- nep_lcm[, which(colnames(nep_lcm) %in% c(14)), drop=TRUE] * 0.0625
nep_lcm_2020$marine_ha <- nep_lcm[, which(colnames(nep_lcm) %in% c(13)), drop=TRUE] * 0.0625
nep_lcm_2020$ocean_ha <- 0
nep_lcm_2020$coast_ha <- rowSums(nep_lcm[, which(colnames(nep_lcm) %in% c(15:19)), drop=TRUE]) * 0.0625

sum(rowSums(nep_lcm_2020[,3:11, drop=TRUE]) != 400) == 0

## (3) SAVE ON DISK
## ================
setwd('D:/Documents/GitHub/NERC--Agile-Sprint/Data/')
st_write(nep_lcm_2020, 'nep_baseline_lu.shp')
