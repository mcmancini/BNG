## process_lcm_to_SEER.R
## =====================
##
## Author: Mattia Mancini
## Created: 20-Jan-2022
## Last modified: 24-Feb-2022
## --------------------------
##
## DESCRIPTION
## 
## Script that takes the 25m raster data contained in the CEH Land Cover Maps
## for the UK and uses it to assign the proportion of land in each class to each
## of the SEER 2km grid cells in the UK.
## This can then be used to compute aggregate land uses to pass to the NEV farm
## model to estimate detailed arable/livestock/grassland allocation for
## farmland, as a baseline to estimate land use change from urbanisation,
## biodiversity changes and so on.
## N.B.: the output of this script is a file containing, for each lcm class, the 
## number of 25m cells contained in each SEER 2km cell. The aggregation of 
## classes into NEV land uses is NOT done within this script, to allow the 
## exploration of different aggregations. 
## =============================================================================

## (0) SETUP
## =========
rm(list=ls())
library(sf)
library(raster)
library(dplyr)

memory.limit(size = 256000)
options(future.globals.maxSize= 128849018880)

## (1) INPUT SPECIFICATION
## =======================
lcm_year <- 2020
lcm_folder <- paste('D:/Documents/OneDrive - University of Exeter/Github/BNG/Data/LCM/LCM_', lcm_year, "/", sep =  "")
SEER_folder <- 'D:/Documents/OneDrive - University of Exeter/Github/BNG/Data/SEER_GRID/'

## (2) LOAD THE DATA
## =================

## 2.1) 2km SEER grid
## ------------------
seer_2km <- st_read(paste(SEER_folder, 'SEER_net2km.shp', sep=""))
seer_2km <- seer_2km[, "new2kid"]

## 2.3) LCM2000 to SEER 2km
## ------------------------
if (lcm_year == 2000){
  lcm_filename <- "gb25_r1.tif"
} else if (lcm_year == 2007){
  lcm_filename <- "lcm2007gb25m.tif"
} else if (lcm_year == 2015){
  lcm_filename <- "lcm2015gb25m.tif"
} else if (lcm_year == 2019){
  lcm_filename <- "gb2019lcm25m.tif"
} else if (lcm_year == 2020){
  lcm_filename <- "gb2020lcm25m.tif"
} else {
  stop("Available LCM data only for years 2000, 2007, 2015, 2019, 2020")
}

# Load LCM
lcm <- raster(paste(lcm_folder, lcm_filename, sep=""))

# Crop to extent of SEER grid
lcm_crop <- crop(lcm, seer_2km)

start_time <- Sys.time()
if (lcm_year == 2000){
  lcm_crop[lcm_crop < 11] <- NA # ~15 minutes to run!
} else if (lcm_year == 2007){
  lcm_crop[lcm_crop < 1] <- NA # ~15 minutes to run!
} else if (lcm_year == 2015){
  lcm_crop[lcm_crop < 1] <- NA # ~15 minutes to run!
} else if (lcm_year == 2019){
  lcm_crop[lcm_crop < 1] <- NA # ~15 minutes to run!
} else if (lcm_year == 2020){
  lcm_crop[lcm_crop < 1] <- NA # ~15 minutes to run!
} else {
  stop("Available LCM data only for years 2000, 2007, 2015, 2019, 2020")
}
end_time <- Sys.time()
end_time - start_time

# convert raster to dataframe containing coordinates and values of each lcm cell
start_time <- Sys.time()
lcm_poly_df <- as.data.frame(cbind(xyFromCell(lcm_crop, 1:ncell(lcm_crop)), values(lcm_crop)))
colnames(lcm_poly_df) <- c('x', 'y', 'lcm_class')
end_time <- Sys.time()
end_time - start_time

# Initialise a vector file containing lcm data aggregated at 2km resolution
if(lcm_year == 2000){
  lcm_2km <- cbind(seer_2km, as.data.frame(matrix(0, nrow = nrow(seer_2km), ncol = 26)))
  colnames(lcm_2km) <- c('new2kid', 11, 21, 41, 42, 43, 51, 52, 61, 71, 81, 91,
                         101, 102, 111, 121, 131, 151, 161, 171, 172, 181, 191, 
                         201, 211, 212, 221, 'geometry')
} else if (lcm_year == 2007){
  lcm_2km <- cbind(seer_2km, as.data.frame(matrix(0, nrow = nrow(seer_2km), ncol = 23)))
  colnames(lcm_2km) <- c('new2kid', c(1:23), 'geometry')
} else if (lcm_year == 2015){
  lcm_2km <- cbind(seer_2km, as.data.frame(matrix(0, nrow = nrow(seer_2km), ncol = 21)))
  colnames(lcm_2km) <- c('new2kid', c(1:21), 'geometry')
} else if (lcm_year == 2019){
  lcm_2km <- cbind(seer_2km, as.data.frame(matrix(0, nrow = nrow(seer_2km), ncol = 21)))
  colnames(lcm_2km) <- c('new2kid', c(1:21), 'geometry')
} else if (lcm_year == 2020){
  lcm_2km <- cbind(seer_2km, as.data.frame(matrix(0, nrow = nrow(seer_2km), ncol = 21)))
  colnames(lcm_2km) <- c('new2kid', c(1:21), 'geometry')
} else {
  stop("Available LCM data only for years 2000, 2007, 2015, 2019, 2020")
}

# Assign 25m rasters to 2km grid (AAA: 2.8 days!!!!!).I attempted to parallelise
# the loop using future_lapply in a multisession but on a small sample of data 
# it was slower than the loop. The advantage of such this loop is that it can be
# stopped at any time without losing data, and can be started where if was
# interrupted keeping track of i or counter. Potentially many instances can also 
# be run on different cores sending subsets of the whole datasets to each 
# instance (be careful to properly save the processed data in this case)
gc()
start_time <- Sys.time()
counter <- 1
to_remove <- c()
for (i in seer_2km$new2kid){
  cat(sprintf("Processing SEER 2km cell %s of 57230\n", counter))
  bbox <- st_bbox(seer_2km[seer_2km$new2kid == i,])
  idx <- which(lcm_poly_df$x >= bbox[1] &
               lcm_poly_df$x < bbox[3] &
               lcm_poly_df$y >= bbox[2] &
               lcm_poly_df$y < bbox[4])
  tmp <- lcm_poly_df[idx, ]
  classes <- table(tmp$lcm_class)
  col_idx <- which(colnames(lcm_2km) %in% names(classes))
  lcm_2km[lcm_2km$new2kid == i, col_idx] <- classes
  to_remove <- c(to_remove, idx)
  if (counter %% 100 == 0){
    lcm_poly_df <- lcm_poly_df[-to_remove,]
    to_remove <- c()
  }
  counter <- counter + 1
}
end_time <- Sys.time()
end_time - start_time

# save on disk
filename <- paste('D:/Documents/OneDrive - University of Exeter/Github/BNG/Data/LCM/LCM_2km/lcm_all_classes_', lcm_year, '.csv', sep="")
st_write(lcm_2km, filename)