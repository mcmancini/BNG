## Census.R
## ========
##
## Author: Mattia C. Mancini and Rebecca Collins
## Creaated: 21-Sep-2020
## Last modified: 04-June-2022
## ---------------------------------------------
##
## Script that takes census data and data on income for England and assigns it 
## to the SEER 2km spatial grid
## ===========================================================================


## (0) SET-UP
## ==========
rm(list=ls())
library(sf)
library(units)
library(RPostgres)
# library(rgdal)
# library(raster)
# library(rgeos)
# library(ggplot2)
# library(raster)
# library(geosphere)
# library(mapview)

## (1) LOAD AND PREPARE THE THE DATA
##     1.1 - LSOA 2011 spatial files
##     1.2 - MSOA spatial files
##     1.3 - Household data from Census 2011
##     1.4 - Population data from Census 2011
##     1.5 - Income data at the MSOA level
##     1.6 - SEER 2km Grid
##     1.7 - List of SEER 2km cell IDs in England
## ==============================================

#  set path for different machines 
path <- "C:/Code/BNG/"

## 1.1. Load the 2011 LSOA spatial files
##      These have been downloaded from 
##      https://census.ukdataservice.ac.uk/use-data/guides/boundary-data.aspx
## ---------------------------------------------------------------------------
setwd(paste0(path,'Data/Census/Boundaries_Census'))
lsoa <- st_read('LSOA_2011/LSOAExtent_of_Realm2011/LSOA_2011_EW_BFE.shp')[, 1]
lsoa$Area <- st_area(lsoa)
colnames(lsoa) <- c('LSOA11CD', 'geometry', 'Area')

## 1.2. MSOA to LSOA lookup table
##      These have been downloaded from
##      https://geoportal.statistics.gov.uk/datasets/fe6c55f0924b4734adf1cf7104a0173e_0/explore
## -----------------------------------------------------------------------------
msoa <- read.csv(paste0(path, "Data/Census/Boundaries_Census/Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District_(December_2017)_Lookup_with_Area_Classifications_in_Great_Britain.csv"))[, c(4, 8)]

## 1.3. Household data
##      This comes from QS406UK: https://www.nomisweb.co.uk/census/2011/qs406uk
## ----------------------------------------------------------------------------
setwd(paste0(path,'Data/Census/2011/'))
qs406 <- read.csv('QS406UK.csv')[, 3:4]
colnames(qs406) <- c('LSOA11CD', 'Households')
lsoa <- merge(lsoa, qs406, by='LSOA11CD')

## 1.4. Population data from Census 2011
##      This comes from QS112UK: https://www.nomisweb.co.uk/census/2011/qs112uk
## ----------------------------------------------------------------------------
qs112 <- read.csv('QS112UK.csv')[, 3:4]
colnames(qs112) <- c('LSOA11CD', 'Population')
lsoa <- merge(lsoa, qs112, by='LSOA11CD')

## 1.5. MSOA income 2018
##      This comes from OMS: 
##      https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales
## -----------------------------------------------------------------------------
setwd(paste0(path, "Data/SEER_SOCIO_ECON/ONS_Income"))
income <- read.csv('income_estimates_2018_EW.csv')[, c(1, 7)]
colnames(income) <- c('MSOA11CD', 'hh_income')
msoa <- merge(msoa, income, by = 'MSOA11CD')

## 1.6. Attach MSOA income to LSOA using look-uptable 
## --------------------------------------------------
lsoa <- dplyr::left_join(lsoa, msoa, by = "LSOA11CD" )
# set income to numeric 
lsoa$hh_income <- as.numeric(lsoa$hh_income)

## 1.7. Load the SEER 2km grid
## ---------------------------
setwd(paste0(path, 'Data/SEER_GRID/'))
seer_2km <- st_read('./SEER_net2km.shp')

## 1.8. List of SEER 2km cell IDs in England
## -----------------------------------------
conn <- dbConnect(Postgres(), 
                  dbname = "NEV",
                  host = "localhost",
                  port = 5432,
                  user="postgres",
                  password="postgres")

df <- dbGetQuery(conn, "SELECT * FROM regions_keys.key_grid_countries_england")
cell_id <- df$new2kid
seer_2km_eng <- seer_2km[seer_2km$new2kid %in% cell_id, 'new2kid']

rm(list = c('income', 'qs112', 'qs406', 'cell_id', 'seer_2km', 'df'))

# set coordinate system to same - both BNG but not recognized as the same 
lsoa <- lsoa %>% st_transform(crs = st_crs(seer_2km_eng))

## (2) ASSIGN DATA TO THE SEER 2km GRID
## ====================================

## 2.1. LSOA data
## --------------

# Spatial intersection
census_join <- st_intersection(seer_2km_eng, lsoa)
census_join$cell_area <- st_area(census_join)

# initialise census dataframe
census_eng <- seer_2km_eng[, 'new2kid']
census_eng$Population <- NA
census_eng$Population_density <- NA
census_eng$Households <- NA
census_eng$Med_hh_inc <- NA

# Calculate 2km cell values aggregating all LSOAs intersecting each 2km cell
# proportional to the intersection area
for (i in unique(census_join$new2kid)){
  df <- census_join[census_join$new2kid == i, ]
  population <- drop_units(sum(df$Population / df$Area * df$cell_area))
  pop_density <- population / 400 #population per hectare
  households <- drop_units(sum(df$Households / df$Area * df$cell_area))
  med_income <- round(drop_units(sum(df$hh_income * df$cell_area) / sum(df$cell_area)))
  census_eng[census_eng$new2kid == i, 'Population'] <- population
  census_eng[census_eng$new2kid == i, 'Population_density'] <- pop_density
  census_eng[census_eng$new2kid == i, 'Households'] <- households
  census_eng[census_eng$new2kid == i, 'Med_hh_inc'] <- med_income
}

## 2.2. MSOA data
## --------------
# income_join <- st_intersection(seer_2km_eng, msoa)
# income_join$cell_area <- st_area(income_join)
# 
# for (i in unique(income_join$new2kid)){
#   df <- income_join[income_join$new2kid == i, ]
#   med_income <- round(drop_units(sum(df$hh_income * df$cell_area) / sum(df$cell_area)))
#   census_eng[census_eng$new2kid == i, 'Med_hh_inc'] <- med_income
# }


## (3) write on disk
## =================
st_write(census_eng, paste0(path,'/Data/SEER_SOCIO_ECON/SEER_2k_socioecon_eng.csv'))
