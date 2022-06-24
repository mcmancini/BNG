## calc_urban_sprawl_Eigenbrod.R
## =============================
##
## Author: Mattia Mancini from F. Eigenbrod
## Created: 29-Apr-2021
## Last modified: 23-Jun-2021
## --------------------------
## 
## DESCRIPTION
##
## Script that takes the projections for urbanization estimated by F. Eigenbrod 
## and colleagues (Eigenbrod et al. 2011),  remaps them to the SEER 2km grid and 
## computes changes in land use on a 2km basis from 2007 to 2031, focusing on
## the new urban land that can be created on farmland by cell. The new amount
## of urban land by 2031 is then converted into number of houses based on the 
## housing densities of the LPAs in which each 2km cell falls. 
## We use the low density scenario and look at year 2031. As the scenarios do  
## not limit the categories of land where urbanisation can occur (e.g. 
## urbanisation can happen on woodland), we clip the projections to only  
## farmland. This allows us to maintain the requisite that the offset land must 
## produce more biodiversity than the land lost to development. The current  
## biodiveristy model used for this research would not allow us to create 
## additional biodiversity for example improving existing woodland, as it 
## focusses on macro land use changes (e.g. farmland to woodland).
## The output of this script contains 2031 land uses and housing densities, and 
## can be used as input to compute biodiversity changes from 2007 land uses
## =============================================================================

## (0) SETUP
## ========
rm(list=ls())

library(sf)
library(raster)
library(RPostgres)
library(dplyr)
library(units)
library(mapview)      # mapview visualisation for debugging
# library(ggplot2)
# library(viridis)      # scale_fill_viridis
library(gridExtra)    # grid_arrange
library(ggpubr)       # annotate_figure

source('D:/Documents/GitHub/BNG/R_scripts/Functions/fcn_plt_map.R')

## (1) LOAD THE DATA
## =================

## 1.1) Urban sprawl from F. Eigenbord, low density scenario
## ---------------------------------------------------------
setwd('D:/Documents/GitHub/BNG/Data/Urban Sprawl - F.Eigenbrod/')
urban_sprawl <- stack('TotalUrbanAndSuburban_2031_LowDensityScenario_1km.tif')
urban_sprawl <- st_as_sf(rasterToPoints(urban_sprawl, spatial = TRUE))
urban_sprawl <- st_set_crs(urban_sprawl, 27700)

## 1.2) Land use data from land cover map 2000
## -------------------------------------------
df <- read.csv('D:/Documents/GitHub/BNG/Data/LCM/LCM_2km/lcm_aggr_2000.csv')

## 1.3) 2km SEER grid
## ------------------
seer_2km <- st_read('D:/Documents/GitHub/BNG/Data/SEER_GRID/SEER_net2km.shp')
seer_2km <- seer_2km[, "new2kid"]


## 1.4) LPA boundaries
## -------------------
setwd("D:/Documents/GitHub/BNG/Data/LPA/")
lpa <- st_read("Local_Authority_Districts_April_2019_Boundaries_UK_BUC.shp")

## 1.5) Load the .csv file containing number of addresses per hectare for each 
##      LPA in 2018 and merge it to the lpa data.
##      England data: https://www.gov.uk/government/statistical-data-sets/live-tables-on-land-use-change-statistics
##      Wales data: https://statswales.gov.wales/Catalogue/Housing/Dwelling-Stock-Estimates/dwellingstockestimates-by-localauthority-tenure
##      Scotland data: https://www.gov.scot/publications/housing-statistics-stock-by-tenure/
##      England has data of dwellings per hectare; Scotland and Wales only have 
##      available data on number of dwellings by tenure per local planning
##      authority, which is what we use here, dividing the total numbers by the
##      area of each planning authority
## -----------------------------------------------------------------------------
setwd('D:/Documents/GitHub/BNG/Data/Housing/')

## ENGLAND
housing_eng <- read.csv('./P331_1516_addresses_ha_2018_England.csv')
## housing_eng: West Somerset and Taunton Deane to be merged into LPA Somerset West and Taunton;
##                  Christchurch, Poole and Bournemouth to be merged into Bournemouth, Christchurch and Poole
##                  East, North, West Dorset, Purbeck and Weymouth and Portland to be merged into Dorset
##                  Forest Heath and St Edmundsbury to be merged into West Suffolk 
##                  Suffolk Coastal and Waveney to merge in East Suffolk
##                  Shepway to be renamed Folkestone and Hythe
##                  As the old LPA codes not always match with the new ones, the merging later on
##                  will be done by lpa names.

## Somerset
old_somerset <- housing_eng[housing_eng$Local.authority == 'West Somerset' | 
                              housing_eng$Local.authority == 'Taunton Deane', ]
new_somerset <- data.frame(lpa[lpa$lad19nm == 'Somerset West and Taunton',c(1, 3)],
                           mean(old_somerset$addresses_ha))
new_somerset$geometry <- NULL
colnames(new_somerset) <- colnames(housing_eng)
housing_eng <- housing_eng[-which(housing_eng$Local.authority == 'West Somerset' | 
                                    housing_eng$Local.authority == 'Taunton Deane'),]
housing_eng <- rbind(housing_eng, new_somerset)
rm(list = c('old_somerset', 'new_somerset'))

## Bournemouth, Christchurch and Poole
BCP <- housing_eng[housing_eng$Local.authority == 'Bournemouth' | 
                     housing_eng$Local.authority == 'Christchurch' |
                     housing_eng$Local.authority == 'Poole', ]
new_BCP <- data.frame(lpa[lpa$lad19nm == 'Bournemouth, Christchurch and Poole',c(1, 3)],
                      mean(BCP$addresses_ha))
new_BCP$geometry <- NULL
colnames(new_BCP) <- colnames(housing_eng)
housing_eng <- housing_eng[-which(housing_eng$Local.authority == 'Bournemouth' | 
                                    housing_eng$Local.authority == 'Christchurch' |
                                    housing_eng$Local.authority == 'Poole'),]
housing_eng <- rbind(housing_eng, new_BCP)
rm(list = c('BCP', 'new_BCP'))

## Dorset
old_dorset <- housing_eng[housing_eng$Local.authority == 'East Dorset' | 
                            housing_eng$Local.authority == 'North Dorset' |
                            housing_eng$Local.authority == 'West Dorset' |
                            housing_eng$Local.authority == 'Purbeck' |
                            housing_eng$Local.authority == 'Weymouth and Portland', ]
new_dorset <- data.frame(lpa[lpa$lad19nm == 'Dorset',c(1, 3)],
                         mean(old_dorset$addresses_ha))
new_dorset$geometry <- NULL
colnames(new_dorset) <- colnames(housing_eng)
housing_eng <- housing_eng[-which(housing_eng$Local.authority == 'East Dorset' | 
                                    housing_eng$Local.authority == 'North Dorset' |
                                    housing_eng$Local.authority == 'West Dorset' |
                                    housing_eng$Local.authority == 'Purbeck' |
                                    housing_eng$Local.authority == 'Weymouth and Portland'),]
housing_eng <- rbind(housing_eng, new_dorset)
rm(list = c('old_dorset', 'new_dorset'))

## West Suffolk
old_WS <- housing_eng[housing_eng$Local.authority == 'Forest Heath' | 
                        housing_eng$Local.authority == 'St Edmundsbury',]
new_WS <- data.frame(lpa[lpa$lad19nm == 'West Suffolk',c(1, 3)],
                     mean(old_WS$addresses_ha))
new_WS$geometry <- NULL
colnames(new_WS) <- colnames(housing_eng)
housing_eng <- housing_eng[-which(housing_eng$Local.authority == 'Forest Heath' | 
                                    housing_eng$Local.authority == 'St Edmundsbury'),]
housing_eng <- rbind(housing_eng, new_WS)
rm(list = c('old_WS', 'new_WS'))

## East Suffolk
old_ES <- housing_eng[housing_eng$Local.authority == 'Suffolk Coastal' | 
                        housing_eng$Local.authority == 'Waveney',]
new_ES <- data.frame(lpa[lpa$lad19nm == 'East Suffolk',c(1, 3)],
                     mean(old_ES$addresses_ha))
new_ES$geometry <- NULL
colnames(new_ES) <- colnames(housing_eng)
housing_eng <- housing_eng[-which(housing_eng$Local.authority == 'Suffolk Coastal' | 
                                    housing_eng$Local.authority == 'Waveney'),]
housing_eng <- rbind(housing_eng, new_ES)
rm(list = c('old_ES', 'new_ES'))

## Shepway
housing_eng[housing_eng$Local.authority == 'Shepway', which(colnames(housing_eng) == 'Local.authority')] <- 'Folkestone and Hythe'

housing_eng <- housing_eng[, c(2:3)]
colnames(housing_eng) <- c('lad19nm', 'addresses_ha')

## WALES
housing_wal <- read.csv('Dwelling_stock_2018_Wales.csv')[, c(1, 5)]
colnames(housing_wal) <- c('lad19nm', 'addresses_ha')
housing_wal <- merge(housing_wal, lpa[, c('lad19nm', 'st_areasha'), drop=TRUE], by='lad19nm')
housing_wal$st_areasha <- housing_wal$st_areasha / 1e4
housing_wal$addresses_ha <- housing_wal$addresses_ha / housing_wal$st_areasha
housing_wal <- housing_wal[, -which(colnames(housing_wal) == 'st_areasha')]

## SCOTLAND
housing_sco <- read.csv('Dwelling_stock_2018_Scotland.csv')
colnames(housing_sco) <- c('lad19nm', 'addresses_ha')
housing_sco$addresses_ha <- housing_sco$addresses_ha * 1e3
housing_sco <- merge(housing_sco, lpa[, c('lad19nm', 'st_areasha'), drop=TRUE], by='lad19nm')
housing_sco$st_areasha <- housing_sco$st_areasha / 1e4
housing_sco$addresses_ha <- housing_sco$addresses_ha / housing_sco$st_areasha
housing_sco <- housing_sco[, -which(colnames(housing_sco) == 'st_areasha')]

housing_uk <- rbind(housing_eng, housing_sco, housing_wal)

housing_density <- merge(lpa[, 3], housing_uk, by='lad19nm')


## (2) ASSIGN ESTIMATED 1KM URBAN LAND IN 2031 FROM EIGENBROD ET ALL 2011 TO 
##     SEER 2KM GRID
## =========================================================================
sprawl_ctrds <- st_centroid(urban_sprawl)
sprawl_2km <- st_join(seer_2km, sprawl_ctrds) %>% 
  group_by(new2kid) %>% 
  summarise(ha_2031 = 400 * mean(TotalUrbanAndSuburban_2031_LowDensityScenario_1km))

#remove NAs: 4 cells inside 4 lakes/lochs in Scotland
sprawl_2km$ha_2031[is.na(sprawl_2km$ha_2031)] <- 0

# merge with LCM 2000 urban land
sprawl_2km <- merge(sprawl_2km, df, by='new2kid')

## (3) CALCULATE HECTARE CHANGE IN URBAN AREAS FROM 2000 to 2031
## =============================================================
sprawl_2km$ha_change <- round(sprawl_2km$ha_2031 - sprawl_2km$urban_ha, 4)
sprawl_2km <- sprawl_2km[, c(1, 2, 3, 5, 4)]

## (4) COMPUTE AREA OF NEW BUILDS BY CELL AND ADJUST LAND USE
## ==========================================================

## 4.1) Join land uses SEER 2km data. This requires some assumptions, as NEV
##      uses the LCM2007 for landuses, whereas the urbanisation model uses the 
##      LCM2000. For this reason, and to calculate biodiversity changes from
##      urbanisation, we take the urban land in 2000 and reassign it to the data
##      from LCM2007. The difference between the two (likely more urban land in
##      2007) will be assigned to farmland such that the totals will still be 
##      400 hectares per cell
## ----------------------------------------------------------------------------- 

# land uses
seer_2km <- merge(seer_2km, df, by='new2kid')
seer_2km$urban_2000 <- sprawl_2km$urban_ha / 400

# set percentage urban to be equal to lcm2000, and add or remove the difference 
# from farmland
seer_2km$percent_urb <- seer_2km$urban_2000
seer_2km$percent_frm <- 1 - rowSums(seer_2km[, 2:5, drop=TRUE])
seer_2km <- seer_2km[, -which(colnames(seer_2km) == 'urban_2000')]

# doing step in line 254 results in negative farmland for 144 cells: set
# farmland to 0 and reassign that difference to woodland if available, otherwise
# to semi-natural grassland
ind <- which(seer_2km$percent_frm < 0)
for (i in ind){
  df <- seer_2km[i, ]
  if (df$percent_wod >= df$percent_frm){
    seer_2km$percent_wod[i] <- seer_2km$percent_wod[i] + seer_2km$percent_frm[i]
    seer_2km$percent_frm[i] <- 0
  } else if (df$percent_grs >= df$percent_frm){
    seer_2km$percent_grs[i] <- seer_2km$percent_grs[i] + seer_2km$percent_frm[i]
    seer_2km$percent_frm[i] <- 0
  }
}

## Save on disk to use as baseline for biodiversity calculations
setwd('D:/Documents/Housing/Output')
st_write(seer_2km, 'seer_landuses_2000.csv')

## 4.2) Adjust land uses from urbanisation
## ---------------------------------------
seer_2km$farmland_area <- seer_2km$percent_frm * 400 * 1e4
seer_2km$area_new_builds <- 0
new_urban_ids <- sprawl_2km$new2kid[sprawl_2km$ha_change > 0]

for (i in new_urban_ids){
  frm_needed <- sprawl_2km[sprawl_2km$new2kid %in% i, 'ha_change', drop=T] * 1e4
  frm_avail <- seer_2km[seer_2km$new2kid %in% i, 'farmland_area', drop=T]
  perc_urban_t0 <- seer_2km$percent_urb[seer_2km$new2kid %in% i]
  
  if (frm_avail >= frm_needed){
    seer_2km$area_new_builds[seer_2km$new2kid %in% i] <- frm_needed
    frm_needed_perc <- frm_needed / (400 * 1e4)
    new_frm <- frm_avail - frm_needed
    new_frm_perc <- new_frm / (400 * 1e4)
    seer_2km$farmland_area[seer_2km$new2kid %in% i] <- new_frm
    seer_2km$percent_frm[seer_2km$new2kid %in% i] <- new_frm_perc
    seer_2km$percent_urb[seer_2km$new2kid %in% i] <- perc_urban_t0 + frm_needed_perc
  } else {
    farm_avail_perc <- seer_2km$percent_frm[seer_2km$new2kid %in% i]
    seer_2km$area_new_builds[seer_2km$new2kid %in% i] <- frm_avail
    seer_2km$farmland_area[seer_2km$new2kid %in% i] <- 0
    seer_2km$percent_frm[seer_2km$new2kid %in% i] <- 0
    seer_2km$percent_urb[seer_2km$new2kid %in% i] <- perc_urban_t0 + farm_avail_perc
  }
}

## (5) COMPUTE NUMBER OF NEW HOUSES BASED ON LPA HOUSING DENSITIES
## ===============================================================
housing_density <- st_buffer(housing_density, dist = 0)
housing_2km <- st_intersection(seer_2km, housing_density)
housing_2km$intsc_area <- drop_units(st_area(housing_2km) / 1e4)

seer_2km$lad19nm <- NA
seer_2km$addresses_ha <- NA

for (i in unique(housing_2km$new2kid)){
  df <- housing_2km[housing_2km$new2kid == i, ]
  addresses_ha <- df$addresses_ha %*% df$intsc_area / 400
  largest_lpa <- df[which.max(df$intsc_area), 'lad19nm', drop=TRUE]
  seer_2km$lad19nm[seer_2km$new2kid == i] <- largest_lpa
  seer_2km$addresses_ha[seer_2km$new2kid == i] <- addresses_ha
}

setwd('D:/Documents/Housing/Output')
st_write(seer_2km, 'seer_landuses_2031.csv')


## (4) MAP THE DATA
## ================

## 4.1. Hectares in 2000 and 2031 -- GREAT BRITAIN
## -----------------------------------------------
base <- sprawl_2km
plt <- fcn_continuous_plot(plot_data = base[base$ha_2031 > 0,], 
                           country = 'Great Britain', 
                           column = 'ha_2031',
                           limits = c(0, 400),
                           plot_title = 'Hectares of urban land by 2031',
                           legend_title = 'Urban land\n[hectares]', 
                           legend_position = 'none',
                           scale = 'magma', 
                           direction = -1)

plt_base <- fcn_continuous_plot(plot_data = base[base$urban_ha > 0, ],
                                country = 'Great Britain', 
                                column = 'urban_ha',
                                limits = c(0, 400),
                                plot_title = 'Hectares of urban land in 2000',
                                legend_title = 'Urban land\n[hectares]', 
                                legend_position = 'none', 
                                scale = 'magma',
                                direction = -1)

base <- seer_2km[seer_2km$area_new_builds > 0,]
base$area_new_builds <- base$area_new_builds / 1e4
plt_loss <- fcn_continuous_plot(plot_data = base,
                                country = 'Great Britain', 
                                 column = 'area_new_builds',
                                 limits = c(0, 400),
                                 plot_title = 'Farmland loss by 2031',
                                 legend_title = 'Farmland loss\n[hectares]',
                                 legend_position = 'none',
                                 scale = 'magma',
                                 direction = -1)

figure <- ggarrange(plt_base, plt, plt_loss,
                    ncol = 3, nrow = 1,
                    common.legend = TRUE,
                    legend = 'bottom')

plot_title <- 'Urban areas in Great Britain'
figure <- annotate_figure(figure, 
                          top = text_grob(plot_title, 
                                          color = "black", 
                                          face = "bold", 
                                          size = 32))
save_path <- 'D:/Documents/Housing/Output/Maps/'
filename <- 'urban_sprawl_GB.jpeg'
ggsave(filename=filename, plot = figure, device = "jpeg",
       path = save_path, units = "in", width = 22, height = 16)


## 4.2. Hectares in 2000 and 2031 -- ENGLAND
## -----------------------------------------
base <- sprawl_2km
eng_cells <- dbGetQuery(conn, "SELECT * FROM regions_keys.key_grid_countries_england")
base <- base[base$new2kid %in% eng_cells$new2kid, ]

plt <- fcn_continuous_plot(plot_data = base[base$ha_2031 > 0,], 
                           country = 'England', 
                           column = 'ha_2031',
                           limits = c(0, 400),
                           plot_title = 'Hectares of urban land by 2031',
                           legend_title = 'Urban land\n[hectares]', 
                           legend_position = 'none',
                           scale = 'magma', 
                           direction = -1)

plt_base <- fcn_continuous_plot(plot_data = base[base$urban_ha > 0, ], 
                                country = 'England',
                                column = 'urban_ha',
                                limits = c(0, 400),
                                plot_title = 'Hectares of urban land in 2000',
                                legend_title = 'Urban land\n[hectares]', 
                                legend_position = 'none', 
                                scale = 'magma',
                                direction = -1)

base <- seer_2km[seer_2km$area_new_builds > 0,]
base <- base[base$new2kid %in% eng_cells$new2kid, ]
base$area_new_builds <- base$area_new_builds / 1e4
plt_loss <- fcn_continuous_plot(plot_data = base, 
                                country = 'England',
                                column = 'area_new_builds',
                                limits = c(0, 400),
                                plot_title = 'Farmland loss by 2031',
                                legend_title = 'Farmland loss\n[hectares]',
                                legend_position = 'none',
                                scale = 'magma',
                                direction = -1)

figure <- ggarrange(plt_base, plt, plt_loss,
                    ncol = 3, nrow = 1,
                    common.legend = TRUE,
                    legend = 'bottom')

plot_title <- 'Urban areas in England'
figure <- annotate_figure(figure, 
                          top = text_grob(plot_title, 
                                          color = "black", 
                                          face = "bold", 
                                          size = 32))
save_path <- 'D:/Documents/Housing/Output/Maps/'
filename <- 'urban_sprawl_ENG.jpeg'
ggsave(filename=filename, plot = figure, device = "jpeg",
       path = save_path, units = "in", width = 22, height = 11)
