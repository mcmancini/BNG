## map_offset_locations.R
## ======================
##
## Author: Mattia C. Mancini and Rebecca Collins
## Created: 22-May-2021
## Last modified: 16-August-2022
## ---------------------------------------------
##
## DESCRIPTION
##
## Script that takes as input the spatial files created with the script
## 'calc_bio_offset.R' and outputs maps. The data mapped represents the
## locations selected to offset the biodiversity loss from urban development. 
## The criteria based on which offset locations are selected are the following:
##   1. Local offset
##   2. Offset in locations that generate the highest biodiversity improvements
##   3. Offset in locations that generate the highest ecosystem services
##   4. Offset in locations that generate the highest equity weighted recreation 
## =============================================================================

## (0) SETUP
## =========
rm(list=ls())
library(sf)
library(gridExtra)    # grid_arrange
library(ggpubr)       # annotate_figure
library(RPostgres)

#update path for different machines 
gitpath <- "D:\\Documents\\GitHub\\BNG\\" 
datapath <- "D:\\Documents\\OneDrive - University of Exeter\\Data\\BNG\\"


source(paste0(gitpath, '/R_scripts/Functions/fcn_plt_map.R'))

## (1) LOAD THE DATA
##     1.1. - SEER 2km grid
##     1.2. - Offset locations, local offset
##     1.3. - Offset locations, max biodiversity gains
##     1.4. - Offset locations, max ecosystem services
##     1.5. - Offset locations, equity weighted for recreation 
##     1.6. - Offset locations, min cost
## ================================================================

## 1.1. Seer 2km grid
## ------------------
setwd(paste0(datapath, "Data/SEER_GRID/")) 
seer_2km <- st_read('./SEER_net2km.shp')
seer_2km <- seer_2km[, "new2kid"]

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
# Eng_2kid <- read.csv(paste0(gitpath, "Output/England_new_2kid.csv"))
# 
# seer_2km <- seer_2km %>% 
#   dplyr::filter(new2kid %in% Eng_2kid$new2kid)

## 1.2. Offset locations, local offset
## -----------------------------------
setwd(paste0(gitpath,'Output/'))
local_bio_offset <- read.csv('local_bio_offset_sng.csv')
local_bio_offset <- merge(seer_2km, local_bio_offset, by='new2kid')

## 1.3. Offset locations, max biodiversity gains
## ---------------------------------------------
max_bio_offset <- read.csv('max_bio_offset_sng.csv') # all services
# max_bio_offset <- read.csv('max_es_rec_ghg_only_offset_urban_sprawl_sng.csv') # flooding and water quality excluded
max_bio_offset <- merge(seer_2km, max_bio_offset, by='new2kid')

## 1.4. Offset locations, max ecosystem services
## ---------------------------------------------
max_es_offset <- read.csv('max_es_offset_sng.csv')
max_es_offset <- merge(seer_2km, max_es_offset, by='new2kid')

## 1.5. Offset locations, max net ecosystem services
## ---------------------------------------------
max_netES_offset <- read.csv('max_netES_offset_sng.csv')
max_netES_offset <- merge(seer_2km, max_netES_offset, by='new2kid')

## 1.6. Offset locations, equity weighted recreation
## -------------------------------------------------
rec_mui_offset <- read.csv('max_equity_offset_sng.csv')
rec_mui_offset <- merge(seer_2km, rec_mui_offset, by='new2kid')

## 1.7. Offset locations, equity weighted recreation
## -------------------------------------------------
min_cost_offset <- read.csv('min_cost_offset_sng.csv')
min_cost_offset <- merge(seer_2km, min_cost_offset, by='new2kid')


## (2) MAPS
## ========

## 2.1. Offset locations, local offset
## -----------------------------------
df <- local_bio_offset

local_bio <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                                 column = 'offset_area_ha', 
                                 limits = c(0, 300),
                                 plot_title = 'a', 
                                 legend_title = '',
                                 legend_position = 'bottom', 
                                 scale = 'magma', 
                                 direction = -1)

## 2.2. Offset locations, max biodiversity gains
## ---------------------------------------------
df <- max_bio_offset

max_bio <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                               column = 'offset_area_ha', 
                               limits = c(0, 300),
                               plot_title = 'b', 
                               legend_title = 'Offset area',
                               legend_position = 'none', 
                               scale = 'magma', 
                               direction = -1)

## 2.3. Offset locations, max ES
## -----------------------------
df <- max_es_offset
max_es <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                              column = 'offset_area_ha', 
                              limits = c(0, 300),
                              plot_title = 'c',
                              legend_title = 'Offset area',
                              legend_position = 'none', 
                              scale = 'magma', 
                              direction = -1)

## 2.3. Offset locations, max net ES
## ---------------------------------
df <- max_netES_offset
max_netES <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                              column = 'offset_area_ha', 
                              limits = c(0, 300),
                              plot_title = 'c',
                              legend_title = 'Offset area',
                              legend_position = 'none', 
                              scale = 'magma', 
                              direction = -1)


## 2.4. Offset locations, max recreation equity weighted
## -----------------------------------------------------
df <- rec_mui_offset

rec_mui <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                               column = 'offset_area_ha', 
                               limits = c(0, 300),
                               plot_title = 'd', 
                               legend_title = 'Offset area',
                               legend_position = 'none', 
                               scale = 'magma', 
                               direction = -1)


## 2.5. Offset locations, min cost
## -------------------------------
df <- min_cost_offset

min_cost <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                               column = 'offset_area_ha', 
                               limits = c(0, 300),
                               plot_title = 'e', 
                               legend_title = 'Offset area',
                               legend_position = 'none', 
                               scale = 'magma', 
                               direction = -1)


## 2.5. facet plots
## ----------------

figure <- ggarrange(local_bio, max_bio,  max_es, max_netES, rec_mui, min_cost, 
                    ncol = 2, nrow = 3,
                    common.legend = TRUE,
                    legend = 'bottom') + 
  bgcolor("white") + 
  border("white")

# plot_title <- 'Locations for biodiversity offsetting of new housing developments'
# figure <- annotate_figure(figure, 
#                           top = text_grob(plot_title, color = "black", face = "bold", size = 32))
save_path <- paste0(gitpath,'Output/Maps/')
filename <- 'offsets_sng_2.jpeg'
ggsave(filename=filename, plot = figure, device = "jpeg",
       path = save_path, units = "in", width = 12, height = 16) 

# Note: unable to add black box to the colour bar 
# https://stackoverflow.com/questions/50070741/draw-border-around-legend-continuous-gradient-color-bar-of-heatmap 

## 2.6. max recreation
## -------------------
rec_offset <- read.csv('max_rec_offset_urban_sprawl.csv')
rec_offset <- merge(seer_2km, rec_offset, by='new2kid')

df <- rec_offset

max_rec <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                                column = 'offset_area_ha', 
                                limits = c(0, 300),
                                plot_title = 'f - max recreation', 
                                legend_title = 'Offset area',
                                legend_position = 'none', 
                                scale = 'magma', 
                                direction = -1)

## 2.7. facet plots
## ----------------

figure <- ggarrange(local_bio, max_bio,  max_es, rec_mui, min_cost, max_rec,
                    ncol = 2, nrow = 3,
                    common.legend = TRUE,
                    legend = 'bottom') + 
  bgcolor("white") + 
  border("white")

# plot_title <- 'Locations for biodiversity offsetting of new housing developments'
# figure <- annotate_figure(figure, 
#                           top = text_grob(plot_title, color = "black", face = "bold", size = 32))
save_path <- paste0(gitpath,'Output/Maps/')
filename <- 'biodiversity_offset_locations_max_rec_comp.jpeg'
ggsave(filename=filename, plot = figure, device = "jpeg",
       path = save_path, units = "in", width = 12, height = 16) 
