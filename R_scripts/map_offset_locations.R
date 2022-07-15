## map_offset_locations.R
## ======================
##
## Author: Mattia C. Mancini
## Created: 22-May-2021
## Last modified: 22-May-2021
## --------------------------
##
## DESCRIPTION
##
## Script that takes as input the spatial files created with the script
## 'calc_bio_offset.R' and outputs maps. The data mapped represents the
## locations selected to offset the biodiversity loss from urban development. 
## The criteria based on which offset locations are selected are the following:
##   1. Local offset
##   2. Offset in locations that generate the highest biodiversity improvements
##   3. Offset in locations that generate the highest household WTP
##   4. Offset in locations that generate the highest population WTP
##   5. Offset in locations that generate the highest hshold. WTP, equity weigh.
##   6. Offset in locations that generate the highest pop. WTP, equity weigh.
## =============================================================================

## (0) SETUP
## =========
rm(list=ls())
library(sf)
library(gridExtra)    # grid_arrange
library(ggpubr)       # annotate_figure

source('D:/Documents/GitHub/biodiversity-net-gain/R Scripts/Functions/fcn_plt_map.R')

## (1) LOAD THE DATA
##     1.1. - SEER 2km grid
##     1.2. - Offset locations, local offset
##     1.3. - Offset locations, max biodiversity gains
##     1.4. - Offset locations, max household WTP
##     1.5. - Offset locations, max population WTP
##     1.6. - Offset locations, max household WTP, equity weighted
##     1.7. - Offset locations, max population WTP, equity weighted
## ================================================================

## 1.1. Seer 2km grid
## ------------------
seer_2km <- st_read('D:/Documents/SEER/____STATE_OF_GB____/SEER_GIS/SEER_GRID/SEER_net2km.shp')
seer_2km <- seer_2km[, "new2kid"]

## 1.2. Offset locations, local offset
## -----------------------------------
setwd('D:/Documents/GitHub/biodiversity-net-gain/CSV Files/bio_offset_landuses/delta_birds/')
local_bio_offset <- read.csv('local_bio_offset.csv')
local_bio_offset <- merge(seer_2km, local_bio_offset, by='new2kid')

## 1.3. Offset locations, max biodiversity gains
## ---------------------------------------------
max_bio_offset <- read.csv('max_bio_offset.csv')
max_bio_offset <- merge(seer_2km, max_bio_offset, by='new2kid')

## 1.4. Offset locations, max household WTP
## ----------------------------------------
wtp_hh_offset <- read.csv('wtp_hh_offset.csv')
wtp_hh_offset <- merge(seer_2km, wtp_hh_offset, by='new2kid')

## 1.5. Offset locations, max population WTP
## ----------------------------------------
wtp_pop_offset <- read.csv('wtp_pop_offset.csv')
wtp_pop_offset <- merge(seer_2km, wtp_pop_offset, by='new2kid')

## 1.6. Offset locations, max household WTP, equity weighted
## ---------------------------------------------------------
wtp_hh_mui_offset <- read.csv('wtp_hh_mui_offset.csv')
wtp_hh_mui_offset <- merge(seer_2km, wtp_hh_mui_offset, by='new2kid')

## 1.7. Offset locations, max population WTP, equity weighted
## ----------------------------------------------------------
wtp_pop_mui_offset <- read.csv('wtp_pop_mui_offset.csv')
wtp_pop_mui_offset <- merge(seer_2km, wtp_pop_mui_offset, by='new2kid')

## (2) MAPS
## ========

## 2.1. Offset locations, local offset
## -----------------------------------
df <- local_bio_offset
df$local_offset <- df$local_offset / 1e4
# low_bound <- round(min(df$local_offset[df$local_offset > 0], na.rm = TRUE), 0)
# high_bound <- round(max(df$local_offset, na.rm = TRUE), 0)
# classes <- c(low_bound, 2.5, 5, 10, 25, 50, 100, 150, 200, 400)
# plt <- fcn_plt_map(data = df, 
#                    column_name = 'local_offset', 
#                    classes = classes,
#                    plot_title = 'Local biodiversity offset', 
#                    legend_title = 'Offset area',
#                    plot_legend = 'bottom', 
#                    scale = 'viridis', 
#                    direction = -1)
local_bio <- fcn_continuous_plot(plot_data = df[df$local_offset > 0,], 
                                 column = 'local_offset', 
                                 limits = c(0, 300),
                                 plot_title = 'a', 
                                 legend_title = '',
                                 legend_position = 'bottom', 
                                 scale = 'viridis', 
                                 direction = -1)

## 2.2. Offset locations, max biodiversity gains
## ---------------------------------------------
df <- max_bio_offset
df$max_bio_offset <- df$max_bio_offset / 1e4
max_bio <- fcn_continuous_plot(plot_data = df[df$max_bio_offset > 0,], 
                               column = 'max_bio_offset', 
                               limits = c(0, 300),
                               plot_title = 'b', 
                               legend_title = 'Offset area',
                               legend_position = 'none', 
                               scale = 'viridis', 
                               direction = -1)

## 2.3. Offset locations, max household WTP
## ----------------------------------------
df <- wtp_hh_offset
df$wtp_hh_offset <- df$wtp_hh_offset / 1e4
wtp_hh <- fcn_continuous_plot(plot_data = df[df$wtp_hh_offset > 0,], 
                              column = 'wtp_hh_offset', 
                              limits = c(0, 300),
                              plot_title = 'Max household WTP offset', 
                              legend_title = 'Offset area',
                              legend_position = 'none', 
                              scale = 'viridis', 
                              direction = -1)


## 2.4. Offset locations, max population WTP
## -----------------------------------------
df <- wtp_pop_offset
df$wtp_pop_offset <- df$wtp_pop_offset / 1e4
wtp_pop <- fcn_continuous_plot(plot_data = df[df$wtp_pop_offset > 0,], 
                               column = 'wtp_pop_offset', 
                               limits = c(0, 300),
                               plot_title = 'c', 
                               legend_title = 'Offset area',
                               legend_position = 'none', 
                               scale = 'viridis', 
                               direction = -1)

## 2.5. Offset locations, max household WTP, equity weighted
## ---------------------------------------------------------
df <- wtp_hh_mui_offset
df$wtp_hh_mui_offset <- df$wtp_hh_mui_offset / 1e4
wtp_hh_mui <- fcn_continuous_plot(plot_data = df[df$wtp_hh_mui_offset > 0,], 
                                  column = 'wtp_hh_mui_offset', 
                                  limits = c(0, 300),
                                  plot_title = 'Max household WTP offset\nequity weighted', 
                                  legend_title = 'Offset area',
                                  legend_position = 'none', 
                                  scale = 'viridis', 
                                  direction = -1)

## 2.6. Offset locations, max population WTP, equity weighted
## ----------------------------------------------------------
df <- wtp_pop_mui_offset
df$wtp_pop_mui_offset <- df$wtp_pop_mui_offset / 1e4
wtp_pop_mui <- fcn_continuous_plot(plot_data = df[df$wtp_pop_mui_offset > 0,], 
                                   column = 'wtp_pop_mui_offset', 
                                   limits = c(0, 300),
                                   plot_title = 'd', 
                                   legend_title = 'Offset area',
                                   legend_position = 'none', 
                                   scale = 'viridis', 
                                   direction = -1)


# figure <- grid.arrange(local_bio, max_bio,  wtp_hh, wtp_pop, wtp_hh_mui, wtp_pop_mui,
#                        layout_matrix = rbind(c(1, 2, 3), c(4, 5, 6)))

figure <- ggarrange(local_bio, max_bio,  wtp_pop, wtp_pop_mui, 
                    ncol = 2, nrow = 2,
                    common.legend = TRUE,
                    legend = 'bottom')

# plot_title <- 'Locations for biodiversity offsetting of new housing developments'
# figure <- annotate_figure(figure, 
#                           top = text_grob(plot_title, color = "black", face = "bold", size = 32))
save_path <- 'D:/Documents/NetGain/Maps/'
filename <- 'biodiversity_offset_locations_deltabirds_Nature.jpeg'
ggsave(filename=filename, plot = figure, device = "jpeg",
       path = save_path, units = "in", width = 12, height = 16)


