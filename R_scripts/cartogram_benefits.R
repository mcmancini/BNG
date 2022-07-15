## cartogram_benefits.R
## ====================
## 
## Author: Mattia Mancini
## Created: 14-Jul-2021
## Last modified: 28-Jul-2021
## --------------------------
##
## DESCRIPTION
## Script that creates a cartogram of the benefits by region in England
## ====================================================================

## (0) SETUP
## =========
rm(list=ls())
library(sf)
library(RPostgres)
library(cartogram)
library(ggplot2)
library(gridExtra)    # grid_arrange
library(ggpubr)       # annotate_figure

## (1) LOAD AND PREPARE THE REQUIRED DATA
## ======================================

## 1.1. Seer 2km grid
## ------------------
setwd('D:/Documents/GitHub/biodiversity-net-gain/Data/SEER_GIS/SEER_GRID/')
seer_2km <- st_read('SEER_net2km.shp')[, 6]

## 1.2. Benefit tables
## -------------------
setwd('D:/Documents/GitHub/biodiversity-net-gain/CSV Files/benefits')
benefits <- read.csv('tot_benefits_delta_birds.csv')[, c(1, 3, 7, 15, 25)]
benefits <- merge(seer_2km, benefits, by='new2kid')

## 1.3. Regions in England
## -----------------------
setwd('D:/Documents/SEER/____STATE_OF_GB____/SEER_GIS/Regions_December_2017_Full_Clipped_Boundaries_in_England/')
regions_shp <- st_read('Regions_December_2017_Full_Clipped_Boundaries_in_England.shp')[, 3]

## 1.4. Census for number of households and median income by cell in England
## -------------------------------------------------------------------------
setwd('D:/Documents/GitHub/biodiversity-net-gain/Data/SEER_SOCIO_ECON/Census_2011')
num_hh <- read.csv('SEER_2k_socioecon_eng.csv')[, c(1, 4:5)]
cell_idx <- benefits[, 'new2kid', drop=TRUE]
num_hh <- num_hh[num_hh$new2kid %in% cell_idx,]
benefits <- merge(benefits, num_hh, by = 'new2kid', all = TRUE)

## 1.5. Merge seer grid and benefits and remove areas outside England
## ------------------------------------------------------------------
benefits <- st_join(benefits, regions_shp, largest=TRUE)

## (2) AGGREGATION BY REGION
## =========================
df <- benefits[, ,drop=TRUE]
df$geometry <- NULL
region_aggr <- aggregate(df[,2:6], by=list(df$rgn17nm), sum)
region_aggr[,2:5] <- region_aggr[,2:5] / region_aggr[, 6]
reg_income <- aggregate(df[,7], by=list(df$rgn17nm), mean)
region_aggr <- merge(region_aggr, reg_income, by = 'Group.1')
colnames(region_aggr)[c(1, 7)] <- c('Region', 'Income')

regions <- merge(regions_shp, region_aggr, by.x='rgn17nm', by.y='Region')
regions[which(regions$rgn17nm == 'Yorkshire and The Humber'), 'rgn17nm'] <- 'Yorkshire'

## (3) CARTOGRAMS
## ==============
cartogram_pop <- cartogram_cont(regions, weight = 'wtp_pop_pop', itermax = 30, prepare = 'none')
cartogram_mui <- cartogram_cont(regions, weight = 'wtp_pop_mui_pop_mui', itermax = 50, prepare = 'none')

# eng_income <- ggplot(data = regions) +
#   geom_sf(aes(fill = Income)) +
#   scale_fill_viridis(option = "magma", 
#                      trans = 'sqrt', 
#                      direction = -1) +
#   theme_bw() +
#   ggtitle('a') +
#   theme(panel.border = element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
#         legend.position = 'bottom', 
#         legend.title = element_blank(),
#         legend.text = element_text(size = 18),
#         legend.key.width=unit(1,"in")) +
#   coord_sf(datum = NA)

eng_income <- ggplot(data = regions, aes(x=reorder(rgn17nm, Income),y=Income)) +
  geom_bar(position="dodge",stat="identity", fill='dodgerblue4', width = 0.6) + 
  coord_flip() +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5), 
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18), 
        axis.text = element_text(size = 16)) +
  labs(y='Average income by region')
  

eng_pop <- ggplot(data = cartogram_pop) +
  geom_sf(aes(fill = wtp_pop_pop)) +
  scale_fill_viridis(option = "inferno", 
                     trans = 'sqrt', 
                     direction = -1,
                     limits = c(8, 24), 
                     labels = function(x) paste0("£", seq(8, 24, 4))) +
  theme_bw() +
  ggtitle('a') +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.width=unit(1,"in")) +
  coord_sf(datum = NA)

eng_mui <- ggplot(data = cartogram_mui) +
  geom_sf(aes(fill = wtp_pop_mui_pop_mui)) +
  scale_fill_viridis(option = "inferno", 
                     trans = 'sqrt', 
                     direction = -1,
                     limits = c(8, 24), 
                     labels = function(x) paste0("£", seq(8, 24, 4))) +
  theme_bw() +
  ggtitle('b') +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
        legend.position = 'none', 
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.width=unit(1,"in")) +
  coord_sf(datum = NA)

# figure <- grid.arrange(eng_pop, eng_mui, 
#                        nrow = 1, 
#                        common.legend = TRUE,
#                        legend = 'bottom')

cartograms <- ggarrange(eng_pop, eng_mui,  
                        ncol = 2, nrow = 1,
                        common.legend = TRUE,
                        legend = 'bottom')

figure <- ggarrange(eng_income, cartograms, ncol = 2, nrow = 1)

# plot_title <- 'Regional effects of equity weighting on average household\nbenefits from biodiversity improvements'
# figure <- annotate_figure(figure, 
#                           top = text_grob(plot_title, color = "black", face = "bold", size = 32))
save_path <- 'D:/Documents/NetGain/Maps/'
filename <- 'Regional_WTP_mui.jpeg'
ggsave(filename=filename, plot = figure, device = "jpeg",
       path = save_path, units = "in", width = 12, height = 5)
