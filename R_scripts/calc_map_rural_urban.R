## calc_rural_urban.R
## ==================
##
## Author: Mattia Mancini
## Created: 20-Sep-2021
## Last modified: 20-Sep-2021
## --------------------------
##
## DESCRIPTION
## Script that takes the benefit table obtained from the calc_benefits.R script 
## together with data on rural and urban classification at the LSOA level, and 
## combines them to compute how BNG offset benefits are distributed between 
## rural/urban areas depending on the BNG mechanisms selected
## ============================================================================

## (0) SETUP
## =========
rm(list=ls())
library(sf)
library(units)
library(ggplot2)
library(gridExtra)    # grid_arrange
library(ggpubr)       # annotate_figure
library(RColorBrewer) # palettes
library(scales)

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

## 1.3. Rural/urban classification for LSOAs (retrieved from
## https://www.gov.uk/government/statistics/2011-rural-urban-classification-lookup-tables-for-all-geographies)
## -----------------------------------------------------------------------------
setwd('D:/Documents/GitHub/biodiversity-net-gain/Data/SEER_GIS/')
urb_rur <- read.csv('LSOA_rural_urban_class.csv')[, c(1, 5)]
colnames(urb_rur) <- c('code', 'class')

## 1.4. LSOA shapefile
## -------------------
setwd('D:/Documents/GitHub/biodiversity-net-gain/Data/SEER_GIS/Boundaries_Census/England_LSOA/')
lsoa <- st_read('england_lsoa_2011.shp')[, 1]
lsoa <- merge(lsoa, urb_rur, by='code')

## (2) ASSIGN RURAL/URBAN CLASSIFICATION TO SEER 2KM CELLS
## =======================================================
seer_urban <- st_intersection(seer_2km, lsoa)
seer_urban$area <- st_area(seer_urban)

benefits$urban_percent <- NA
for (i in benefits$new2kid){
  df <- seer_urban[seer_urban$new2kid == i,]
  df_urban <- df[df$class == 'Urban', ]
  urban_percent <- drop_units(sum(df_urban$area) / sum(df$area))
  benefits[benefits$new2kid == i, 'urban_percent'] <- urban_percent
}

## (3) COMPUTE URBAN AND RURAL BENEFITS BASED ON THE BNG SCENARIOS
## ===============================================================
local_pop   <- benefits[, c('new2kid', 'local_pop', 'urban_percent', 'geometry')]
max_bio_pop <- benefits[, c('new2kid', 'max_bio_pop', 'urban_percent', 'geometry')]
wtp_pop     <- benefits[, c('new2kid', 'wtp_pop_pop', 'urban_percent', 'geometry')]
wtp_pop_mui <- benefits[, c('new2kid', 'wtp_pop_mui_pop_mui', 'urban_percent', 'geometry')]

local_pop$urban_benefits   <- local_pop$local_pop * local_pop$urban_percent
local_pop$rural_benefits   <- local_pop$local_pop * (1 - local_pop$urban_percent)
max_bio_pop$urban_benefits <- max_bio_pop$max_bio_pop * max_bio_pop$urban_percent
max_bio_pop$rural_benefits <- max_bio_pop$max_bio_pop * (1 - local_pop$urban_percent)
wtp_pop$urban_benefits     <- wtp_pop$wtp_pop_pop * wtp_pop$urban_percent
wtp_pop$rural_benefits     <- wtp_pop$wtp_pop_pop * (1 - wtp_pop$urban_percent)
wtp_pop_mui$urban_benefits <- wtp_pop_mui$wtp_pop_mui_pop_mui * wtp_pop_mui$urban_percent
wtp_pop_mui$rural_benefits <- wtp_pop_mui$wtp_pop_mui_pop_mui * (1 - wtp_pop_mui$urban_percent)

# Function to normalise benefits from urban and rural areas such that the ratio
# between the two is expressed in a scale varying from -1 (all benefits from 
# rural land) to +1 (all benefits from urban land), passing through 0 (50% from
# urban and 50% from rural land)
normalise <- function(urban_val, rural_val){
  span <- urban_val + rural_val
  ratio_urban <- urban_val / span
  range <- 2 * ratio_urban - 1
  return(range)
}

local_pop$urban_ratio   <- NA
max_bio_pop$urban_ratio <- NA
wtp_pop$urban_ratio     <- NA
wtp_pop_mui$urban_ratio <- NA

# local BNG offset
for (i in local_pop$new2kid){
  urban_val <- local_pop[local_pop$new2kid == i, 'urban_benefits', drop=TRUE]
  rural_val <- local_pop[local_pop$new2kid == i, 'rural_benefits', drop=TRUE]
  local_pop$urban_ratio[local_pop$new2kid == i] <- normalise(urban_val, rural_val)
}

# Max biodiversity BNG offset
for (i in max_bio_pop$new2kid){
  urban_val <- max_bio_pop[max_bio_pop$new2kid == i, 'urban_benefits', drop=TRUE]
  rural_val <- max_bio_pop[max_bio_pop$new2kid == i, 'rural_benefits', drop=TRUE]
  max_bio_pop$urban_ratio[max_bio_pop$new2kid == i] <- normalise(urban_val, rural_val)
}

# Max population WTP BNG offset
for (i in wtp_pop$new2kid){
  urban_val <- wtp_pop[wtp_pop$new2kid == i, 'urban_benefits', drop=TRUE]
  rural_val <- wtp_pop[wtp_pop$new2kid == i, 'rural_benefits', drop=TRUE]
  wtp_pop$urban_ratio[wtp_pop$new2kid == i] <- normalise(urban_val, rural_val)
}

# Max population WTP, equity weighted BNG offset
for (i in wtp_pop_mui$new2kid){
  urban_val <- wtp_pop_mui[wtp_pop_mui$new2kid == i, 'urban_benefits', drop=TRUE]
  rural_val <- wtp_pop_mui[wtp_pop_mui$new2kid == i, 'rural_benefits', drop=TRUE]
  wtp_pop_mui$urban_ratio[wtp_pop_mui$new2kid == i] <- normalise(urban_val, rural_val)
}


## (4) FIGURE
## ==========

## 4.1. Pie charts
## ---------------
local_bng <- data.frame(
  group = c("urban_benefits", "rural_benefits"),
  values = c(sum(local_pop$urban_benefits) / (sum(local_pop$urban_benefits) + sum(local_pop$rural_benefits)), 
             sum(local_pop$rural_benefits) / (sum(local_pop$urban_benefits) + sum(local_pop$rural_benefits)))
)

bio_bng <- data.frame(
  group = c("urban_benefits", "rural_benefits"),
  values = c(sum(max_bio_pop$urban_benefits) / (sum(max_bio_pop$urban_benefits) + sum(max_bio_pop$rural_benefits)), 
             sum(max_bio_pop$rural_benefits) / (sum(max_bio_pop$urban_benefits) + sum(max_bio_pop$rural_benefits)))
)

wtp_bng <- data.frame(
  group = c("urban_benefits", "rural_benefits"),
  values = c(sum(wtp_pop$urban_benefits) / (sum(wtp_pop$urban_benefits) + sum(wtp_pop$rural_benefits)), 
             sum(wtp_pop$rural_benefits) / (sum(wtp_pop$urban_benefits) + sum(wtp_pop$rural_benefits)))
)

wtp_mui_bng <- data.frame(
  group = c("urban_benefits", "rural_benefits"),
  values = c(sum(wtp_pop_mui$urban_benefits) / (sum(wtp_pop_mui$urban_benefits) + sum(wtp_pop_mui$rural_benefits)), 
             sum(wtp_pop_mui$rural_benefits) / (sum(wtp_pop_mui$urban_benefits) + sum(wtp_pop_mui$rural_benefits)))
)



local_pie <- ggplot(local_bng, aes(x="", y=values, fill=group))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  scale_fill_manual(values = alpha(c('darkkhaki', 'snow4'), 0.8),
                    labels = c('Rural', 'Urban')) +
  ggtitle("a") +
  theme(
    plot.title = element_text(size=30),
    axis.text.x=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.position = 'none') +
  geom_text(aes(y = values/2 + c(0, cumsum(values)[-length(values)]), 
                label = percent(values, accuracy = 0.1)), size=5)

bio_pie <- ggplot(bio_bng, aes(x="", y=values, fill=group))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  scale_fill_manual(values = alpha(c('darkkhaki', 'snow4'), 0.8),
                    labels = c('Rural', 'Urban')) +
  ggtitle("b") +
  theme(
    plot.title = element_text(size=30),
    axis.text.x=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.position = 'none') +
  geom_text(aes(y = values/2 + c(0, cumsum(values)[-length(values)]), 
                label = percent(values, accuracy = 0.1)), size=5)

wtp_pie <- ggplot(wtp_bng, aes(x="", y=values, fill=group))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  scale_fill_manual(values = alpha(c('darkkhaki', 'snow4'), 0.8),
                    labels = c('Rural', 'Urban')) +
  ggtitle("c") +
  theme(
    plot.title = element_text(size=30),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.position = 'none') +
  geom_text(aes(y = values/2 + c(0, cumsum(values)[-length(values)]), 
                label = percent(values, accuracy = 0.1)), size=5)

wtp_mui_pie <- ggplot(wtp_mui_bng, aes(x="", y=values, fill=group))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  scale_fill_manual(values = alpha(c('darkkhaki', 'snow4'), 0.8),
                    labels = c('Rural', 'Urban')) +
  ggtitle("d") +
  theme(
    plot.title = element_text(size=30),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.position = 'none') +
  geom_text(aes(y = values/2 + c(0, cumsum(values)[-length(values)]), 
                label = percent(values, accuracy = 0.1)), size=5)

## 4.2. Map
## --------
# eng_border <- st_read('D:/Documents/SEER/____STATE_OF_GB____/SEER_GIS/Countries (Great Britain)/england_full_clipped.shp')
map <- ggplot() +
  # geom_sf(data = eng_border, fill = 'NA', lwd = 1) +
  geom_sf(data = lsoa,
          aes(fill = class),
          color = NA) +
  scale_fill_manual(values = alpha(c('darkkhaki', 'snow4'), 1)) +
  theme_bw() +
  theme(plot.title = element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

pies <- ggarrange(local_pie, bio_pie, wtp_pie, wtp_mui_pie,  
                        ncol = 2, nrow = 2,
                        common.legend = TRUE,
                        legend = 'bottom')

figure <- ggarrange(map, pies, ncol = 2, nrow = 1)

save_path <- 'D:/Documents/NetGain/Maps/'
filename <- 'rural_urban_benefits.jpeg'
ggsave(filename=filename, plot = figure, device = "jpeg",
       path = save_path, units = "in", width = 12, height = 8)



