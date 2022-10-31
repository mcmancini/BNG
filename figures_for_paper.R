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
library(viridis)
library(dplyr)
library(tidyr)

#update path for different machines 
gitpath <- "C:/Users/Rebecca/Documents/GitHub/BNG/" 
datapath <- "C:/Users/Rebecca/OneDrive - University of Exeter/Data/BNG/"

source(paste0(gitpath, '/R_scripts/Functions/fcn_plt_map_coloured_border.R'))

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

# # filter to England 
# conn <- dbConnect(Postgres(), 
#                   dbname = "NEV",
#                   host = "localhost",
#                   port = 5432,
#                   user="postgres",
#                   password="postgres")
# 
# df <- dbGetQuery(conn, "SELECT * FROM regions_keys.key_grid_countries_england")
# cell_id <- df$new2kid
# seer_2km <- seer_2km[seer_2km$new2kid %in% cell_id, 'new2kid']

# Alternative when not on the network 
Eng_2kid <- read.csv(paste0(gitpath, "Output/England_new_2kid.csv"))

seer_2km <- seer_2km %>% 
  dplyr::filter(new2kid %in% Eng_2kid$new2kid)

## 1.2. Offset locations, local offset
## -----------------------------------
setwd(paste0(gitpath,'Output/'))
local_bio_offset <- read.csv('local_bio_offset_urban_sprawl.csv')
local_bio_offset <- merge(seer_2km, local_bio_offset, by='new2kid')

## 1.3. Offset locations, max biodiversity gains
## ---------------------------------------------
max_bio_offset <- read.csv('max_bio_offset_urban_sprawl_2031.csv')
max_bio_offset <- merge(seer_2km, max_bio_offset, by='new2kid')

## 1.4. Offset locations, max ecosystem services
## ---------------------------------------------
# max_bio_offset <- read.csv('max_bio_offset_urban_sprawl_2031.csv') # all services 
max_bio_offset <- read.csv('max_es_rec_ghg_only_offset_urban_sprawl_scc.csv') # flooding and water quality excluded
max_bio_offset <- merge(seer_2km, max_bio_offset, by='new2kid')

## 1.5. Offset locations, equity weighted recreation
## -------------------------------------------------
rec_mui_offset <- read.csv('max_rec_offset_urban_sprawl_equity_weighted.csv')
rec_mui_offset <- merge(seer_2km, rec_mui_offset, by='new2kid')

## 1.5. Offset locations, equity weighted recreation
## -------------------------------------------------
min_cost_offset <- read.csv('min_cost_offset_urban_sprawl_2031.csv')
min_cost_offset <- merge(seer_2km, min_cost_offset, by='new2kid')

## ========
## (2) MAPS
## ========

## 2.1. Offset locations, local offset
## -----------------------------------

coul <-viridis(5,option = "viridis",) 

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
coul <- brewer.pal(5, "Dark2") # Set2 is okay 


source(paste0(gitpath, '/R_scripts/Functions/fcn_plt_map_coloured_border.R'))


df <- local_bio_offset
# update data paths in the function if this doesnt work  
local_bio <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                                 column = 'offset_area_ha', 
                                 limits = c(0, 300),
                                 plot_title = 'i', 
                                 legend_title = 'Offset area:',
                                 legend_position = 'bottom', 
                                 scale = 'magma',
                                 border_col = coul[1], 
                                 direction = -1)

## 2.2. Offset locations, max biodiversity gains
## ---------------------------------------------

# define the boarder colour 
colors_border <- coul[2]

df <- max_bio_offset

max_bio <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                               column = 'offset_area_ha', 
                               limits = c(0, 300),
                               plot_title = 'ii', 
                               legend_title = 'Offset area:',
                               legend_position = 'none', 
                               scale = 'magma',
                               border_col = coul[2], 
                               direction = -1)

## 2.3. Offset locations, max household WTP
## ----------------------------------------
df <- max_es_offset
max_es <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                              column = 'offset_area_ha', 
                              limits = c(0, 300),
                              plot_title = 'iii',
                              legend_title = 'Offset area:',
                              legend_position = 'none', 
                              scale = 'magma', 
                              border_col = coul[3], 
                              direction = -1)


## 2.4. Offset locations, max population WTP
## -----------------------------------------
df <- rec_mui_offset

rec_mui <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                               column = 'offset_area_ha', 
                               limits = c(0, 300),
                               plot_title = 'iv', 
                               legend_title = 'Offset area:',
                               legend_position = 'none', 
                               scale = 'magma',
                               border_col = coul[4],
                               direction = -1)

   

## 2.5. Offset locations, min cost
## -------------------------------
df <- min_cost_offset

min_cost <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                               column = 'offset_area_ha', 
                               limits = c(0, 300),
                               plot_title = 'v', 
                               legend_title = 'Offset area:',
                               legend_position = 'none', 
                               scale = 'magma',
                               border_col = coul[5],
                               direction = -1)

## ==============
## (2) Radar plot
## ==============

all_data <- read.csv(paste0(gitpath,"Output/baseline_2031_urbanisation/all_scenario_offset_outputs.csv")) 

total_sp_rich <- read.csv(paste0(gitpath, "Output/Figures/Scenario_total_sp_rich.csv"))

# install.packages("devtools")
# devtools::install_github("ricardo-bion/ggradar")
library(ggradar)

# format data 

# radar_df_benefits <- all_data %>%
#   dplyr::filter(result_file == "benefits") %>%
#   group_by(scenario) %>%
#   dplyr::summarise(other_ben = (sum(total)-sum(rec)),
#                    tot_rec = sum(rec))

radar_df_benefits <- all_data %>% 
  dplyr::filter(result_file == "benefits") %>% 
  group_by(scenario) %>% 
  dplyr::summarise(tot_rec = sum(rec),
                   tot_ghg = (sum(ghg_farm)+sum(ghg_forestry)+sum(ghg_soil_forestry)),
                   tot_flooding = sum(flooding),
                   tot_wq = (sum(totn)+sum(totp))
  )

radar_df_bio <- all_data %>% 
  dplyr::filter(result_file == "env_outs") %>% 
  left_join(total_sp_rich, by = c("new2kid", "scenario")) %>%  #add in percentage change from 2000 LCM
  group_by(scenario) %>% 
  dplyr::summarise(mean_sr = mean(sr_perc_chg)) 

radar_df_cost <- all_data %>% 
  dplyr::filter(result_file == "costs") %>% 
  dplyr::group_by(scenario) %>%
  dplyr::summarise(tot_cost = sum(total))

# join and scale the data 
radar_df <- full_join(radar_df_benefits, radar_df_bio, by = "scenario") %>% 
  full_join(radar_df_cost, by = "scenario") %>% 
  dplyr::filter(scenario == "local_offset"|
                  scenario == "max_bio" |
                  scenario == "max_es" |
                  scenario == "max_rec_equity_weighted" |
                  scenario == "min_cost") %>% 
  as.data.frame()

scenario <- c("Option 1", "Option 2", "Option 3", "Option 4", "Option 5")

scenario <-as.data.frame(scenario)

# scale 
df_scaled <- round(apply(radar_df[,-1], 2, scales::rescale), 2) 
df_scaled <- as.data.frame(cbind(scenario = scenario, df_scaled)) 
head(df_scaled)

# Set graphic colors
# coul <-viridis(5,option = "viridis",) 
colors_border <- coul
colors_in <- alpha(coul,0.8)

#plot 
radar <- ggradar(df_scaled,
                 # grid customization 
                 background.circle.colour = "grey",
                 axis.line.colour = "gray60",
                 gridline.min.colour = "gray60",
                 gridline.mid.colour = "gray60",
                 gridline.max.colour = "gray60",
                 label.gridline.min = FALSE,
                 label.gridline.mid = FALSE,
                 label.gridline.max = FALSE,
                 # axis labels customization
                 # axis.labels = c("Net benefit", "Recreation", "Biodiversity", "Cost"),
                 axis.labels = c("Max. recreation", "Max. GHG removal", "Max. flood risk reduction", "Max. water quality", "Max. biodiversity gain", "Min. food reduction"), 
                 axis.label.offset = 1.05,
                 axis.label.size = 3,
                 # line and point customization
                 group.point.size = 0, 
                 group.colours = colors_border,
                 group.line.width = 0.9,
                 # legend customization
                 legend.title = "BNG policy:",
                 # plot.legend = FALSE
) +
  theme(
    # Legend title and text labels
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Title font color size and face
    legend.title = element_text(size = 8, hjust = 0.5),
    # Title alignment. Number from 0 (left) to 1 (right)
    legend.title.align = NULL,
    # Text label font color size and face
    legend.text = element_text(size = 8),
    # Text label alignment. Number from 0 (left) to 1 (right)
    legend.text.align = 0,
    
    # Legend position, margin and background
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Legend position: right, left, bottom, top, none
    legend.position = "bottom", 
    # Margin around each legend
    legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
    
    # Legend direction and justification
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Layout of items in legends ("horizontal" or "vertical")
    legend.direction = "horizontal", 
    # Positioning legend inside or outside plot 
    # ("center" or two-element numeric vector) 
    legend.justification = "center", 
    
    # Spacing between legends. 
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # legend.spacing = unit(0.1, "cm"),
    legend.spacing.x = NULL,                 # Horizontal spacing
    legend.spacing.y = NULL,                 # Vertical spacing
    
    # Legend box
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # Arrangement of multiple legends ("horizontal" or "vertical")
    legend.box = NULL, 
    # Margins around the full legend area
    legend.box.margin = margin(0, 0, 0, 0, "cm"), 
    # Background of legend area: element_rect()
    legend.box.background = element_blank(), 
    # The spacing between the plotting area and the legend box
    legend.box.spacing = unit(0.4, "cm"), 
    
    
    plot.title = element_text(size = 11, hjust = 0, face = "bold")
    
  ) + 
  guides(colour = guide_legend(title.position="left", title.hjust = 0.1)) + 
  ggtitle("") 

radar


## ==============
## (4) Bar charts
## ==============

# 4.1. Load species rich data for summary 
# ---------------------------------------

sr_chg <- read.csv(paste0(gitpath, "Output/species_richness_change_2000-2031.csv")) 

sr <- read.csv(paste0(gitpath, "Output/species_richness_2000.csv")) %>% 
  left_join(sr_chg, by = "new2kid") %>% 
  dplyr::mutate(sr_100_2031 = sr_100 + sr_chg_100) %>% 
  dplyr::rename(sr_100_2000 = sr_100) %>% 
  dplyr::select(-sr_chg_100)

# identify the cells that experience change using the landscover
lcm_agg_2000 <- read.csv(paste0(datapath,"Data/LCM/LCM_2km/lcm_aggr_2000.csv")) %>% 
  dplyr::rename(farm_ha_2000 = farm_ha, 
                wood_ha_2000 = wood_ha, 
                sng_ha_2000 = sng_ha, 
                urban_ha_2000 = urban_ha, 
                water_ha_2000 = water_ha)

urban_sprawl_2031 <- read.csv(paste0(datapath,"Data/Urban Sprawl - F.Eigenbrod/urban_sprawl_2031_sprawl.csv")) %>% 
  dplyr::rename(farm_ha_2031 = farm_ha, 
                wood_ha_2031 = wood_ha, 
                sng_ha_2031 = sng_ha, 
                urban_ha_2031 = urban_ha, 
                water_ha_2031 = water_ha)

lcm_2000_2031_chg <- left_join(lcm_agg_2000, urban_sprawl_2031, by = "new2kid") %>% 
  filter(new2kid %in%  seer_2km$new2kid) %>% 
  dplyr::mutate(farm_chg = ifelse((farm_ha_2000 - farm_ha_2031) != 0, 1, 0),
                wood_chg = ifelse((wood_ha_2000 - wood_ha_2031) != 0, 1, 0),
                sng_chg = ifelse((sng_ha_2000 - sng_ha_2031) != 0, 1, 0), 
                urban_chg = ifelse((urban_ha_2000 - urban_ha_2031) != 0, 1, 0), 
                water_chg = ifelse((water_ha_2000 - water_ha_2031) != 0, 1, 0),  
                any_chg = ifelse((farm_chg + wood_chg + sng_chg + urban_chg + water_chg), 1, 0)
  ) %>% 
  dplyr::select(new2kid, any_chg)

# baseline figures: 
sr %>% 
  filter(new2kid %in%  seer_2km$new2kid) %>% 
  left_join(lcm_2000_2031_chg, by = "new2kid") %>% 
  filter(any_chg == 1) %>% 
  dplyr::mutate(sr_chg = sr_100_2031 - sr_100_2000, 
                sr_perc_chg = ((sr_100_2031 - sr_100_2000)/sr_100_2000),
                sr_perc_chg = replace_na(sr_perc_chg, 0)) %>%
  summarise(sr_2000 = mean(sr_100_2000), 
            sr_2030 = mean(sr_100_2031),
            sr_chg = mean(sr_chg),
            sr_perc_chg = mean(sr_perc_chg))


# 4.2. Summarise benefits from outputs  
# ------------------------------------

total_benefits <- all_data %>%
  filter(result_file == "benefits") %>% 
  filter(hectares_chg > 0) %>% 
  group_by(scenario) %>% 
  summarise(total_benefits = sum(total), 
            total_other_ben = sum(total)-sum(rec), 
            total_rec = sum(rec))

total_costs <- all_data %>%
  filter(result_file == "costs") %>% 
  filter(hectares_chg > 0) %>% 
  group_by(scenario) %>% 
  summarise(total_costs = sum(total))

total_sp_rich <- all_data %>%
  filter(result_file == "env_outs") %>%
  left_join(sr, by = "new2kid") %>% 
  dplyr::mutate(scen_bio = (sr_100_2031 + bio), 
                sr_chg = (scen_bio - sr_100_2000), 
                sr_perc_chg = ((scen_bio - sr_100_2000)/sr_100_2000),
                sr_perc_chg = replace_na(sr_perc_chg, 0),
                sr_ha = ifelse(hectares_chg > 0, ((scen_bio - sr_100_2000)/hectares_chg),0),
                sr_ha = replace_na(sr_ha, 0),
                sr_perc_chg_ha = ifelse(hectares_chg > 0, (sr_perc_chg/hectares_chg),0), 
                sr_perc_chg_ha = replace_na(sr_perc_chg_ha, 0)) %>% 
  dplyr::select(new2kid, scenario, sr_chg, scen_bio, sr_perc_chg, sr_ha, hectares_chg) 


total_sp_rich_sum_tbl <- total_sp_rich %>%
  filter(hectares_chg > 0) %>% 
  group_by(scenario) %>% 
  summarise(sr_chg = mean(sr_chg),
            sr_perc_chg = mean(sr_perc_chg),
            sr = mean(scen_bio), 
            sr_ha = mean(sr_ha)) %>% 
  as.data.frame()

## 4.3 Make summary table 
## ----------------------
benefits <- full_join(total_benefits, total_costs, by = "scenario") %>% 
  dplyr::mutate(net_ben = total_benefits - total_costs) %>% 
  full_join(total_sp_rich_sum_tbl, by = "scenario") %>% 
  dplyr::rename(BNG.policy.option = scenario) 


## 4.4 filter and reshape 
## ----------------------
lapply(benefits, class)
benefits$BNG.policy.option <- as.factor(benefits$BNG.policy.option)

# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/ 
dat <- benefits %>%
  dplyr::select(-sr_chg, -total_benefits, -total_other_ben, - sr, -sr_ha) %>%
  filter(BNG.policy.option != "max_rec") %>% 
  filter(BNG.policy.option != "max_es_equity_weighted") %>% 
  mutate(BNG.policy.option = case_when(BNG.policy.option == "local_offset" ~ "Option 1",
                                       BNG.policy.option == "max_bio" ~ "Option 2",
                                       BNG.policy.option == "max_es" ~ "Option 3",
                                       BNG.policy.option == "max_rec_equity_weighted" ~ "Option 4",
                                       BNG.policy.option == "min_cost" ~ "Option 5")) %>% 
  gather("benefit", "value", total_rec:sr_perc_chg, factor_key=TRUE) %>% 
  dplyr::mutate(montetary_val = ifelse(benefit != "sr_perc_chg", 1, 0))

## 4.3 Plot barcharts 
## ------------------

# coul <-viridis(5,option = "viridis",) 
colors_border <- coul

#define hlines 
BNG_threshold <- 10

III <- dat %>% 
  filter(benefit == "sr_perc_chg") %>% 
  mutate(value = value*100) %>%  
  ggplot(aes(x = reorder(BNG.policy.option, desc(BNG.policy.option)), y = value, fill = BNG.policy.option)) + 
  geom_bar(stat = "identity", colour = NA) +
  # annotate("text", x = "Option 4", y = BNG_threshold, label = "10% BNG threshold", vjust = 0, hjust = 1.1, col = "red") + 
  # geom_hline(aes(yintercept = BNG_threshold),linetype = 'dashed', col = 'red') + 
  coord_flip() +
    ylab("Species richness change (%)") + 
  xlab("") + 
  ggtitle("iii") +
  scale_fill_manual(values = colors_border ) + 
  scale_y_continuous(n.breaks = 6) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="right", 
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 9)) 

I <- dat %>% 
  filter(benefit == "net_ben") %>% 
  mutate(value = value/1000000000) %>%  
  ggplot(aes(x = reorder(BNG.policy.option, desc(BNG.policy.option)), y = value, fill = BNG.policy.option)) +  
  geom_bar(stat = "identity", colour = NA) +
  coord_flip() +
  ylab("Net benefit (£ billion)") + 
  xlab("") + 
  ggtitle("i") +
  scale_fill_manual(values = colors_border ) +
  scale_y_continuous(n.breaks = 6) + # use labels to redefine 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none", 
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 9)) 

II <- dat %>% 
  filter(benefit == "total_rec") %>% 
  mutate(value = value/1000000) %>%  
  ggplot(aes(x = reorder(BNG.policy.option, desc(BNG.policy.option)), y = value, fill = BNG.policy.option)) +  
  geom_bar(stat = "identity", colour = NA) +
  coord_flip() +
  ylab("Recreation benefit (£ million)") + 
  xlab("") + 
  ggtitle("ii") +
  scale_fill_manual(values = colors_border ) + 
  scale_y_continuous(n.breaks = 6) + # use labels to redefine 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 9))

IV <- dat %>% 
  filter(benefit == "total_costs") %>% 
  mutate(value = value/1000000000) %>%  
  ggplot(aes(x = reorder(BNG.policy.option, desc(BNG.policy.option)), y = value, fill = BNG.policy.option)) + 
  geom_bar(stat = "identity", colour = NA) +
  coord_flip() +
  ylab("Cost (£ billion)") + 
  xlab("") + 
  ggtitle("iv") +
  scale_fill_manual(values = colors_border ) + 
  scale_y_continuous(n.breaks = 6) + # use labels to redefine 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.text.y = element_text(colour = "black"),
        plot.title = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 9)) 

## ===============
## (5) FACET PLOTS
## ===============

# extract the legends - common legends does not work to extract the policy colours and the density 
# https://www.geeksforgeeks.org/add-common-legend-to-combined-ggplot2-plots-in-r/ 

get_only_legend <- function(plot) {
  
  # get tabular interpretation of plot
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  
  #  Mark only legend in plot
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  
  # extract legend
  legend <- plot_table$grobs[[legend_plot]]
  
  # return legend
  return(legend) 
}

radar_legend <- get_only_legend(radar)
maps_legend <- get_only_legend(local_bio)
# bar_legend <- get_only_legend(I)


## 5.1 Maps
## --------
maps <- ggarrange(local_bio, max_bio,  max_es, rec_mui, min_cost,
                  ncol = 1, nrow = 5,  
                  common.legend = TRUE,
                  legend = 'none') + 
  bgcolor("white") + 
  border("white")

maps_with_legend <- ggarrange(maps, maps_legend, 
                  ncol = 1, nrow = 2, 
                  heights =  c(1, 0.1))

# Note: unable to add black box to the colour bar 
# https://stackoverflow.com/questions/50070741/draw-border-around-legend-continuous-gradient-color-bar-of-heatmap 

save_path <- paste0(gitpath,'Output/Maps/')
filename <- 'facet_maps.jpeg'
ggsave(filename=filename, plot = maps_with_legend, device = "jpeg",
       path = save_path, units = "mm", height = 200, width = 61) 

## 5.2 Barcharts
## -------------

barcharts <- ggarrange(I, II, III, IV, 
                      ncol = 2, nrow = 2,
                      common.legend = TRUE,
                      legend = 'none') + 
  bgcolor("white") + 
  border("white")

save_path <- paste0(gitpath,'Output/Figures/')
filename <- 'biodiversity_offset_locations_benefits_chart.jpeg'
ggsave(filename=filename, plot = barcharts, device = "jpeg",
       path = save_path) 


## 5.2 Barcharts and radar
## -----------------------

bar_radar <- ggarrange(radar, barcharts, 
                       ncol = 1, nrow = 2,
                       heights = c(1,1), 
                       common.legend = TRUE,
                       legend = 'none', 
                       labels = c("b", "c")) + 
  bgcolor("white") + 
  border("white")


## 5.3 Maps and radar only
## -----------------------

# maps_radar <- ggarrange(local_bio, max_bio,  max_es, rec_mui, min_cost, radar,
#                   ncol = 2, nrow = 3,
#                   common.legend = FALSE,
#                   legend = 'none', 
#                   labels = c("a", "b", "c", "d", "e", "f")) + 
#   bgcolor("white") + 
#   border("white")
# 
# figure.2 <- grid.arrange(maps_radar, radar_legend, maps_legend, nrow = 3, heights = c(10, 0.5, 0.5))
# 
# save_path <- paste0(gitpath,'Output/Maps/')
# filename <- 'biodiversity_offset_locations_with_radar.jpeg'
# ggsave(filename=filename, plot = figure.2, device = "jpeg",
#        path = save_path, units = "in", height = 16, width = 12) 

## 5.4 Portrait all figures
## ------------------------

all_portrait <- ggarrange(maps_with_legend, bar_radar, 
                         ncol = 2, nrow = 1,
                         heights = c(1),
                         widths = c(0.5, 1),
                         labels = c("a","")) 

all_portrait_legends <- ggarrange(all_portrait, radar_legend, 
                                  ncol = 1, nrow = 2, 
                                  heights = c(1, 0.1))

save_path <- paste0(gitpath,'Output/Maps/')
filename <- 'biodiversity_offset_locations_with_radar_bars_portrait.jpeg'
ggsave(filename=filename, plot = all_portrait_legends, device = "jpeg",
       path = save_path, units = "mm", height = 200, width = 183) 
# max height = 247 mm

