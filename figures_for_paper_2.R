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
gitpath <- "D:/Documents/GitHub/BNG/" 
datapath <- "C:/Users/mcm216/OneDrive - University of Exeter/Data/BNG/"

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
Eng_2kid <- read.csv(paste0(datapath, "DATA/SEER_GRID/England_new_2kid.csv"))

seer_2km <- seer_2km %>% 
  dplyr::filter(new2kid %in% Eng_2kid$new2kid)

## 1.2. Offset locations, local offset
## -----------------------------------
setwd(paste0(gitpath,'Output/JNCC/'))
local_bio_offset <- read.csv('local_bio_offset_sng.csv')
local_bio_offset <- merge(seer_2km, local_bio_offset, by='new2kid')

## 1.3. Offset locations, max biodiversity gains
## ---------------------------------------------
max_bio_offset <- read.csv('max_bio_offset_sng.csv')
max_bio_offset <- merge(seer_2km, max_bio_offset, by='new2kid')

## 1.4. Offset locations, max ecosystem services
## ---------------------------------------------
# max_bio_offset <- read.csv('max_bio_offset_urban_sprawl_2031.csv') # all services 
max_es_offset <- read.csv('max_netES_offset_sng.csv') # flooding and water quality excluded
max_es_offset <- merge(seer_2km, max_es_offset, by='new2kid')

## 1.5. Offset locations, equity weighted recreation
## -------------------------------------------------
rec_mui_offset <- read.csv('max_equity_offset_sng.csv')
rec_mui_offset <- merge(seer_2km, rec_mui_offset, by='new2kid')

## 1.5. Offset locations, equity weighted recreation
## -------------------------------------------------
min_cost_offset <- read.csv('min_cost_offset_sng.csv')
min_cost_offset <- merge(seer_2km, min_cost_offset, by='new2kid')

## ========
## (2) MAPS
## ========

## 2.1. Offset locations, local offset
## -----------------------------------

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
brewer.pal(4, "Dark2") # Set2 is okay 

coul <- c("black","#1B9E77","#D95F02","#7570B3","#E7298A")


source(paste0(gitpath, '/R_scripts/Functions/fcn_plt_map_coloured_border.R'))


df <- local_bio_offset
# update data paths in the function if this doesnt work  
local_bio <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                                 column = 'offset_area_ha', 
                                 limits = c(0, 500),
                                 plot_title = '(1) Local offset (status quo)', 
                                 legend_title = 'Offset area:',
                                 legend_position = 'none', 
                                 scale = 'magma',
                                 border_col = coul[1], 
                                 direction = -1)

local_bio_legend <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                                        column = 'offset_area_ha', 
                                        limits = c(0, 300),
                                        plot_title = '(1) Local offset (status quo)', 
                                        legend_title = 'Offset area:',
                                        legend_position = 'bottom', 
                                        scale = 'magma',
                                        border_col = coul[1], 
                                        direction = -1)

## 2.2. Offset locations, max biodiversity gains
## ---------------------------------------------

# define the boarder colour 
colors_border <- coul[1]

df <- max_bio_offset

max_bio <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                               column = 'offset_area_ha', 
                               limits = c(0, 300),
                               plot_title = '(2) Maximise biodiversity', 
                               legend_title = 'Offset area:',
                               legend_position = 'none', 
                               scale = 'magma',
                               border_col = coul[2], 
                               direction = -1)

## 2.3. Offset locations, min cost
## -------------------------------
df <- min_cost_offset

min_cost <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                                column = 'offset_area_ha', 
                                limits = c(0, 300),
                                plot_title = '(3) Minimise costs', 
                                legend_title = 'Offset area:',
                                legend_position = 'none', 
                                scale = 'magma',
                                border_col = coul[3],
                                direction = -1)

## 2.4. Offset locations, max household WTP
## ----------------------------------------
df <- max_es_offset
max_es <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                              column = 'offset_area_ha', 
                              limits = c(0, 300),
                              plot_title = '(4) Maximise (non-biodiveristy) co-benefits minus cost',
                              legend_title = 'Offset area:',
                              legend_position = 'none', 
                              scale = 'magma', 
                              border_col = coul[4], 
                              direction = -1)


## 2.4. Offset locations, max population WTP
## -----------------------------------------
df <- rec_mui_offset

rec_mui <- fcn_continuous_plot(plot_data = df[df$offset_area_ha > 0,], 
                               column = 'offset_area_ha', 
                               limits = c(0, 300),
                               plot_title = '(5) Maximise equity weighted co-benefits', 
                               legend_title = 'Offset area:',
                               legend_position = 'none', 
                               scale = 'magma',
                               border_col = coul[5],
                               direction = -1)

   



## ==============
## (2) Radar plot
## ==============

all_data <- read.csv(paste0(gitpath,"Output/Figures/Scenario_benefits_summary_table_sng.csv")) 

# install.packages("devtools")
# devtools::install_github("ricardo-bion/ggradar")
library(ggradar)

# join and scale the data 

all_data$scenario <- ordered(all_data$scenario, levels = c("local_offset", "max_bio", "min_cost", "max_netES", "max_rec_equity_weighted", "max_es" ))

radar_df <- all_data %>% 
    dplyr::filter(scenario == "local_offset"|
                  scenario == "max_bio" |
                  scenario == "min_cost"|
                  scenario == "max_netES" |
                  scenario == "max_rec_equity_weighted") %>% 
  dplyr::mutate(Net_benefits = total_benefits - total_costs) %>% 
  dplyr::select(scenario,sr_perc_chg , Net_benefits, total_rec, total_costs) %>% 
  dplyr::mutate(total_costs = total_costs*-1) %>% 
  as.data.frame()

scenario <- c("Option 1", "Option 2", "Option 4", "Option 5", "Option 3") # use options to ensure the colours are in the same order as all other figures
# scenario <- c("Local offset", "Max. biodiversity", "Max. ecosystem services", "Equity weighted", "Max. food production")


scenario <-as.data.frame(scenario)

# scale 
df_scaled <- round(apply(radar_df[,-1], 2, scales::rescale), 2) 
df_scaled <- as.data.frame(cbind(scenario = scenario, df_scaled)) 
head(df_scaled)

# Set graphic colors
# coul <-viridis(5,option = "viridis",) 
colors_border <- coul

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
                 axis.labels = c("Maximum biodiversity", "Maximum co-benefits minus costs", "Maximum equity weighted co-benefits", "Minimum costs"),
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
    
    
    plot.title = element_text(size = 8, hjust = 0, face = "bold")
    
  ) + 
  guides(colour = guide_legend(title.position="left", title.hjust = 0.1)) + 
  ggtitle("") 

radar


## ==============
## (4) Bar charts
## ==============

lapply(all_data, class)
# all_data$scenario <- ordered(all_data$scenario, levels = c("local_offset", "max_bio", "max_es", "max_netES", "max_rec_equity_weighted", "min_cost"))

bc_dat <- all_data %>% 
  dplyr::mutate(Net_benefits = total_benefits - total_costs) %>% 
  dplyr::select(scenario, total_rec, total_costs, Net_benefits, sr_perc_chg) %>%
  filter(scenario != "max_es") %>% 
  mutate(scenario_nm = case_when(scenario == "local_offset" ~ "Local offset (status quo)",
                                       scenario == "max_bio" ~ "Maximise biodiversity",
                                       scenario == "max_netES" ~ "Maximise (non-biodiveristy) co-benefits minus costs",
                                       scenario == "max_rec_equity_weighted" ~ "Maximise equity weighted co-benefits",
                                       scenario == "min_cost" ~ "Minimise costs")) %>% 
  gather("benefit", "value", total_rec:sr_perc_chg, factor_key=TRUE) %>% 
  dplyr::mutate(montetary_val = ifelse(benefit != "sr_perc_chg", 1, 0))

bc_dat$scenario_nm <- ordered(bc_dat$scenario_nm, levels = c("Local offset (status quo)",
                                                             "Maximise biodiversity",
                                                             "Minimise costs", 
                                                             "Maximise (non-biodiveristy) co-benefits minus costs", 
                                                             "Maximise equity weighted co-benefits"))

## 4.3 Plot barcharts 
## ------------------

# coul <-viridis(5,option = "viridis",) 
colors_border <- coul

#define hlines 
BNG_threshold <- 10

I <- bc_dat %>%
  filter(benefit == "sr_perc_chg") %>%
  mutate(value = value*100) %>%
  ggplot(aes(x = reorder(scenario_nm, dplyr::desc(scenario_nm)), y = value, fill = scenario_nm)) +
  geom_bar(stat = "identity", colour = NA) +
  # annotate("text", x = "Option 4", y = BNG_threshold, label = "10% BNG threshold", vjust = 0, hjust = 1.1, col = "red") +
  # geom_hline(aes(yintercept = BNG_threshold),linetype = 'dashed', col = 'red') +
  coord_flip() +
  ylab("Species richness change (%)") +
  xlab("") +
  ggtitle("(1) Biodiversity gain") +
  scale_fill_manual(values = colors_border, name = NULL) +
  scale_y_continuous(n.breaks = 6) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="left",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8))


bc_sr <- bc_dat %>% 
  filter(benefit == "sr_perc_chg") %>% 
  mutate(value = value*100) %>%  
  ggplot(aes(x = reorder(scenario_nm, dplyr::desc(scenario_nm)), y = value, fill = scenario_nm)) + 
  geom_bar(stat = "identity", colour = NA) +
  coord_flip() +
  ylab("Species richness change (%)") + 
  scale_y_continuous(expand = c(0, 0), n.breaks = 6) +
  xlab("") + 
  ggtitle("(1) Biodiversity gain") +
  scale_fill_manual(values = colors_border ) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour = "black", size = 8),
        axis.ticks = element_line(colour = "black", linewidth = 0.5), 
        panel.border = element_blank(),
        axis.line.y.left = element_line(colour = "white"), 
        axis.line.x.bottom = element_line(colour = "black", linewidth = 1), 
        plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10)) + 
  geom_hline(aes(yintercept = 0),linetype = 'solid', col = 'black', linewidth = 1.5)


bc_netben <- bc_dat %>% 
  filter(benefit == "Net_benefits") %>% 
  mutate(value = value/1000000000) %>%  
  ggplot(aes(x = reorder(scenario_nm, dplyr::desc(scenario_nm)), y = value, fill = scenario_nm)) +  
  geom_bar(stat = "identity", colour = NA) +
  coord_flip() +
  ylab("Net benefit (£ billion)") + 
  xlab("") + 
  ggtitle("(4) Co-benefits minus costs") +
  scale_fill_manual(values = colors_border ) +
  scale_y_continuous(expand = c(0, 0), n.breaks = 6) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour = "black", size = 8),
        axis.ticks = element_line(colour = "black", linewidth = 1), 
        panel.border = element_blank(),
        axis.line.y.left = element_line(colour = "white"), 
        axis.line.x.bottom = element_line(colour = "black", linewidth = 0.5), 
        plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10)) + 
  geom_hline(aes(yintercept = 0),linetype = 'solid', col = 'black', linewidth = 1.5)

bc_rec <- bc_dat %>% 
  filter(benefit == "total_rec") %>% 
  mutate(value = value/1000000000) %>%  
  ggplot(aes(x = reorder(scenario_nm, dplyr::desc(scenario_nm)), y = value, fill = scenario_nm)) +  
  geom_bar(stat = "identity", colour = NA) +
  coord_flip() +
  ylab("Recreation benefit (£ billion)") + 
  xlab("") + 
  ggtitle("(3) Co-benefits") +
  scale_fill_manual(values = colors_border ) + 
  scale_y_continuous(expand = c(0, 0), n.breaks = 6) +
  geom_hline(aes(yintercept = 0),linetype = 'solid', col = 'black') +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour = "black", size = 8),
        axis.ticks = element_line(colour = "black", linewidth = 1), 
        panel.border = element_blank(),
        axis.line.y.left = element_line(colour = "white"), 
        axis.line.x.bottom = element_line(colour = "black", linewidth = 0.5), 
        plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10)) + 
  geom_hline(aes(yintercept = 0),linetype = 'solid', col = 'black', linewidth = 1.5)


bc_food <- bc_dat %>% 
  filter(benefit == "total_costs") %>% 
  mutate(value = -1*value/1000000000) %>%  
  ggplot(aes(x = reorder(scenario_nm, dplyr::desc(scenario_nm)), y = value, fill = scenario_nm)) + 
  geom_bar(stat = "identity", colour = NA) +
  coord_flip() +
  ylab("Costs (£ billion)") + 
  xlab("") + 
  ggtitle("(2) Costs") +
  scale_fill_manual(values = colors_border ) + 
  scale_y_continuous(expand = c(0,0), n.breaks = 6) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour = "black", size = 8),
        axis.ticks = element_line(colour = "black", linewidth = 1), 
        panel.border = element_blank(),
        axis.line.y.left = element_line(colour = "white"), 
        axis.line.x.bottom = element_line(colour = "black", linewidth = 0.5), 
        plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10)) + 
  geom_hline(aes(yintercept = 0),linetype = 'solid', col = 'black', linewidth = 1.5)

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
maps_legend <- get_only_legend(local_bio_legend)
bar_legend <- get_only_legend(I)


## 5.1 Maps
## --------
maps <- ggarrange(local_bio, max_bio, min_cost, max_es, rec_mui, maps_legend, 
                  ncol = 2, nrow = 3) + 
  bgcolor("white") + 
  border("white")


# maps_with_legend <- ggarrange(maps, maps_legend, 
#                   ncol = 1, nrow = 2, 
#                   heights =  c(1, 0.1))

# Note: unable to add black box to the colour bar 
# https://stackoverflow.com/questions/50070741/draw-border-around-legend-continuous-gradient-color-bar-of-heatmap 

save_path <- paste0(gitpath,'Output/Figures/Figures_for_paper/')
filename <- '230214_facet_maps_sng.png'
ggsave(filename=filename, plot = maps, device = "png",
       path = save_path, units = "mm", height = 210, width = 297, dpi = 1200) 

## 5.2 Barcharts
## -------------

barcharts <- ggarrange(bc_rec, bc_netben, bc_sr, bc_food, 
                      ncol = 2, nrow = 2,
                      common.legend = TRUE,
                      legend = 'none') + 
  bgcolor("white") + 
  border("white")

barchart_with_leg <- ggarrange(barcharts, bar_legend,  
                               ncol = 1, nrow = 2, 
                               heights = c(1,0.1))


## 5.2 Barcharts and radar
## -----------------------

bar_radar <- ggarrange(radar, barcharts, 
                       ncol = 1, nrow = 2,
                       heights = c(1, 1), 
                       common.legend = TRUE,
                       legend = 'none', 
                       labels = c("b", "c")) + 
  bgcolor("white") + 
  border("white")



## 5.4 Portrait all figures
## ------------------------

all_fig <- ggarrange(maps, bar_radar, 
                         ncol = 2, nrow = 1,
                         heights = c(1),
                         widths = c(0.8, 1),
                         labels = c("a","")) 

all_fig_legends <- ggarrange(all_fig, bar_legend, 
                                  ncol = 1, nrow = 2, 
                                  heights = c(1, 0.1))

save_path <- paste0(gitpath,'Output/Figures/Figures_for_paper/')
filename <- '230214_biodiversity_offset_locations_with_radar_bars_portrait.png'
ggsave(filename=filename, plot = all_fig_legends, device = "png",
       path = save_path, units = "mm", height = 210, width = 297, dpi = 1200) 
# max height = 247 mm

## ===============
## editable export
## ===============

install.packages("export")
library(export)

save_path <- paste0(gitpath,'Output/Figures/Figures_for_paper/')
# maps 
ggsave(local_bio, file=paste0(save_path,"map_local_bio.png"), width=7, height=5)
ggsave(max_bio, file=paste0(save_path,"map_max_bio.png"), width=7, height=5)
ggsave(min_cost, file=paste0(save_path,"map_min_cost.png"), width=7, height=5)
ggsave(rec_mui, file=paste0(save_path,"map_rec.png"), width=7, height=5)
ggsave(max_es, file=paste0(save_path,"map_max_net_es.png"), width=7, height=5)
# radar
graph2ppt(radar, file=paste0(save_path,"radar.pptx"), width=7, height=5)
# barchart
graph2ppt(bc_sr, file=paste0(save_path,"bc_sr.pptx"), width=7, height=5)
graph2ppt(bc_food, file=paste0(save_path,"bc_cost.pptx"), width=7, height=5)
graph2ppt(bc_netben, file=paste0(save_path,"bc_net_ben.pptx"), width=7, height=5)
graph2ppt(bc_rec, file=paste0(save_path,"bc_rec.pptx"), width=7, height=5)
# legend
# radar
graph2ppt(I, file=paste0(save_path,"legend.pptx"), width=7, height=5)
