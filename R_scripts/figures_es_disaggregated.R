## figures_es_disaggregated.R
## ==========================
##
## Author: Mattia Mancini and Rebecca Collins 
## Created: 05-Sep-2022
## Last modified: 05-Sep-2022
## ------------------------------------------
##
## Script takes the outputs from save_output_structure_as_csv.m and creates
## plots for each ecosystem service for each scenario
## ========================================================================

## (0) SET-UP
## ==========
rm(list=ls())
library(sf)
library(dplyr)
library(RPostgres)
library(stringr)
library(tidyr)
library(ggplot2)
library(viridis)
library(gridExtra)    # grid_arrange
library(ggpubr)       # annotate_figure

# source('D:\Documents\GitHub\BNG\R_scripts/Functions/fcn_plt_map.R')

#update path for different machines 
gitpath <- "D:/Documents/GitHub/BNG/"
datapath <- "D:/Documents/Data/BNG/"

## =============================================================================
## (1) Load the data
## =================

## 1.1. SEER 2km grid
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

# 1.2. Load data from save_output_structure_as_csvs.m 
# ---------------------------------------------------
#define file path
file_path <- paste0(gitpath,"Output/baseline_2031_urbanisation/Offset_outputs/SCC/")

#specify the correct file extension eg .xls .csv .xlsx
path_files <- dir(file_path, pattern = "*.csv")

#read file files and merge them into one dataframe
all_data <- tibble(filename = path_files) %>% 
  mutate(file_contents = purrr::map(filename,
                             ~read.csv(file.path(file_path, .)))) %>% 
  tidyr::unnest(cols = c(file_contents)) %>% 
  # use stringr do make file identifiers when subsetting the data 
  dplyr::mutate(filename = str_remove(filename, pattern = ".csv"), 
         scenario = ifelse(str_detect(filename, "local_offset") == TRUE,"local_offset", 
                           ifelse(str_detect(filename,"max_bio") == TRUE, "max_bio", 
                                   ifelse(str_detect(filename,"max_es_offset") == TRUE, "max_es", 
                                           ifelse(str_detect(filename, "max_es_equity") == TRUE, "max_es_equity_weighted",
                                                  ifelse(str_detect(filename, "max_rec_offset")==TRUE, "max_rec", 
                                                         ifelse(str_detect(filename, "max_rec_equity") == TRUE, "max_rec_equity_weighted", "min_cost")))))), 
         result_file = ifelse(str_detect(filename, "benefits") == TRUE, "benefits", 
                          ifelse(str_detect(filename, "costs") == TRUE, "costs", 
                                  ifelse(str_detect(filename, "es_outs") == TRUE, "es_outs", 
                                         ifelse(str_detect(filename, "env_outs") == TRUE, "env_outs", 0))))) %>% 
  # Filter to England 
  filter(new2kid %in%  seer_2km$new2kid)


## 2. Radar plot 
## =============

# https://r-charts.com/ranking/ggradar/
# https://exts.ggplot2.tidyverse.org/ggradar.html


# install.packages("devtools")
# devtools::install_github("ricardo-bion/ggradar")
library(ggradar)

# format data 
radar_df_es <- all_data %>% 
  dplyr::filter(result_file == "benefits") %>%
  dplyr::filter(hectares_chg > 0 ) %>%
  group_by(scenario) %>% 
  dplyr::summarise(tot_rec = sum(rec),
                   tot_ghg = (sum(ghg_farm)+sum(ghg_forestry)+sum(ghg_soil_forestry)),
                   tot_flooding = sum(flooding),
                   tot_wq = (sum(totn)+sum(totp))
  )


# join and scale the data 
radar_df <- radar_df_es %>% 
  dplyr::filter(scenario == "local_offset"|
                  scenario == "max_bio" |
                  scenario == "max_es" |
                  scenario == "max_rec_equity_weighted" |
                  scenario == "min_cost") %>% 
  as.data.frame()

scenario <- c("Local offset", "Maximum biodiversity", "Maximum ecosystem services", "Equity weighted", "Minimum cost")

scenario <-as.data.frame(scenario)

# scale 
df_scaled <- round(apply(radar_df[,-1], 2, scales::rescale), 2) 
df_scaled <- as.data.frame(cbind(scenario = scenario, df_scaled)) 
head(df_scaled)

# Set graphic colors
coul <-viridis(5,option = "turbo",) 
colors_border <- coul
colors_in <- alpha(coul,0.8)

#plot 
radar_es <- ggradar(df_scaled,
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
          axis.labels = c("Recreation", "GHGs", "Flooding", "Water quality"),
          axis.label.offset = 1.05,
          axis.label.size = 4,
          # line and point customization
          group.point.size = 0, 
          group.colours = colors_border,
          group.line.width = 0.9,
          # legend customization
          legend.title = "",
          plot.legend = FALSE, 
          fill = FALSE
  ) + theme(
      # Legend title and text labels
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      # Title font color size and face
      legend.title = element_text(size = 0),
      # Title alignment. Number from 0 (left) to 1 (right)
      legend.title.align = NULL,
      # Text label font color size and face
      legend.text = element_text(size = 11),
      # Text label alignment. Number from 0 (left) to 1 (right)
      legend.text.align = 0,
      
      # Legend position, margin and background
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      # Legend position: right, left, bottom, top, none
      legend.position = "right", 
      # Margin around each legend
      legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
      
      # Legend direction and justification
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      # Layout of items in legends ("horizontal" or "vertical")
      legend.direction = "vertical", 
      # Positioning legend inside or outside plot 
      # ("center" or two-element numeric vector) 
      legend.justification = "center", 
      
      # Spacing between legends. 
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      legend.spacing = unit(0.4, "cm"), 
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
      legend.box.spacing = unit(0.4, "cm")
    ) 

save_path <- 'D:/Documents/GitHub/BNG/Output/Figures/'
ggsave('radar_es.jpeg', radar_es, path=save_path, units = "in", width = 12, height = 5)


## (3) BAR PLOT
## ============
df_es <- radar_df_es %>% 
  dplyr::filter(scenario == "local_offset"|
                  scenario == "max_bio" |
                  scenario == "max_es" |
                  scenario == "max_rec_equity_weighted" |
                  scenario == "min_cost") %>% 
  as.data.frame()

colnames(df_es) <- c('scenario', 'recreation', 'GHGs', 'flooding', 'water quality')
df_long <- reshape2::melt(df_es, id.vars = c('scenario'))

df_long <- df_long %>% 
  mutate(across('scenario', str_replace, 'local_offset', 'Local\noffset')) %>%
  mutate(across('scenario', str_replace, 'max_bio', 'Maximum\nbiodiversity')) %>%
  mutate(across('scenario', str_replace, 'max_es', 'Maximum\necosystem services')) %>%
  mutate(across('scenario', str_replace, 'max_rec_equity_weighted', 'equality-based\nallocation')) %>%
  mutate(across('scenario', str_replace, 'min_cost', 'Market allocation'))

df_long <- df_long %>%
  mutate(across('scenario', factor, levels=c("Local\noffset",
                                             "Maximum\nbiodiversity",
                                             "Maximum\necosystem services",
                                             "equality-based\nallocation",
                                             "Market allocation")))


plt <- ggplot(df_long, aes(fill= variable, y=value, x=variable)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label = formatC(value, format = 'e', digits=2)), vjust = -1) +
  facet_wrap(~scenario) +
  scale_y_continuous(limits = c(-5e8, 7.5e10)) +
  scale_fill_viridis(discrete = T, option = "G", direction = -1) +
  ggtitle("Ecosystem services by BNG policy option") +
  ylab("Value in GBP [£]") +
  facet_wrap(~scenario) +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("")

ggsave('es_by_scenario_scc.jpeg', plt, path = save_path, units = "in", width = 12, height = 7)
