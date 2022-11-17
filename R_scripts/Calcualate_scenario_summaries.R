## Calcualate scenario summaries.R
## ===============================
##
## Author: Mattia Mancini and Rebecca Collins 
## Created: 17-Jul-2022
## Last modified: 17-Jul-2022
## ------------------------------------------
##
## Script takes the outputs from save_output_structure_as_csvs.m and processes
## and summarises into data that can be mapped and plotted, and from which 
## summary tables can be produced.


## Still to do: 
## 1) check the calculation for species richness for the table - we are seeing
##    a decline in biodiversity where there is an increase in SNG and woodland
## 2) check the data going into the cartograph weighting
## 3) check the dimensions for the radar plot = costs isnt revealing much and 
##    going again our narrative for equity weighting. 
## =============================================================================

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
datapath <- "D:/Documents/OneDrive - University of Exeter/Data/BNG/"

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
file_path <- paste0(gitpath,"Output/baseline_2031_urbanisation/Offset_outputs/")

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

# save - save in different folder for repeatability as above code reads within one folder 
write.csv(all_data, paste0(gitpath,"Output/baseline_2031_urbanisation/all_scenario_offset_outputs.csv"))

all_data <- read.csv(paste0(gitpath,"Output/baseline_2031_urbanisation/all_scenario_offset_outputs.csv"))

# 1.3. Load spatial data for mapping 
# ----------------------------------

eng_border <- st_read(paste0(datapath, "Data/SEER_GRID/england_full_clipped.shp"))

# 1.3. Load species rich data for summary 
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

## =============================================================================
## (2) Summarise benefits
## ======================

# WE WANT TO CREATE A TABLE THAT FOR EACH OF THE SCENARIOS
# SCEARIOS ARE: (1) LOCAL OFFSET, 
#               (2) MAX. BIODIVERSITY, 
#               (3) MAX. ES, 
#               (4) MAX. Es EQUITY WEIGHTED 

# VARIABLES: (1) benefits = result_file = benefits = total 
#            (2) costs = result_file = costs = total 
#            (3) sp rich = result_file = env_outs = bio - CHECK  

total_benefits <- all_data %>%
  filter(result_file == "benefits") %>% 
  filter(hectares_chg > 0) %>% 
  group_by(scenario) %>% 
  summarise(total_benefits = sum(ghg_forestry) + sum(rec), 
            total_other_ben = sum(ghg_forestry), 
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

# summarise benefits 
benefit_table <- full_join(total_benefits, total_costs, by = "scenario") %>% 
  full_join(total_sp_rich_sum_tbl, by = "scenario")

write.csv(benefit_table, paste0(gitpath, "Output/Figures/Scenario_benefits_summary_table.csv"))


## =============================================================================
## 3.0. Radar plot 
## ===============

# https://r-charts.com/ranking/ggradar/
# https://exts.ggplot2.tidyverse.org/ggradar.html


# install.packages("devtools")
# devtools::install_github("ricardo-bion/ggradar")
library(ggradar)

# format data 

radar_df_benefits <- all_data %>%
  dplyr::filter(result_file == "benefits") %>%
  dplyr::filter(hectares_chg > 0 ) %>% 
  group_by(scenario) %>%
  dplyr::summarise(other_ben = (sum(total)-sum(rec)),
                   tot_rec = sum(rec))

radar_df_es_benefits <- all_data %>% 
  dplyr::filter(result_file == "benefits") %>%
  dplyr::filter(hectares_chg > 0 ) %>%
  group_by(scenario) %>% 
  dplyr::summarise(tot_rec = sum(rec),
                   tot_ghg = (sum(ghg_farm)+sum(ghg_forestry)+sum(ghg_soil_forestry)),
                   tot_flooding = sum(flooding),
                   tot_wq = (sum(totn)+sum(totp))
  )

radar_df_bio <- all_data %>% 
  dplyr::filter(result_file == "env_outs") %>% 
  dplyr::filter(hectares_chg > 0 ) %>%
  left_join(total_sp_rich, by = c("new2kid", "scenario")) %>%  #add in percentage change from 2000 LCM
  group_by(scenario) %>% 
  dplyr::summarise(mean_sr = mean(sr_perc_chg)) 

radar_df_cost <- all_data %>% 
  dplyr::filter(result_file == "costs") %>% 
  dplyr::filter(hectares_chg > 0 ) %>%
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
ggradar(df_scaled,
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
        axis.labels = c("Other benefits", "Recreation", "Biodiversity", "Cost"),
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


# if we chose to make a multi-panel plot 

  figure <- ggarrange(p1, 
                    ggarrange(p1,p1, nrow = 2), 
                    ncol = 2, nrow = 1,
                    common.legend = TRUE,
                    legend = 'right') + 
  bgcolor("white") + 
  border("white")


# 2kids for max bio, 

max_bio_2kid <- all_data %>% 
  dplyr::filter(result_file == "env_outs") %>% 
  dplyr::filter(scenario == "max_bio") %>% 
  dplyr::filter(hectares_chg > 0) %>%
  dplyr::select(new2kid)

max_rec_2kid <- all_data %>% 
  dplyr::filter(result_file == "env_outs") %>% 
  dplyr::filter(hectares_chg > 0) %>%
  dplyr::filter(scenario == "max_rec_equity_weighted") %>% 
  dplyr::select(new2kid)

min_cost_2kid <- all_data %>% 
  dplyr::filter(result_file == "env_outs") %>% 
  dplyr::filter(hectares_chg > 0) %>%
  dplyr::filter(scenario == "min_cost") %>% 
  dplyr::select(new2kid)

sr_base_max_bio <- sr %>%
  filter(new2kid %in% max_bio_2kid$new2kid ) %>% 
  summarise(avg = mean(sr_100_2000),
            max = max(sr_100_2000),
            min = min(sr_100_2000))
  
sr_base_max_rec <- sr %>%
  filter(new2kid %in% max_rec_2kid$new2kid ) %>% 
  summarise(avg = mean(sr_100_2000),
            max = max(sr_100_2000),
            min = min(sr_100_2000))


sr_base_min_cost <- sr %>%
  filter(new2kid %in% min_cost_2kid$new2kid ) %>% 
  summarise(avg = mean(sr_100_2000),
            max = max(sr_100_2000),
            min = min(sr_100_2000))

check_bio <- all_data %>% 
  filter(result_file == "env_outs") %>% 
  filter(scenario == "max_bio") %>% 
  select(new2kid, hectares_chg)

all_data %>% 
  filter(result_file == "env_outs") %>% 
  group_by(scenario) %>% 
  summarise(total_ha_chg = sum(hectares_chg))

