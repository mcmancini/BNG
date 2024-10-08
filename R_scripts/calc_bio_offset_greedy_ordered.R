## calc_bio_offset_equal_projects.R
## ================================
##
## Author: Mattia Mancini and Rebecca Collins 
## Created: 10-Mar-2021
## Last modified: 10-Nov-2022
## ------------------------------------------
##
## Script to locate compensation areas and to compute costs, biodiversity and 
## welfare trade-offs based on a series of criteria: 
##   1) Local offset - locations for compensation near building sites.
##   2) Offset based on max biodiversity improvements - locations for 
##      compensation in areas with the highest potential biodiversity gains.
##   3) Offset based on max Ecosystem Services - locations for compensation in 
##      areas with the highest potential for ecosystem services gains.
##   4) Offset based on max recreational values equity weighted
## =============================================================================

## (0) SET-UP
## ==========
rm(list=ls())
library(sf)
library(dplyr)

# update path for different machines 
gitpath            <- "D:\\Documents\\GitHub\\BNG\\" 
datapath           <- "D:\\Documents\\OneDrive - University of Exeter\\Data\\BNG\\Data\\"

# Input paths
seer_path              <- paste0(datapath,    "SEER_GRID/")
base_lcm_path          <- paste0(datapath,    "LCM/LCM_2km/lcm_aggr_2000.csv")
urban_lcm_path         <- paste0(datapath,    "Urban Sprawl - F.Eigenbrod/urban_sprawl_2031_sprawl.csv")
bio_jncc_input_path    <- paste0(gitpath,     "Output/farm2sng_bio.csv")
bio_pollinator_path    <- paste0(gitpath,     "Output/farm2sng_pollinator_ucl.csv")
bio_priority_path      <- paste0(gitpath,     "Output/farm2sng_priority_ucl.csv")
bio_poll_prio_path     <- paste0(gitpath,     "Output/farm2sng_poll_prio_ucl.csv")
es_input_path          <- paste0(gitpath,     "Output/farm2sng_es.csv")
net_es_input_path      <- paste0(gitpath,     "Output/farm2sng_netES.csv")
rec_input_path         <- paste0(gitpath,     "Output/farm2sng_rec.csv")
cost_input_path        <- paste0(gitpath,     "Output/farm2sng_Oc.csv")
census_path            <- paste0(datapath,    "SEER_SOCIO_ECON/SEER_2k_socioecon_eng.csv")

# Output paths
output_path             <- paste0(gitpath,     "Output/")
local_output_path       <- paste0(output_path, "local_bio_offset_sng.csv")
bio_output_path         <- paste0(output_path, "max_bio_offset_sng.csv")
pollinator_output_path  <- paste0(output_path, "max_pollinator_offset_sng.csv")
priority_output_path    <- paste0(output_path, "max_priority_offset_sng.csv")
poll_prio_output_path   <- paste0(output_path, "max_poll_prio_offset_sng.csv")
es_output_path          <- paste0(output_path, "max_es_offset_sng.csv")
net_es_output_path      <- paste0(output_path, "max_netES_offset_sng.csv")
rec_output_path         <- paste0(output_path, "max_equity_offset_sng.csv")
cost_output_path        <- paste0(output_path, "min_cost_offset_sng.csv")

# load required functions
source(paste0(gitpath, '/R_scripts/Functions/load_bng_data.R'))
source(paste0(gitpath, '/R_scripts/Functions/ordering_algorithms.R'))

# Input parameters
mui        <-  1.3   # marginal utility of income
saveondisk <-  TRUE  # if false, it won't save!

## (1) LOAD THE DATA AND PREPROCESS IT
##     - 1. SEER 2km grid
##     - 2. new housing locations
## ===================================

## 1.1. SEER 2km grid
## ------------------
seer_2km <- load_seer_grid(seer_path)

## 1.2. New housing locations
## --------------------------
city_spread <- load_housing_locations(base_lcm_path, urban_lcm_path)

## (2) OFFSET MECHANISMS
##     2.1. - Local offset
##     2.2. - Maximise biodiversity
##     2.3. - Maximise ecosystem services
##     2.4. - Maximise recreation equity-weighted
##     2.5. - Minimise opportunity cost of agriculture
## ===================================================

## 2.1. Local offset
## -----------------
local_offset <- calc_local_offset(city_spread, local_output_path, 'sng', saveondisk)

## 2.2. Offset based on max biodiversity improvements
## --------------------------------------------------
max_bio <- read.csv(bio_jncc_input_path)[, c('new2kid','sr_chg_ha')] %>%
  rename(target = sr_chg_ha)
bio_offset <- maximise_target(city_spread, max_bio, 'sng', decreasing=TRUE, bio_output_path, saveondisk=TRUE)

max_pollinators <- read.csv(bio_pollinator_path)[, c('new2kid','sr_chg_ha')] %>%
  rename(target = sr_chg_ha)
pollinator_offset <- maximise_target(city_spread, max_pollinators, 'sng', decreasing=TRUE, pollinator_output_path, saveondisk=TRUE)

max_priority <- read.csv(bio_priority_path)[, c('new2kid','sr_chg_ha')] %>%
  rename(target = sr_chg_ha)
priority_offset <- maximise_target(city_spread, max_priority, 'sng', decreasing=TRUE, priority_output_path, saveondisk=TRUE)

max_poll_prio <- read.csv(bio_poll_prio_path)[, c('new2kid','sr_chg_ha')] %>%
  rename(target = sr_chg_ha)
poll_prio_offset <- maximise_target(city_spread, max_poll_prio, 'sng', decreasing=TRUE, poll_prio_output_path, saveondisk=TRUE)


## 2.3. Offset based on max Ecosystem Services
## -------------------------------------------
max_es <-  read.csv(es_input_path)[, c('new2kid','tot_es_ha')] %>%
  rename(target = tot_es_ha)
es_offset <- maximise_target(city_spread, max_es, 'sng', decreasing=TRUE, es_output_path, saveondisk)

## 2.4. Offset based on max net ecosystem services
## -----------------------------------------------
max_netES <-  read.csv(net_es_input_path)[, c('new2kid','net_es_ha')] %>%
  rename(target = net_es_ha)
net_es_offset <- maximise_target(city_spread, max_netES, 'sng', decreasing=TRUE, net_es_output_path, saveondisk)

## 2.5. offset based max rec, equity weighted for recreation 
## ---------------------------------------------------------
max_rec <- read.csv(rec_input_path)[, c('new2kid','rec_ha')] %>%
  rename(target = rec_ha)
max_rec <- merge(seer_2km, max_rec, by = 'new2kid')

# load medium hh income 
med_income <- read.csv(census_path) %>% 
  dplyr::select(new2kid, Med_hh_inc)
max_rec <- merge(max_rec, med_income, by = 'new2kid')

# equity weighting 
base_income <- median(max_rec$Med_hh_inc)
cell_income <- max_rec$Med_hh_inc
mui_weights <- base_income^mui * cell_income^(-mui)
max_rec$target <- max_rec$target*mui_weights 
max_rec <- max_rec %>%
  dplyr::select(new2kid, target)
st_geometry(max_rec) <- NULL
max_rec$target[is.na(max_rec$target)] <- 0
rec_offset <- maximise_target(city_spread, max_rec, offset_lc='sng', decreasing=TRUE, rec_output_path, saveondisk)

## 2.6. Offset based on min. cost
## ------------------------------
min_cost <- read.csv(cost_input_path)[, c('new2kid', 'farm_oc_ha')] %>%
  rename(target = farm_oc_ha)
cost_offset <- maximise_target(city_spread, min_cost, 'sng', decreasing=FALSE, cost_output_path, saveondisk)

