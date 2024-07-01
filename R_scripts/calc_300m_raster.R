## calc_300m_raster.R
## ==================
##
## Author: Mattia Mancini
## Created: 09-May-2023
## ----------------------
##
## DESCRIPTION
## -----------
## Script that takes the 25m resolution rasters for the various activity-based 
## scenarios and the CEH lcm 2020 and converts them to multilayer rasters at a
## 300m resolution for all the land cover types that are available in lcm.
## This is used as the land cover input for the Glob2Loc biodiversity model.
## ===========================================================================

## (0) SETUP
## =========
rm(list=ls())

# Paths
# -----


