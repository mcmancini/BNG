## ordering_algorithms.R
## =====================
## 
## Authors: Mattia Mancini, Rebecca Collins
## Created: 10-Nov-2022
## ----------------------------------------
## 
## DESCRIPTION
## Script containing the ordering algorithms for locating offsets based on a
## variety of policy criteria, including local offsetting, and target
## near maximisation
## The functions in this script are called by the 
## calc_bio_offset_greedy_ordered.R script
## ==========================================================================

library(sf)
library(dplyr)
library(RPostgres)
library(tidyr)

## CHECK_OUTPUT. Function checking that checks and returns the output of the
##   ordering algorithms. Used internally in this script
## =========================================================================
check_output <- function(offset_locations, offset_lc){
  if (offset_lc == 'mixed'){
    df <- offset_locations %>%
      dplyr::mutate(
        wood_ha = wood_ha_scenario + (offset/2), 
        sng_ha = sng_ha_scenario + (offset/2)) %>% 
      rename(
        farm_ha = farmland_area,
        urban_ha = urban_ha_scenario, 
        water_ha = water_ha_scenario) %>% 
      dplyr::select(
        new2kid, 
        farm_ha,
        wood_ha, 
        sng_ha, 
        urban_ha, 
        water_ha, 
        offset) %>% 
      dplyr::rename(offset_area_ha = offset)
  } else if (offset_lc == 'sng'){
    df <- offset_locations %>%
      dplyr::mutate(
        sng_ha = sng_ha_scenario + offset) %>% 
      rename(
        wood_ha = wood_ha_scenario,
        farm_ha = farmland_area,
        urban_ha = urban_ha_scenario, 
        water_ha = water_ha_scenario) %>% 
      dplyr::select(
        new2kid, 
        farm_ha,
        wood_ha, 
        sng_ha, 
        urban_ha, 
        water_ha, 
        offset) %>% 
      dplyr::rename(offset_area_ha = offset)
  }
  
  # check cell areas
  check_area <- df %>% 
    group_by(new2kid) %>% 
    mutate(sum = farm_ha + wood_ha + sng_ha + urban_ha +water_ha) %>% 
    filter(round(sum, 2) != 400) # some small rounding errors 
  if (nrow(check_area) != 0){
    stop('Land uses do not add up to 400 hectares')
  }
  
  # check all offset is allocated 
  if (sum(offset_locations$offset) != sum(offset_locations$area_new_builds)){
    stop('Area of offsets does not match the area of the development projects')
  }
  return(df)
}

## 1) LOCAL OFFSET
## ===============
calc_local_offset <- function(locs_to_offset, output_path, offset_lc, saveondisk){
  
  # we know for each cell the area in m2 converted into new buildings. Is there an
  # equal amount of farmland to be converted into high biodiversity land? If so, 
  # do the conversion; otherwise, convert all the available farmland left (if any)
  # and do the remaining conversion in the nearest cell with available farmland.
  # The conversion land is a mix of 50% sng and 50% woodland
  
  ## Case 1: there is more farmland than the required land for biodiversity 
  ## compensation
  locs_to_offset$offset <- 0
  
  idx <- (locs_to_offset$area_new_builds > 0) & (locs_to_offset$farmland_area >= locs_to_offset$area_new_builds)
  locs_to_offset$offset[idx] <- locs_to_offset$area_new_builds[idx]
  locs_to_offset$farmland_area[idx] <- locs_to_offset$farmland_area[idx] - locs_to_offset$offset[idx]
  
  bio_area_perc <- locs_to_offset$offset[idx] / (400)
  locs_to_offset$percent_frm[idx] <- round(locs_to_offset$percent_frm[idx] - bio_area_perc, 9)
  
  if (offset_lc == 'mixed'){
    locs_to_offset$percent_grs[idx] <- round(locs_to_offset$percent_grs[idx] + (bio_area_perc / 2), 9)
    locs_to_offset$percent_wod[idx] <- round(locs_to_offset$percent_wod[idx] + (bio_area_perc / 2), 9)
  } else if (offset_lc == 'sng'){
    locs_to_offset$percent_grs[idx] <- round(locs_to_offset$percent_grs[idx] + (bio_area_perc), 9)
  } else {
    stop("The \'offset_lc\' argument can only assume values \'mixed\' or \'sng\'")
  }
  

  ## Case 2: There is some farmland available but not enough for biodiversity 
  ## compensation
  idx <- (locs_to_offset$area_new_builds > locs_to_offset$offset) &
    (locs_to_offset$farmland_area > 0)
  
  locs_to_offset$offset[idx] <- locs_to_offset$farmland_area[idx]
  locs_to_offset$farmland_area[idx] <- 0
  
  bio_area_perc <- locs_to_offset$offset[idx] / (400)
  locs_to_offset$percent_frm[idx] <- 0
  if (offset_lc == 'mixed'){
    locs_to_offset$percent_grs[idx] <- round(locs_to_offset$percent_grs[idx] + (bio_area_perc / 2), 9)
    locs_to_offset$percent_wod[idx] <- round(locs_to_offset$percent_wod[idx] + (bio_area_perc / 2), 9)
  } else if (offset_lc == 'sng'){
    locs_to_offset$percent_grs[idx] <- round(locs_to_offset$percent_grs[idx] + (bio_area_perc), 9)
  } else {
    stop("The \'offset_lc\' argument can only assume values \'mixed\' or \'sng\'")
  }
  
  ## Case 3: There is no farmland available for biodiversity compensation
  ## In this case, select the closest cell with available farmland, if possible in
  ## the same LPA. I use a radius of 30km just to make sure London is covered:
  ## some areas in London are central and far from any farmland. Such a large
  ## radius is only useful to find available areas around London; for all other
  ## locations, having a large buffer only slows computation, but the results are
  ## always adjacent or close cells (minimum distance to the cell from a subset
  ## of cells whose size depends on radius)
  
  
  idx <- (locs_to_offset$area_new_builds > locs_to_offset$offset)
  
  lpa_greater_london <- c("Barking and Dagenham", "Barnet", "Bexley", "Brent",
                          "Bromley", "Camden", "City of London", "Croydon",
                          "Ealing", "Enfield", "Greenwich", "Hackney",
                          "Hammersmith and Fulham", "Haringey", "Harrow",
                          "Havering", "Hillingdon", "Hounslow", "Islington",
                          "Kensington and Chelsea", "Kingston upon Thames",
                          "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge",
                          "Richmond upon Thames", "Southwark", "Sutton",
                          "Tower Hamlets", "Waltham Forest", "Wandsworth",
                          "Westminster")
  
  for (cell in locs_to_offset$new2kid[idx]){
    frm_needed <- locs_to_offset$area_new_builds[locs_to_offset$new2kid == cell] - 
      locs_to_offset$offset[locs_to_offset$new2kid == cell]
    self_cell <- locs_to_offset[locs_to_offset$new2kid == cell, ]
    closest_cells <- st_buffer(locs_to_offset[locs_to_offset$new2kid == cell, ], 
                               dist = 30000)
    ind_cells <- st_intersection(seer_2km, closest_cells)[,1]
    closest_cells <- locs_to_offset[locs_to_offset$new2kid %in% ind_cells$new2kid, ]
    # remove Greater London area, if applicable
    is_in_gr_london <- length(which(closest_cells$lad19nm %in% lpa_greater_london))
    if(is_in_gr_london != 0){
      closest_cells_idx <- which(closest_cells$lad19nm %in% lpa_greater_london)
      closest_cells <- closest_cells[-closest_cells_idx, ]
    }
    # is self_cell contained into closest_cells?
    is_contained <- length(which(closest_cells$new2kid %in% cell))
    if (is_contained != 0){
      closest_cells <- closest_cells[-which(closest_cells$new2kid %in% cell), ]
    }
    avail_cells <- closest_cells[(closest_cells$farmland_area > frm_needed), ]
    lpa_avail_cells <- avail_cells[avail_cells$lad19nm %in% self_cell$lad19nm, ]
    if (nrow(lpa_avail_cells) > 0){
      avail_cells <- lpa_avail_cells
    }
    
    avail_cells <- st_centroid(avail_cells)
    self_cell <- st_centroid(self_cell)
    # ind <- which(locs_to_offset$new2kid %in% avail_cells$new2kid)
    # avail_cells <- st_centroid(locs_to_offset[ind, ])
    # self_cell <- st_centroid(locs_to_offset[locs_to_offset$new2kid == cell, ])
    
    dist <- st_distance(avail_cells, self_cell)
    avail_cells <- avail_cells[which(dist == min(dist)), ]
    new_bio_cell <- avail_cells$new2kid[sample(nrow(avail_cells), 1)]
    
    bio_area_perc <- frm_needed / (400)
    ind <- locs_to_offset$new2kid == new_bio_cell
    locs_to_offset$offset[ind] <- locs_to_offset$offset[ind] + frm_needed
    locs_to_offset$farmland_area[ind] <- locs_to_offset$farmland_area[ind] - frm_needed
    locs_to_offset$percent_frm[ind] <- round(locs_to_offset$percent_frm[ind] - bio_area_perc, 9)
    if (offset_lc == 'mixed'){
      locs_to_offset$percent_grs[ind] <- round(locs_to_offset$percent_grs[ind] + (bio_area_perc / 2), 9)
      locs_to_offset$percent_wod[ind] <- round(locs_to_offset$percent_wod[ind] + (bio_area_perc / 2), 9)
    } else if (offset_lc == 'sng'){
      locs_to_offset$percent_grs[ind] <- round(locs_to_offset$percent_grs[ind] + (bio_area_perc), 9)
    } else {
      stop("The \'offset_lc\' argument can only assume values \'mixed\' or \'sng\'")
    }
  }
  
  offset <- check_output(locs_to_offset, offset_lc)
  
  if (saveondisk == FALSE){
    return(offset)
  } else {
    st_write(offset, output_path)
    return(offset)
  }
}

## ORDERING ALGORITHM
## ==================
maximise_target <-  function(locs_to_offset, locs_to_target, offset_lc, decreasing, output_path, saveondisk){

  locs_to_offset$offset <- 0

  locs_to_target <- merge(seer_2km, locs_to_target, by = 'new2kid')
  offset_farmland <- locs_to_offset[,c(1,which(colnames(locs_to_offset) == 'farmland_area')), drop=TRUE]
  locs_to_target <- merge(locs_to_target, offset_farmland, by='new2kid')

  # reorder the cells based on the 'decreasing' function argument
  sort_idx <- sort(locs_to_target$target, decreasing = decreasing, index.return = TRUE)[[2]]
  locs_to_target <- locs_to_target[sort_idx, ]
  # idx_positive_target <- sum(locs_to_target$target > 0)
  # reshuffle_vector <- c(idx_positive_target:nrow(locs_to_target))
  # resampled_vector <- sample(reshuffle_vector)
  # locs_to_target[reshuffle_vector, ] <- locs_to_target[resampled_vector,]

  # Tot offset area = tot area of new buildings
  tot_offset <- sum(locs_to_offset$area_new_builds)
  offset_proj <- locs_to_offset$area_new_builds[locs_to_offset$area_new_builds > 0]
  sort_idx <- sort(offset_proj, decreasing = TRUE, index.return = TRUE)[[2]]
  offset_proj <- offset_proj[sort_idx]

  # ordering routine
  for (i in c(1:length(offset_proj))){
    area_to_offset <- offset_proj[i]
    enough_offset_idx <- which(locs_to_target$farmland_area >= area_to_offset)[1]
    offset_cell_id <- locs_to_target$new2kid[enough_offset_idx]
    idx = which(locs_to_offset$new2kid == offset_cell_id)
    locs_to_offset$offset[idx] <- area_to_offset
    locs_to_offset$farmland_area[idx] <- locs_to_offset$farmland_area[idx] - area_to_offset
    offset_area_perc <- area_to_offset / 400 # CORRECT TO /400 TO CORRECT THE % ?
    locs_to_offset$percent_frm[idx] <- locs_to_offset$percent_frm[idx] - offset_area_perc
    if (offset_lc == 'mixed'){
      locs_to_offset$percent_grs[idx] <- round(locs_to_offset$percent_grs[idx] + 0.5 * offset_area_perc, 9)
      locs_to_offset$percent_wod[idx] <- round(locs_to_offset$percent_wod[idx] + 0.5 * offset_area_perc, 9)
    } else if (offset_lc == 'sng'){
      locs_to_offset$percent_grs[idx] <- round(locs_to_offset$percent_grs[idx] + offset_area_perc, 9)
    } else {
      stop("The \'offset_lc\' argument can only assume values \'mixed\' or \'sng\'")
    }
    locs_to_target <- locs_to_target[-enough_offset_idx,]
  }

  offset <- check_output(locs_to_offset, offset_lc)

  if (saveondisk == FALSE){
    return(offset)
  } else {
    st_write(offset, output_path)
    return(offset)
  }
}

# ## ORDERING ALGORITHM VERSION 2
# ## ============================
# maximise_target <-  function(locs_to_offset, locs_to_target, decreasing, output_path, saveondisk){
# 
#   locs_to_offset$offset <- 0
# 
#   locs_to_target <- merge(seer_2km, locs_to_target, by = 'new2kid')
#   offset_farmland <- locs_to_offset[,c(1,which(colnames(locs_to_offset) == 'farmland_area')), drop=TRUE]
#   locs_to_target <- merge(locs_to_target, offset_farmland, by='new2kid')
# 
#   # reorder the cells based on the 'decreasing' function argument
#   sort_idx <- sort(locs_to_target$target, decreasing = decreasing, index.return = TRUE)[[2]]
#   locs_to_target <- locs_to_target[sort_idx, ]
#   idx_positive_target <- sum(locs_to_target$target > 0)
#   reshuffle_vector <- c(idx_positive_target:nrow(locs_to_target))
#   resampled_vector <- sample(reshuffle_vector)
#   locs_to_target[reshuffle_vector, ] <- locs_to_target[resampled_vector,]
# 
#   # Tot offset area = tot area of new buildings
#   tot_offset <- sum(locs_to_offset$area_new_builds)
#   offset_proj <- locs_to_offset[locs_to_offset$area_new_builds > 0,c('new2kid', 'farmland_area', 'area_new_builds')]
#   sort_idx <- sort(offset_proj$farmland_area, decreasing = TRUE, index.return = TRUE)[[2]]
#   offset_proj <- offset_proj[sort_idx,]
# 
#   # ordering routine
#   for (i in c(1:nrow(locs_to_target))){
#     avail_area <- locs_to_target$farmland_area[i]
#     if (nrow(offset_proj)>0){
#       area_diff <-  avail_area - offset_proj$area_new_builds
#       fitting_offset_idx <- which.min(replace(area_diff, area_diff<0, NA))
#       if (length(fitting_offset_idx) == 0){
#         fitting_offset_idx <- 1
#       }
#       offset_cell_id <- locs_to_target$new2kid[i]
#       idx = which(locs_to_offset$new2kid == offset_cell_id)
#       offset_area <- offset_proj$area_new_builds[fitting_offset_idx]
#       locs_to_offset$offset[idx] <- offset_area
#       locs_to_offset$farmland_area[idx] <- locs_to_offset$farmland_area[idx] - offset_area
#       offset_area_perc <- offset_area / 400 # CORRECT TO /400 TO CORRECT THE % ?
#       locs_to_offset$percent_frm[idx] <- locs_to_offset$percent_frm[idx] - offset_area_perc
#       locs_to_offset$percent_grs[idx] <- locs_to_offset$percent_grs[idx] + 0.5 * offset_area_perc
#       locs_to_offset$percent_wod[idx] <- locs_to_offset$percent_wod[idx] + 0.5 * offset_area_perc
#       offset_proj <- offset_proj[-fitting_offset_idx,]
#     } else {
#       break
#     }
#   }
#   offset <- check_output(locs_to_offset)
# 
#   if (saveondisk == FALSE){
#     return(offset)
#   } else {
#     st_write(offset, output_path)
#     return(offset)
#   }
# }