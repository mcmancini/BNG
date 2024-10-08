## fcn_plt_map.R
## =============
##
## Author: Mattia C. Mancini
## Created: 16-Mar-2021
## Last modified: 16-Mar-2021
## --------------------------
##
## Map a spatial sf dataframe, specifying what column of the data to be mapped,
## how to split the data into classes, the title of the plot and whether to
## include the legend. 
## 
## DETAILS
## - Data: A spatial sf dataframe
## - column_name: the exact name of the column in the dataframe to be plotted
## - classes: 'default' or a specified numeric vector containing the breaks that
##            separate the wantted classes
## = plot_title: a character string
## - plot_legend: can assume values indicating the position of the legend as in 
##                ggplot theme(legend.position = ....) argument, e.g. 'right', 
##                'bottom', or 'none'
## - scale: one of the scale_fill_viridis options
## =============================================================================
require(ggplot2)
require(viridis)

fcn_plt_map <- function(data, 
                        column_name, 
                        classes, 
                        plot_title,
                        legend_title,
                        plot_legend, 
                        scale, 
                        border_col,
                        direction){
  eng_border <- st_read("C:/Users/mcm216/OneDrive - University of Exeter/Data/BNG/Data/SEER_GRID/england_full_clipped.shp") 
  
  eng_border_buff <- st_simplify(eng_border,preserveTopology = FALSE, dTolerance = 5000) %>% 
    st_buffer(dist = 10000)
  
  # df <- data[, c('new2kid', column_name)]
  df <- data
  
  # legend
  legend_position <- plot_legend
  
  # Set low and high bounds, and split the data into categories for plotting
  ind <- which(colnames(df) == column_name)
  low_bound <- min(df[, ind, drop = TRUE], na.rm = TRUE)
  high_bound <- max(df[, ind, drop = TRUE], na.rm = TRUE)
  
  if (is.character(classes) == TRUE){
    no_classes <- 8
    quantiles <- seq(low_bound, high_bound, (high_bound - low_bound)/(no_classes - 2))
  } else {
    no_classes <- length(classes)
    quantiles <- classes
  }
  
  labels <- c()
  for(idx in 1:length(quantiles)){
    labels <- c(labels, paste0(round(quantiles[idx], digits = 3), 
                               " - ", round(quantiles[idx + 1], digits = 3)))
  }
  labels <- labels[1:length(labels)-1]
  
  df$bins <- cut(df[, ind, drop = TRUE], 
                 breaks = quantiles, 
                 labels = labels, 
                 include.lowest = T)
  
  plt <- ggplot() +
    geom_sf(data = eng_border, fill = border_col, colour = border_col, lwd = 3) +
    geom_sf(data = eng_border, fill = 'grey85', colour = 'grey85', lwd = 1) +
    geom_sf(data = df, 
            aes(fill = bins), 
            color = NA) +
    scale_fill_viridis(
      option = scale,
      discrete = T,
      direction = direction,
      drop = TRUE,
      na.value = "white",
      guide = guide_legend(
        title =  legend_title,
        title.position = 'top',
        reverse = F, 
        nrow=4,
        byrow=TRUE)) + 
    ggtitle(plot_title) +
    theme(plot.title = element_text(size=11, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11, hjust = 0.5),
          legend.position = legend_position)
  
  #return
  return(plt)
}


fcn_continuous_plot <- function(plot_data, 
                                column, 
                                limits, 
                                plot_title, 
                                legend_title, 
                                legend_position, 
                                scale,
                                border_col,
                                direction) {
  df <- plot_data
  eng_border <- st_read("C:/Users/mcm216/OneDrive - University of Exeter/Data/BNG/Data/SEER_GRID/england_full_clipped.shp") 
  

  formatLabel <- function(x) format(round(x, 3), nsmall = 2)
  formatIntegers <- function(x) format(x, scientific = 999)
  labels = parse(text=paste(seq(0, 300, 100), "~ha"))
  
  plot <- ggplot() +
    geom_sf(data = eng_border, fill = border_col, colour = border_col, lwd = 3) +
    geom_sf(data = eng_border, fill = 'grey85', colour = 'grey85', lwd = 0.8) +
    geom_sf(data = df,
            aes(fill = get(column), color = get(column))) +
    scale_fill_viridis(legend_title, 
                       labels = labels, 
                       option = scale, 
                       limits = limits, 
                       discrete = FALSE,
                       direction = direction, 
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black",
                                               title.position = "top",
                                               title.theme = element_text(size = 8), 
                                               label.position = "bottom", 
                                               label.theme = element_text(angle = 0, vjust = 0, hjust=0, size = 8),
                                               barwidth = unit(40, units ="mm"),
                                               barheight = unit(5, units ="mm"))) +
    scale_color_viridis(legend_title, 
                        labels = labels, 
                        option = scale, 
                        limits = limits, 
                        discrete = FALSE,
                        direction = direction) +
    ggtitle(plot_title) +
    theme_bw() +
    theme(panel.border = element_blank(), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) +
    coord_sf(datum = NA) +
    theme(plot.title = element_text(size=11, hjust = 0, face = "bold"),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11, hjust = 0.5),
          legend.position = legend_position)  
          # legend.key.height=unit(5,"mm"),
          # legend.key.width = unit(30, "mm"))
    
  
  return(plot)
}


fcn_change_plot <- function(plot_data, 
                            column, 
                            limits, 
                            plot_title, 
                            legend_title, 
                            legend_position) {
  eng_border <- st_read("C:/Users/mcm216/OneDrive - University of Exeter/Data/BNG/Data/SEER_GRID/england_full_clipped.shp")
  
  formatLabel <- function(x) format(round(x, 3), nsmall = 2)
  formatIntegers <- function(x) format(x, scientific = 999)

  plot <- ggplot() +
    geom_sf(data = eng_border, fill = border_col, colour = border_col, lwd = 3) +
    geom_sf(data = eng_border, fill = 'grey85', colour = 'grey85', lwd = 1) +
    geom_sf(data = plot_data,
            aes(fill = get(column), color = get(column))) +
    scale_fill_gradient2(low = 'red3',
                         mid = 'white',
                         high = 'blue4',
                         # labels = formatLabel, 
                         limits = limits) +
    scale_color_gradient2(low = 'red3',
                          mid = 'white',
                          high = 'blue4',
                          # labels = formatLabel, 
                          limits = limits) +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size=11, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11, hjust = 0.5),
          legend.position = legend_position)
  return(plot)
}
