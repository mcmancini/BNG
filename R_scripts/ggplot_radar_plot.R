# https://r-charts.com/ranking/ggradar/
# https://exts.ggplot2.tidyverse.org/ggradar.html


install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar")
library(ggradar)

# format data 

radar_df_benefits <- all_data %>%
  dplyr::filter(result_file == "benefits") %>%
  group_by(scenario) %>%
  dplyr::summarise(other_ben = (sum(total)-sum(rec)),
                   tot_rec = sum(rec))

radar_df_es_benefits <- all_data %>% 
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
        plot.legend = FALSE
        ) +
  theme(
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
    legend.position = "bottom", 
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

