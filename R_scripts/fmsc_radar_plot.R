## =============================================================================
## 1.0. Radar plot 
## ===============

# https://r-graph-gallery.com/spider-or-radar-chart.html

## 1.1. load packages
## ------------------
library(fmsb)
library(scales)
library(RColorBrewer)
library(gridGraphics)
library(grid)

## 1.2. format data
## ----------------

# data format = specific see: https://r-graph-gallery.com/142-basic-radar-chart.html
# https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/ 
# vars of interest: total (benefit), bio, total (cost), rec

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
  magrittr::set_rownames(.$scenario) %>%
  as.data.frame() %>% 
  dplyr::select(-scenario)

radar_df_all_es <- full_join(radar_df_es_benefits, radar_df_bio, by = "scenario") %>% 
  full_join(radar_df_cost, by = "scenario") %>% 
  dplyr::filter(scenario == "local_offset"|
                  scenario == "max_bio" |
                  scenario == "max_es" |
                  scenario == "max_rec_equity_weighted" |
                  scenario == "min_cost") %>%
  magrittr::set_rownames(.$scenario) %>%
  as.data.frame() %>% 
  dplyr::select(-scenario)

# scale so all vars are on the same scale 
df_scaled <- round(apply(radar_df, 2, scales::rescale), 2)
df_scaled <- as.data.frame(df_scaled)
head(df_scaled)

df_scaled_all_es <- round(apply(radar_df_all_es, 2, scales::rescale), 2)
df_scaled_all_es <- as.data.frame(df_scaled_all_es)
head(df_scaled_all_es)


## 1.3. plot
## ---------

# Set graphic colors
coul <-viridis(4,option = "turbo",) 
colors_border <- coul
colors_in <- alpha(coul,0.3)

# https://www.geeksforgeeks.org/how-to-use-par-function-in-r/#:~:text=The%20par()%20function%20is,create%20multiple%20plots%20at%20once.
# https://r-charts.com/ranking/radar-chart/ 
op <- par()
# par(mar = c(8,0,0,0))
# https://bookdown.org/ndphillips/YaRrr/plot-margins.html 
par(mfrow = c(1,1))
par(xpd = TRUE)

# pdf(file = paste0(gitpath,"Output/Figures/Radar_plot_4_dim.pdf"),
#     width = 4.48, # in inches
#     height = 4.38)

radarchart(df_scaled, 
           axistype=0 , 
           maxmin=F,
           pty = 32,
           centerzero=F,
           seg = 4, 
           
           #custom polygon
           pcol=colors_border , #colours of the line
           pfcol=colors_in , #colour fill 
           plwd=1 , #width of the line
           plty=1, #line type 
           
           #custom the grid
           cglcol="grey", #grid line colour
           cglty=1, # grid line type
           axislabcol="black", 
           cglwd=1, # grid line width
           
           #custom labels
           # vlcex=1, 
           vlabels = c("Other benefits", "Recreation", "Biodiversity", "Cost")
) 


legend("bottom", 
       inset = c(-0, -0.4),
       legend = c("Local offset", "Maximum biodiversity", "Maximum ecosystem services", "Equity weighted", "Minimum cost"),
       bty = "n", # box around legend
       pch=0 , #plotting symbol - https://www.datanovia.com/en/blog/pch-in-r-best-tips/
       col=colors_border,
       pt.cex = 2,
       text.col = "white")

# add legand fill 
legend("bottom", 
       inset = c(-0, -0.4),
       legend = c("Local offset", "Maximum biodiversity", "Maximum ecosystem services", "Equity weighted", "Minimum cost"),
       bty = "n", # box around legend
       pch=15 , #plotting symbol - https://www.datanovia.com/en/blog/pch-in-r-best-tips/
       col=colors_in,
       pt.cex = 2)

# dev.off() # create the file

par(op)

# https://r-charts.com/ranking/ggradar/ - to try again in ggplot 

# Produce radar plots showing both the average profile and the individual profile:

# Variables summary
# Get the minimum and the max of every column  
col_max <- apply(df_scaled, 2, max)
col_min <- apply(df_scaled, 2, min)
# Calculate the average profile 
col_mean <- apply(df_scaled, 2, mean)
# Put together the summary of columns
col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))

df_scaled2 <- as.data.frame(rbind(col_summary, df_scaled))
head(df_scaled2)

radarchart(df_scaled2[-c(1:3),], axistype=0 , maxmin=F, seg = 6, pty = 32,
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=1 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
           #custom labels
           vlcex=0.8, 
           vlabels = c("Benefits", "Recreation", "Biodiversity", "Costs")) 


legend("bottom", legend = rownames(df_scaled2[-c(1,2),]), bty = "n", pch=20 , col=colors_in)

# Set graphic colors
coul <-viridis(1,option = "turbo",) # the yellow is hard to read so we want to skip it 
colors_border <- coul
colors_in <- alpha(coul,0.3)

opar <- par() 
# Define settings for plotting in a 3x4 grid, with appropriate margins:
par(mar = rep(0.8,4))
par(mfrow = c(2,2))
# Produce a radar-chart for each student
for (i in 4:nrow(df_scaled2)) {
  radarchart(
    df_scaled2[c(1:3, i), ], 
    # point style
    pty = 32,
    #custom polygon
    pfcol=c("#99999980", colors_in), 
    pcol=c(NA,colors_border), plwd=1 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
    #custom labels
    vlcex=0.8, 
    # vlabels = c("Benefits", "Recreation", "Biodiversity", "Costs"),
    title = row.names(df_scaled2)[i]
  )
}
# Restore the standard par() settings
par <- par(opar) 

