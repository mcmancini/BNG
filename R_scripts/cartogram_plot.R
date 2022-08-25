
## =============================================================================
## 1.0. Cartogram
## ==============

## 1.1. load packages
## ------------------
library(cartogram)

## 1.2. Benefit tables
## -------------------
setwd('D:/Documents/GitHub/biodiversity-net-gain/CSV Files/benefits')
benefits <- all_data %>%
  dplyr::filter(scenario == "max_rec_equity_weighted" |scenario == "max_es") %>%
  dplyr::filter(result_file == "benefits") %>%
  dplyr::select(new2kid, total, hectares_chg, scenario)

benefits <- merge(seer_2km, benefits, by='new2kid')

## 1.3. Regions in England
## -----------------------
regions_shp <- st_read(paste0(datapath,'Data/Regions/Regions_(December_2017)_Boundaries.shp'))[, 3]

## 1.4. Census for number of households and median income by cell in England
## -------------------------------------------------------------------------
num_hh <- read.csv(paste0(datapath,'Data/SEER_SOCIO_ECON/SEER_2k_socioecon_eng.csv'))[, c(1, 4:5)]
cell_idx <- benefits[, 'new2kid', drop=TRUE]
num_hh <- num_hh[num_hh$new2kid %in% cell_idx,]
benefits <- merge(benefits, num_hh, by = 'new2kid', all = TRUE)

## 1.5. Merge seer grid and benefits and remove areas outside England
## ------------------------------------------------------------------
benefits <- st_join(benefits, regions_shp, largest=TRUE)
# save locally as this takes a while to run
st_write(benefits,"C:/Data/cartogram_benefits_data.shp")
benefits <- st_read("C:/Data/cartogram_benefits_data.shp") %>%
  dplyr::rename(hectares_chg =  hctrs_c,
                scenario = scenari,
                Households = Hoshlds,
                Med_hh_inc = Md_hh_n)

## 1.6. AGGREGATION BY REGION - NEED TO CORRECT WEIGHTING VARS
## --------------------------
df1 <- benefits %>%
  st_drop_geometry() %>%
  dplyr::group_by(rgn17nm) %>%
  dplyr::summarise(mean_med_hh_income = mean(Med_hh_inc)) %>%
  dplyr::rename(Region = rgn17nm,
                Income = mean_med_hh_income)

df2 <- benefits %>%
  st_drop_geometry() %>%
  dplyr::filter(scenario == "max_es") %>%
  dplyr::group_by(rgn17nm) %>%
  dplyr::summarise(max_es_total_es_per_hh = sum(total)/sum(Households)) %>%
  dplyr::rename(Region = rgn17nm)

df3 <- benefits %>%
  st_drop_geometry() %>%
  dplyr::filter(scenario == "max_rec_equity_weighted") %>%
  dplyr::group_by(rgn17nm) %>%
  dplyr::summarise(max_rec_total_es_per_hh = sum(total)/sum(Households)) %>%
  dplyr::rename(Region = rgn17nm)

region_aggr <- full_join(df1,df2) %>%
  full_join(df3)

rm(df1, df2, df3)

regions <- merge(regions_shp, region_aggr, by.x='rgn17nm', by.y='Region')

regions[which(regions$rgn17nm == 'Yorkshire and The Humber'), 'rgn17nm'] <- 'Yorkshire'

## 1.7. Plot
## ---------
cartogram_pop <- cartogram_cont(regions, weight = 'max_es_total_es_per_hh', itermax = 30, prepare = 'none')
cartogram_mui <- cartogram_cont(regions, weight = 'max_rec_total_es_per_hh', itermax = 50, prepare = 'none')


eng_income <- ggplot(data = regions, aes(x=reorder(rgn17nm, Income),y=Income)) +
  geom_bar(position="dodge",stat="identity", fill='dodgerblue4', width = 0.6) +
  coord_flip() +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text = element_text(size = 16)) +
  labs(y='Average income by region')


eng_pop <- ggplot(data = cartogram_pop) +
  geom_sf(aes(fill = max_es_total_es_per_hh)) +
  scale_fill_viridis(option = "inferno",
                     trans = 'sqrt',
                     direction = -1,
                     limits = c(8, 24),
                     labels = function(x) paste0("£", seq(8, 24, 4))) +
  theme_bw() +
  ggtitle('a') +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.width=unit(1,"in")) +
  coord_sf(datum = NA)

eng_mui <- ggplot(data = cartogram_mui) +
  geom_sf(aes(fill = max_rec_total_es_per_hh)) +
  scale_fill_viridis(option = "inferno",
                     trans = 'sqrt',
                     direction = -1,
                     limits = c(8, 24),
                     labels = function(x) paste0("£", seq(8, 24, 4))) +
  theme_bw() +
  ggtitle('b') +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 30, hjust = 0.5, vjust = 0.5),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.width=unit(1,"in")) +
  coord_sf(datum = NA)

cartograms <- ggarrange(eng_pop, eng_mui,
                        ncol = 2, nrow = 1,
                        common.legend = TRUE,
                        legend = 'bottom')

figure <- ggarrange(eng_income, cartograms, ncol = 2, nrow = 1)

# plot_title <- 'Regional effects of equity weighting on average household\nbenefits from biodiversity improvements'
# figure <- annotate_figure(figure,
#                           top = text_grob(plot_title, color = "black", face = "bold", size = 32))

# save_path <- pasteo(gitpath, "Output/Maps/")
# filename <- 'Regional_WTP_mui.jpeg'
# ggsave(filename=filename, plot = figure, device = "jpeg",
#        path = save_path, units = "in", width = 12, height = 5)

