library(terra)
library(raster)
library(ggplot2)
library(rnaturalearthdata)
library(rnaturalearth)
library(maps)
library(mapproj)
library(ggthemes)
library(sf)
library(cowplot)


#last step is making the plots of the initial distributions and present day ENMs (both training extent and full prediction to US)

sp_ls <- c("ABMA", "ANBO", "ANHE", "LISY", "PSMA", "RALU")

#load initial distribution, initial southern distribution, and then load in the ENM
ABMA_ini <- raster('./outputs/maxent/ABMA_ini.tif')
ANBO_ini <- raster('./outputs/maxent/ANBO_ini.tif')
ANHE_ini <- raster('./outputs/maxent/ANHE_ini.tif')
LISY_ini <- raster('./outputs/maxent/LISY_ini.tif')
PSMA_ini <- raster('./outputs/maxent/PSMA_ini.tif')
RALU_ini <- raster('./outputs/maxent/RALU_ini.tif')

ABMA_ini_south <- raster('./outputs/maxent/ABMA_ini_south.tif')
ANBO_ini_south <- raster('./outputs/maxent/ANBO_ini_south.tif')
ANHE_ini_south <- raster('./outputs/maxent/ANHE_ini_south.tif')
LISY_ini_south <- raster('./outputs/maxent/LISY_ini_south.tif')
PSMA_ini_south <- raster('./outputs/maxent/PSMA_ini_south.tif')
RALU_ini_south <- raster('./outputs/maxent/RALU_ini_south.tif')

#this is the one that used the reduced range
ABMA_ini_SDM <- raster('./outputs/maxent/rasters/ABMA_ini_cont.tif')
ANBO_ini_SDM <- raster('./outputs/maxent/rasters/ANBO_ini_cont.tif')
ANHE_ini_SDM <- raster('./outputs/maxent/rasters/ANHE_ini_cont.tif')
LISY_ini_SDM <- raster('./outputs/maxent/rasters/LISY_ini_cont.tif')
PSMA_ini_SDM <- raster('./outputs/maxent/rasters/PSMA_ini_cont.tif')
RALU_ini_SDM <- raster('./outputs/maxent/rasters/RALU_ini_cont.tif')

#this one instead predicts to all of US for habitat suitability
ABMA_present_SDM <- raster('./outputs/maxent/rasters/ssp245/ABMA_hs1.tif')
ANBO_present_SDM <- raster('./outputs/maxent/rasters/ssp245/ANBO_hs1.tif')
ANHE_present_SDM <- raster('./outputs/maxent/rasters/ssp245/ANHE_hs1.tif')
LISY_present_SDM <- raster('./outputs/maxent/rasters/ssp245/LISY_hs1.tif')
PSMA_present_SDM <- raster('./outputs/maxent/rasters/ssp245/PSMA_hs1.tif')
RALU_present_SDM <- raster('./outputs/maxent/rasters/ssp245/RALU_hs1.tif')

#load in the counties for mapping
world <- ne_countries(scale = "medium", returnclass = "sf")

#reproject world map
world_repro <- st_transform(world, CRS("ESRI:102008"))

#make a dataframe for each ini as well as model train and predicted maps

ini_list <- list()
for (i in sp_ls) {
  df <- as.data.frame(get(paste(i,"_ini", sep='')), xy=TRUE) 
  ini_list[[i]] <- df
}

ini_south_list <- list()
for (i in sp_ls) {
  df <- as.data.frame(get(paste(i,"_ini_south", sep='')), xy=TRUE)
  ini_south_list[[i]] <- df
}

best_model_pred_list <- list()
for (i in sp_ls) {
  df <- as.data.frame(get(paste(i,"_ini_SDM", sep='')), xy = TRUE)
  best_model_pred_list[[i]] <- df
}

present_pred_list <- list()
for (i in sp_ls) {
  df <- as.data.frame(get(paste(i,"_present_SDM", sep='')), xy = TRUE)
  present_pred_list[[i]] <- df
}

#going to create the initial distribution maps first
#note that the species list doesn't have the full name for the titles, so going to brute force this with a bunch of lines of code


# initial_distribution ----------------------------------------------------
ABMA_ini_map <- ggplot() + 
  geom_raster(data = ini_list$ABMA, aes(x = x, y = y, fill = as.factor(ABMA_ini))) + 
  scale_fill_manual(values = c('white','steelblue4'),
                    labels = c('','Present'),
                    na.translate=FALSE) +
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Ambystoma macrodactylum") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

ANBO_ini_map <- ggplot() + 
  geom_raster(data = ini_list$ANBO, aes(x = x, y = y, fill = as.factor(ANBO_ini))) + 
  scale_fill_manual(values = c('white','steelblue4'),
                    labels = c('','Present'),
                    na.translate=FALSE) +
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Anaxyrus boreas") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

ANHE_ini_map <- ggplot() + 
  geom_raster(data = ini_list$ANHE, aes(x = x, y = y, fill = as.factor(ANHE_ini))) + 
  scale_fill_manual(values = c('white','steelblue4'),
                    labels = c('','Present'),
                    na.translate=FALSE) +
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Anaxyrus hemiophrys") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

LISY_ini_map <- ggplot() + 
  geom_raster(data = ini_list$LISY, aes(x = x, y = y, fill = as.factor(LISY_ini))) + 
  scale_fill_manual(values = c('white','steelblue4'),
                    labels = c('','Present'),
                    na.translate=FALSE) +
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Lithobates sylvaticus") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

PSMA_ini_map <- ggplot() + 
  geom_raster(data = ini_list$PSMA, aes(x = x, y = y, fill = as.factor(PSMA_ini))) + 
  scale_fill_manual(values = c('white','steelblue4'),
                    labels = c('','Present'),
                    na.translate=FALSE) +
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Pseudacris maculata") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

RALU_ini_map <- ggplot() + 
  geom_raster(data = ini_list$RALU, aes(x = x, y = y, fill = as.factor(RALU_ini))) + 
  scale_fill_manual(values = c('white','steelblue4'),
                    labels = c('','Present'),
                    na.translate=FALSE) +
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Rana luteiventris") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

legend_b <- get_legend(
  RALU_ini_map)

plot_final <- plot_grid(ABMA_ini_map, ANBO_ini_map, ANHE_ini_map, LISY_ini_map, PSMA_ini_map, RALU_ini_map, labels = c('','', '', '', '', ''), label_size = 2, ncol = 3, rel_heights = c(1,1,1,1,1,1))

ggsave("./outputs/supp_figure2.png", plot = plot_final, 
       width = 30, height = 50, units = "cm", dpi = 600)