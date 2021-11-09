library(terra)
library(raster)
library(rasterVis)
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

ABMA_ini <- aggregate(ABMA_ini, fact=8, fun = modal)
ANBO_ini <- aggregate(ANBO_ini, fact=8, fun = modal)
ANHE_ini <- aggregate(ANHE_ini, fact=8, fun = modal)
LISY_ini <- aggregate(LISY_ini, fact=8, fun = modal)
PSMA_ini <- aggregate(PSMA_ini, fact=8, fun = modal)
RALU_ini <- aggregate(RALU_ini, fact=8, fun = modal)

ABMA_ini_south <- raster('./outputs/maxent/ABMA_ini_south.tif')
ANBO_ini_south <- raster('./outputs/maxent/ANBO_ini_south.tif')
ANHE_ini_south <- raster('./outputs/maxent/ANHE_ini_south.tif')
LISY_ini_south <- raster('./outputs/maxent/LISY_ini_south.tif')
PSMA_ini_south <- raster('./outputs/maxent/PSMA_ini_south.tif')
RALU_ini_south <- raster('./outputs/maxent/RALU_ini_south.tif')

ABMA_ini_south <- aggregate(ABMA_ini_south, fact=8, fun = modal)
ANBO_ini_south <- aggregate(ANBO_ini_south, fact=8, fun = modal)
ANHE_ini_south <- aggregate(ANHE_ini_south, fact=8, fun = modal)
LISY_ini_south <- aggregate(LISY_ini_south, fact=8, fun = modal)
PSMA_ini_south <- aggregate(PSMA_ini_south, fact=8, fun = modal)
RALU_ini_south <- aggregate(RALU_ini_south, fact=8, fun = modal)

#this is the one that used the reduced range
ABMA_ini_SDM <- raster('./outputs/maxent/rasters/ABMA_ini_cont.tif')
ANBO_ini_SDM <- raster('./outputs/maxent/rasters/ANBO_ini_cont.tif')
ANHE_ini_SDM <- raster('./outputs/maxent/rasters/ANHE_ini_cont.tif')
LISY_ini_SDM <- raster('./outputs/maxent/rasters/LISY_ini_cont.tif')
PSMA_ini_SDM <- raster('./outputs/maxent/rasters/PSMA_ini_cont.tif')
RALU_ini_SDM <- raster('./outputs/maxent/rasters/RALU_ini_cont.tif')

ABMA_ini_SDM <- aggregate(ABMA_ini_SDM, fact=8, fun = modal)
ANBO_ini_SDM <- aggregate(ANBO_ini_SDM, fact=8, fun = modal)
ANHE_ini_SDM <- aggregate(ANHE_ini_SDM, fact=8, fun = modal)
LISY_ini_SDM <- aggregate(LISY_ini_SDM, fact=8, fun = modal)
PSMA_ini_SDM <- aggregate(PSMA_ini_SDM, fact=8, fun = modal)
RALU_ini_SDM <- aggregate(RALU_ini_SDM, fact=8, fun = modal)

#this one instead predicts to all of US for habitat suitability
ABMA_present_SDM <- raster('./outputs/maxent/rasters/ssp245/ABMA_hs1.tif')
ANBO_present_SDM <- raster('./outputs/maxent/rasters/ssp245/ANBO_hs1.tif')
ANHE_present_SDM <- raster('./outputs/maxent/rasters/ssp245/ANHE_hs1.tif')
LISY_present_SDM <- raster('./outputs/maxent/rasters/ssp245/LISY_hs1.tif')
PSMA_present_SDM <- raster('./outputs/maxent/rasters/ssp245/PSMA_hs1.tif')
RALU_present_SDM <- raster('./outputs/maxent/rasters/ssp245/RALU_hs1.tif')

ABMA_present_SDM <- aggregate(ABMA_present_SDM, fact=8, fun = modal)
ANBO_present_SDM <- aggregate(ANBO_present_SDM, fact=8, fun = modal)
ANHE_present_SDM <- aggregate(ANHE_present_SDM, fact=8, fun = modal)
LISY_present_SDM <- aggregate(LISY_present_SDM, fact=8, fun = modal)
PSMA_present_SDM <- aggregate(PSMA_present_SDM, fact=8, fun = modal)
RALU_present_SDM <- aggregate(RALU_present_SDM, fact=8, fun = modal)

#load in the counties for mapping
world <- ne_countries(scale = "medium", returnclass = "sf")

#reproject world map
world_repro <- st_transform(world, CRS("ESRI:102008"))

#make a dataframe for each ini as well as model train and predicted maps

#note that doing this in a list creates from very large objects
ini_list <- list()
for (i in sp_ls) {
  df <- as.data.frame(get(paste(i,"_ini", sep='')), xy=TRUE)
  ini_list[[i]] <- df
}

#very large. remove NA and break up.
ini_list <- lapply(ini_list, function(i){na.omit(i)})
names(ini_list) <- c("ABMA_ini_df", "ANBO_ini_df", "ANHE_ini_df", "LISY_ini_df", "PSMA_ini_df", "RALU_ini_df")
list2env(ini_list, .GlobalEnv)

#now the same thing but the south distribution
ini_south_list <- list()
for (i in sp_ls) {
  df <- as.data.frame(get(paste(i,"_ini_south", sep='')), xy=TRUE)
  ini_south_list[[i]] <- df
}
ini_south_list <- lapply(ini_south_list, function(i){na.omit(i)})
names(ini_south_list) <- c("ABMA_ini_south_df", "ANBO_ini_south_df", "ANHE_ini_south_df", "LISY_ini_south_df", "PSMA_ini_south_df", "RALU_ini_south_df")
list2env(ini_list, .GlobalEnv)

#next is the best model using species specific extent
best_model_pred_list <- list()
for (i in sp_ls) {
  df <- as.data.frame(get(paste(i,"_ini_SDM", sep='')), xy = TRUE)
  best_model_pred_list[[i]] <- df
}
best_model_pred_list <- lapply(best_model_pred_list, function(i){na.omit(i)})
names(best_model_pred_list) <- c("ABMA_ini_enm_df", "ANBO_ini_enm_df", "ANHE_ini_enm_df", "LISY_ini_enm_df", "PSMA_ini_enm_df", "RALU_ini_enm_df")
list2env(best_model_pred_list, .GlobalEnv)

#next is the prediction for whole of NA
present_pred_list <- list()
for (i in sp_ls) {
  df <- as.data.frame(get(paste(i,"_present_SDM", sep='')), xy = TRUE)
  present_pred_list[[i]] <- df
}
present_pred_list <- lapply(present_pred_list, function(i){na.omit(i)})
names(present_pred_list) <- c("ABMA_enm_df", "ANBO_enm_df", "ANHE_enm_df", "LISY_enm_df", "PSMA_enm_df", "RALU_enm_df")
list2env(present_pred_list, .GlobalEnv)

#going to create the initial distribution maps first
#note that the species list doesn't have the full name for the titles, so going to brute force this with a bunch of lines of code


# initial_distribution ----------------------------------------------------
ABMA_ini_map <- ggplot() + 
  geom_raster(data = ABMA_ini_df, aes(x = x, y = y, fill = as.factor(ABMA_ini))) + 
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
  geom_raster(data = ANBO_ini_df, aes(x = x, y = y, fill = as.factor(ANBO_ini))) + 
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
  geom_raster(data = ANHE_ini_df, aes(x = x, y = y, fill = as.factor(ANHE_ini))) + 
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
  geom_raster(data = LISY_ini_df, aes(x = x, y = y, fill = as.factor(LISY_ini))) + 
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
  geom_raster(data = PSMA_ini_df, aes(x = x, y = y, fill = as.factor(PSMA_ini))) + 
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
  geom_raster(data = RALU_ini_df, aes(x = x, y = y, fill = as.factor(RALU_ini))) + 
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
RALU_ini_map + 
    scale_fill_manual(name = "Distribution", 
                      values = c('white','steelblue4'),
                      labels = c('','Presence'),
                      na.translate=FALSE) +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
)

plot_legend <- plot_grid(legend_b, labels = c(''))

plot_maps <- plot_grid(ABMA_ini_map, ANBO_ini_map, ANHE_ini_map, LISY_ini_map, PSMA_ini_map, RALU_ini_map, labels = c('','', '', '', '', '',''), label_size = 2, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))

plot_final <- plot_grid(plot_legend, plot_maps, ncol = 1, rel_heights = c(.1,1))

ggsave("./outputs/supp_figure2.png", plot = plot_final, 
       width = 12, height = 9, dpi = 600)



# ini_south_dist --------------------------------------------------
ABMA_ini_map <- ggplot() + 
  geom_raster(data = ABMA_ini_south_df, aes(x = x, y = y, fill = as.factor(ABMA_ini_south))) + 
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
  geom_raster(data = ANBO_ini_south_df, aes(x = x, y = y, fill = as.factor(ANBO_ini_south))) + 
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
  geom_raster(data = ANHE_ini_south_df, aes(x = x, y = y, fill = as.factor(ANHE_ini_south))) + 
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
  geom_raster(data = LISY_ini_south_df, aes(x = x, y = y, fill = as.factor(LISY_ini_south))) + 
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
  geom_raster(data = PSMA_ini_south_df, aes(x = x, y = y, fill = as.factor(PSMA_ini_south))) + 
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
  geom_raster(data = RALU_ini_south_df, aes(x = x, y = y, fill = as.factor(RALU_ini_south))) + 
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
  RALU_ini_map + 
    scale_fill_manual(name = "Distribution", 
                      values = c('white','steelblue4'),
                      labels = c('','Presence'),
                      na.translate=FALSE) +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
)

plot_legend <- plot_grid(legend_b, labels = c(''))

plot_maps <- plot_grid(ABMA_ini_map, ANBO_ini_map, ANHE_ini_map, LISY_ini_map, PSMA_ini_map, RALU_ini_map, labels = c('','', '', '', '', '',''), label_size = 2, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))

plot_final <- plot_grid(plot_legend, plot_maps, ncol = 1, rel_heights = c(.1,1))

ggsave("./outputs/supp_figure3.png", plot = plot_final, 
       width = 12, height = 9, dpi = 600)


# ENM_best_mod ------------------------------------------------------------

#for reference, these are the colors from the points script
#scale_colour_manual(values = c("orange", "blue3", "grey35", "green4", "deeppink", "purple")) 

ABMA_ini_enm_map <- ggplot() + 
    geom_raster(data = ABMA_ini_enm_df, aes(x = x, y = y, fill = ABMA_ini_cont)) + 
    scale_fill_gradient(name = "ENM Value", low = "white",high = "orange",
    guide = "colourbar",
    aesthetics = "fill") + 
    geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Ambystoma macrodactylum") +
    ylim(-1100471, 4736542)+
    scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
    theme_classic(base_size = 15) +
    theme(plot.title = element_text(face="italic"), legend.position = "none")  

ANBO_ini_enm_map <- ggplot() + 
  geom_raster(data = ANBO_ini_enm_df, aes(x = x, y = y, fill = ANBO_ini_cont)) + 
  scale_fill_gradient(name = "ENM Value", low = "white",high = "blue3",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Anaxyrus boreas") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

ANHE_ini_enm_map <- ggplot() + 
  geom_raster(data = ANHE_ini_enm_df, aes(x = x, y = y, fill = ANHE_ini_cont)) + 
  scale_fill_gradient(name = "ENM Value", low = "white",high = "grey35",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Anaxyrus hemiophrys") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

LISY_ini_enm_map <- ggplot() + 
  geom_raster(data = LISY_ini_enm_df, aes(x = x, y = y, fill = LISY_ini_cont)) + 
  scale_fill_gradient(name = "ENM Value", low = "white",high = "green4",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Lithobates sylvaticus") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

PSMA_ini_enm_map <- ggplot() + 
  geom_raster(data = PSMA_ini_enm_df, aes(x = x, y = y, fill = PSMA_ini_cont)) + 
  scale_fill_gradient(name = "ENM Value", low = "white",high = "deeppink",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Pseudacris maculata") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

RALU_ini_enm_map <- ggplot() + 
  geom_raster(data = RALU_ini_enm_df, aes(x = x, y = y, fill = RALU_ini_cont)) + 
  scale_fill_gradient(name = "ENM Value", low = "white",high = "purple",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Rana luteiventris") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")


plot_final <- plot_grid(ABMA_ini_enm_map, ANBO_ini_enm_map, ANHE_ini_enm_map, LISY_ini_enm_map, PSMA_ini_enm_map, RALU_ini_enm_map, labels = c('','', '', '', '', '',''), label_size = 0, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))


ggsave("./outputs/supp_figure4_train.png", plot = plot_final, 
       width = 16, height = 10, dpi = 600)



# full_NA_pred ------------------------------------------------------------


ABMA_enm_map <- ggplot() + 
  geom_raster(data = ABMA_enm_df, aes(x = x, y = y, fill = ABMA_hs1)) + 
  scale_fill_gradient(name = "ENM Value", low = "white",high = "orange",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Ambystoma macrodactylum") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")  

ANBO_enm_map <- ggplot() + 
  geom_raster(data = ANBO_enm_df, aes(x = x, y = y, fill = ANBO_hs1)) + 
  scale_fill_gradient(name = "ENM Value", low = "white",high = "blue3",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Anaxyrus boreas") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

ANHE_enm_map <- ggplot() + 
  geom_raster(data = ANHE_enm_df, aes(x = x, y = y, fill = ANHE_hs1)) + 
  scale_fill_gradient(name = "ENM Value", low = "white",high = "grey35",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Anaxyrus hemiophrys") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

LISY_enm_map <- ggplot() + 
  geom_raster(data = LISY_enm_df, aes(x = x, y = y, fill = LISY_hs1)) + 
  scale_fill_gradient(name = "ENM Value", low = "white",high = "green4",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Lithobates sylvaticus") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

PSMA_enm_map <- ggplot() + 
  geom_raster(data = PSMA_enm_df, aes(x = x, y = y, fill = PSMA_hs1)) + 
  scale_fill_gradient(name = "ENM Value", low = "white",high = "deeppink",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Pseudacris maculata") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

RALU_enm_map <- ggplot() + 
  geom_raster(data = RALU_enm_df, aes(x = x, y = y, fill = RALU_hs1)) + 
  scale_fill_gradient(name = "ENM Value", low = "white",high = "purple",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Rana luteiventris") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")


plot_final <- plot_grid(ABMA_enm_map, ANBO_enm_map, ANHE_enm_map, LISY_enm_map, PSMA_enm_map, RALU_enm_map, labels = c('','', '', '', '', '',''), label_size = 0, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))


ggsave("./outputs/supp_figure5_pred.png", plot = plot_final, 
       width = 16, height = 10, dpi = 600)


