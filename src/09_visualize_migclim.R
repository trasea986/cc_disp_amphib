library(dismo)
library(tidyverse)
library(maptools)
library(ggthemes)
library(rnaturalearthdata)
library(rnaturalearth)
library(sf)

#figure 1 is the change in hs 1 and 5
#figure 2 is the migclim for 585
#supp 6 will be enm 245
#supp 7 will be enm 370
#supp 8 will be enm 585
#supp 9 will be change hs 1 and 5 ssp 245
#supp 10 will be change hs 1 and 5 ssp 370

#load in the counties for mapping
world <- ne_countries(scale = "medium", returnclass = "sf")

#reproject world map
world_repro <- st_transform(world, CRS("ESRI:102008"))

#create list of species
sp_ls <- c("ABMA", "ANBO", "ANHE", "LISY", "PSMA", "RALU")

#prior to visualizing migclim, going to bring in the different hs5 prediction maps to highlight the differences between the 3 SSPs. some space issues if doing all of these rasters in single stacks, so splitting up.

ABMA_ssp245 <- raster('./outputs/maxent/rasters/ssp245/ABMA_hs5.tif')
ABMA_ssp370 <- raster('./outputs/maxent/rasters/ssp370/ABMA_hs5.tif')
ABMA_ssp585 <- raster('./outputs/maxent/rasters/ssp585/ABMA_hs5.tif')
ANBO_ssp245 <- raster('./outputs/maxent/rasters/ssp245/ANBO_hs5.tif')
ANBO_ssp370 <- raster('./outputs/maxent/rasters/ssp370/ANBO_hs5.tif')
ANBO_ssp585 <- raster('./outputs/maxent/rasters/ssp585/ANBO_hs5.tif')
ANHE_ssp245 <- raster('./outputs/maxent/rasters/ssp245/ANHE_hs5.tif')
ANHE_ssp370 <- raster('./outputs/maxent/rasters/ssp370/ANHE_hs5.tif')
ANHE_ssp585 <- raster('./outputs/maxent/rasters/ssp585/ANHE_hs5.tif')
LISY_ssp245 <- raster('./outputs/maxent/rasters/ssp245/LISY_hs5.tif')
LISY_ssp370 <- raster('./outputs/maxent/rasters/ssp370/LISY_hs5.tif')
LISY_ssp585 <- raster('./outputs/maxent/rasters/ssp585/LISY_hs5.tif')
PSMA_ssp245 <- raster('./outputs/maxent/rasters/ssp245/PSMA_hs5.tif')
PSMA_ssp370 <- raster('./outputs/maxent/rasters/ssp370/PSMA_hs5.tif')
PSMA_ssp585 <- raster('./outputs/maxent/rasters/ssp585/PSMA_hs5.tif')
RALU_ssp245 <- raster('./outputs/maxent/rasters/ssp245/RALU_hs5.tif')
RALU_ssp370 <- raster('./outputs/maxent/rasters/ssp370/RALU_hs5.tif')
RALU_ssp585 <- raster('./outputs/maxent/rasters/ssp585/RALU_hs5.tif')

HS_stacked_245 <- stack(ABMA_ssp245, ANBO_ssp245, ANHE_ssp245, LISY_ssp245, PSMA_ssp245, RALU_ssp245)
HS_stacked_370 <- stack(ABMA_ssp370, ANBO_ssp370, ANHE_ssp370, LISY_ssp370, PSMA_ssp370, RALU_ssp370)
HS_stacked_585 <- stack(ABMA_ssp585, ANBO_ssp585, ANHE_ssp585, LISY_ssp585, PSMA_ssp585, RALU_ssp585)

#need to aggregate due to memory issues if you try to use all cells
HS_stacked_245 <- aggregate(HS_stacked_245, fact=8, fun = modal)
HS_stacked_370 <- aggregate(HS_stacked_370, fact=8, fun = modal)
HS_stacked_585 <- aggregate(HS_stacked_585, fact=8, fun = modal)

#repeat process as above with initial enm
ABMA_hs1 <- raster('./outputs/maxent/rasters/ssp245/ABMA_hs1.tif')
ANBO_hs1 <- raster('./outputs/maxent/rasters/ssp245/ANBO_hs1.tif')
ANHE_hs1 <- raster('./outputs/maxent/rasters/ssp245/ANHE_hs1.tif')
LISY_hs1 <- raster('./outputs/maxent/rasters/ssp245/LISY_hs1.tif')
PSMA_hs1 <- raster('./outputs/maxent/rasters/ssp245/PSMA_hs1.tif')
RALU_hs1 <- raster('./outputs/maxent/rasters/ssp245/RALU_hs1.tif')
HS_stacked_hs1 <- stack(ABMA_hs1, ANBO_hs1, ANHE_hs1, LISY_hs1, PSMA_hs1, RALU_hs1)
HS_stacked_hs1 <- aggregate(HS_stacked_hs1, fact=8, fun = modal)


#create dataframes for ggplot from the rasters
#note that doing this in a list creates very large objects, so again keep broken up
ssp245_list <- list()
for (i in 1:6) {
  df <- as.data.frame(HS_stacked_245[[i]], xy=TRUE, na.rm = TRUE)
  ssp245_list[[i]] <- df
}

ssp370_list <- list()
for (i in 1:6) {
  df <- as.data.frame(HS_stacked_370[[i]], xy=TRUE, na.rm = TRUE)
  ssp370_list[[i]] <- df
}

ssp585_list <- list()
for (i in 1:6) {
  df <- as.data.frame(HS_stacked_585[[i]], xy=TRUE, na.rm = TRUE)
  ssp585_list[[i]] <- df
}

hs1_list <- list()
for (i in 1:6) {
  df <- as.data.frame(HS_stacked_hs1[[i]], xy=TRUE, na.rm = TRUE)
  hs1_list[[i]] <- df
}

#add names
names(ssp245_list) <- c("ABMA_245_df", "ANBO_245_df", "ANHE_245_df", "LISY_245_df", "PSMA_245_df", "RALU_245_df")
names(ssp370_list) <- c("ABMA_370_df", "ANBO_370_df", "ANHE_370_df", "LISY_370_df", "PSMA_370_df", "RALU_370_df")
names(ssp585_list) <- c("ABMA_585_df", "ANBO_585_df", "ANHE_585_df", "LISY_585_df", "PSMA_585_df", "RALU_585_df")
names(hs1_list) <- c("ABMA_hs1_df", "ANBO_hs1_df", "ANHE_hs1_df", "LISY_hs1_df", "PSMA_hs1_df", "RALU_hs1_df")

#bring into the environment for plotting
list2env(ssp245_list, .GlobalEnv)
list2env(ssp370_list, .GlobalEnv)
list2env(ssp585_list, .GlobalEnv)
list2env(hs1_list, .GlobalEnv)


#now to repeat these steps but using calculated rasters (so no hs1 in this case)
#create empty list
#calculate raster differences from the ssp and hs1
#convert to df
HS_stacked_change_245 <- list()
for (i in 1:6) {
  rast_dif <- HS_stacked_245[[1]] - HS_stacked_hs1[[1]]
  df <- as.data.frame(rast_dif, xy=TRUE, na.rm = TRUE)
  HS_stacked_change_245[[i]] <- df
}

HS_stacked_change_370 <- list()
for (i in 1:6) {
  rast_dif <- HS_stacked_370[[1]] - HS_stacked_hs1[[1]]
  df <- as.data.frame(rast_dif, xy=TRUE, na.rm = TRUE)
  HS_stacked_change_370[[i]] <- df
}

HS_stacked_change_585 <- list()
for (i in 1:6) {
  rast_dif <- HS_stacked_585[[1]] - HS_stacked_hs1[[1]]
  df <- as.data.frame(rast_dif, xy=TRUE, na.rm = TRUE)
  HS_stacked_change_585[[i]] <- df
}

#add names
names(HS_stacked_change_245) <- c("ABMA_245dif_df", "ANBO_245dif_df", "ANHE_245dif_df", "LISY_245dif_df", "PSMA_245dif_df", "RALU_245dif_df")
names(HS_stacked_change_370) <- c("ABMA_370dif_df", "ANBO_370dif_df", "ANHE_370dif_df", "LISY_370dif_df", "PSMA_370dif_df", "RALU_370dif_df")
names(HS_stacked_change_585) <- c("ABMA_585dif_df", "ANBO_585dif_df", "ANHE_585dif_df", "LISY_585dif_df", "PSMA_585dif_df", "RALU_585dif_df")


#bring into the environment for plotting
list2env(HS_stacked_change_245, .GlobalEnv)
list2env(HS_stacked_change_370, .GlobalEnv)
list2env(HS_stacked_change_585, .GlobalEnv)

#going to go with the color scheme from 06_visualize_maxent for figure 2

#stuff to fix, fill was changed in geom_raster and scale fill gradient was changed
# ssp245 enm map ----------------------------------------------------------
ABMA_245_map <- ggplot() + 
  geom_raster(data = ABMA_245_df, aes(x = x, y = y, fill = ABMA_hs5)) + 
  scale_fill_gradient2("ENM Value",
                       low = 'white', high = 'steelblue4',
                       na.value = NA) +
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Ambystoma macrodactylum") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face="italic"), legend.position = "none")

ANBO_245_map <- ggplot() + 
  geom_raster(data = ANBO_245_df, aes(x = x, y = y, fill = as.factor(ANBO_245))) + 
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

ANHE_245_map <- ggplot() + 
  geom_raster(data = ANHE_245_df, aes(x = x, y = y, fill = as.factor(ANHE_245))) + 
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

LISY_245_map <- ggplot() + 
  geom_raster(data = LISY_245_df, aes(x = x, y = y, fill = as.factor(LISY_245))) + 
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

PSMA_245_map <- ggplot() + 
  geom_raster(data = PSMA_245_df, aes(x = x, y = y, fill = as.factor(PSMA_245))) + 
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

RALU_245_map <- ggplot() + 
  geom_raster(data = RALU_245_df, aes(x = x, y = y, fill = as.factor(RALU_245))) + 
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
  RALU_245_map + 
    scale_fill_manual(name = "Distribution", 
                      values = c('white','steelblue4'),
                      labels = c('','Presence'),
                      na.translate=FALSE) +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
)

plot_legend <- plot_grid(legend_b, labels = c(''))

plot_maps <- plot_grid(ABMA_245_map, ANBO_245_map, ANHE_245_map, LISY_245_map, PSMA_245_map, RALU_245_map, labels = c('','', '', '', '', '',''), label_size = 2, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))

plot_final <- plot_grid(plot_legend, plot_maps, ncol = 1, rel_heights = c(.1,1))

ggsave("./outputs/supp_figure6.png", plot = plot_final, 
       width = 12, height = 9, dpi = 600)



# ssp370 enm map ---------------------------------------------------------
ABMA_370_map <- ggplot() + 
  geom_raster(data = ABMA_370_df, aes(x = x, y = y, fill = as.factor(ABMA_370))) + 
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

ANBO_370_map <- ggplot() + 
  geom_raster(data = ANBO_370_df, aes(x = x, y = y, fill = as.factor(ANBO_370))) + 
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

ANHE_370_map <- ggplot() + 
  geom_raster(data = ANHE_370_df, aes(x = x, y = y, fill = as.factor(ANHE_370))) + 
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

LISY_370_map <- ggplot() + 
  geom_raster(data = LISY_370_df, aes(x = x, y = y, fill = as.factor(LISY_370))) + 
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

PSMA_370_map <- ggplot() + 
  geom_raster(data = PSMA_370_df, aes(x = x, y = y, fill = as.factor(PSMA_370))) + 
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

RALU_370_map <- ggplot() + 
  geom_raster(data = RALU_370_df, aes(x = x, y = y, fill = as.factor(RALU_370))) + 
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
  RALU_370_map + 
    scale_fill_manual(name = "Distribution", 
                      values = c('white','steelblue4'),
                      labels = c('','Presence'),
                      na.translate=FALSE) +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
)

plot_legend <- plot_grid(legend_b, labels = c(''))

plot_maps <- plot_grid(ABMA_370_map, ANBO_370_map, ANHE_370_map, LISY_370_map, PSMA_370_map, RALU_370_map, labels = c('','', '', '', '', '',''), label_size = 2, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))

plot_final <- plot_grid(plot_legend, plot_maps, ncol = 1, rel_heights = c(.1,1))

ggsave("./outputs/supp_figure7.png", plot = plot_final, 
       width = 12, height = 9, dpi = 600)

# ssp585 enm map ----------------------------------------------------------
ABMA_585_map <- ggplot() + 
  geom_raster(data = ABMA_585_df, aes(x = x, y = y, fill = as.factor(ABMA_585))) + 
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

ANBO_585_map <- ggplot() + 
  geom_raster(data = ANBO_585_df, aes(x = x, y = y, fill = as.factor(ANBO_585))) + 
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

ANHE_585_map <- ggplot() + 
  geom_raster(data = ANHE_585_df, aes(x = x, y = y, fill = as.factor(ANHE_585))) + 
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

LISY_585_map <- ggplot() + 
  geom_raster(data = LISY_585_df, aes(x = x, y = y, fill = as.factor(LISY_585))) + 
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

PSMA_585_map <- ggplot() + 
  geom_raster(data = PSMA_585_df, aes(x = x, y = y, fill = as.factor(PSMA_585))) + 
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

RALU_585_map <- ggplot() + 
  geom_raster(data = RALU_585_df, aes(x = x, y = y, fill = as.factor(RALU_585))) + 
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
  RALU_585_map + 
    scale_fill_manual(name = "Distribution", 
                      values = c('white','steelblue4'),
                      labels = c('','Presence'),
                      na.translate=FALSE) +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
)

plot_legend <- plot_grid(legend_b, labels = c(''))

plot_maps <- plot_grid(ABMA_585_map, ANBO_585_map, ANHE_585_map, LISY_585_map, PSMA_585_map, RALU_585_map, labels = c('','', '', '', '', '',''), label_size = 2, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))

plot_final <- plot_grid(plot_legend, plot_maps, ncol = 1, rel_heights = c(.1,1))

ggsave("./outputs/figure8.png", plot = plot_final, 
       width = 12, height = 9, dpi = 600)




# ssp245 enm change map ----------------------------------------------------------
ABMA_245_map <- ggplot() + 
  geom_raster(data = ABMA_245_df, aes(x = x, y = y, fill = as.factor(ABMA_245))) + 
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

ANBO_245_map <- ggplot() + 
  geom_raster(data = ANBO_245_df, aes(x = x, y = y, fill = as.factor(ANBO_245))) + 
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

ANHE_245_map <- ggplot() + 
  geom_raster(data = ANHE_245_df, aes(x = x, y = y, fill = as.factor(ANHE_245))) + 
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

LISY_245_map <- ggplot() + 
  geom_raster(data = LISY_245_df, aes(x = x, y = y, fill = as.factor(LISY_245))) + 
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

PSMA_245_map <- ggplot() + 
  geom_raster(data = PSMA_245_df, aes(x = x, y = y, fill = as.factor(PSMA_245))) + 
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

RALU_245_map <- ggplot() + 
  geom_raster(data = RALU_245_df, aes(x = x, y = y, fill = as.factor(RALU_245))) + 
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
  RALU_245_map + 
    scale_fill_manual(name = "Distribution", 
                      values = c('white','steelblue4'),
                      labels = c('','Presence'),
                      na.translate=FALSE) +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
)

plot_legend <- plot_grid(legend_b, labels = c(''))

plot_maps <- plot_grid(ABMA_245_map, ANBO_245_map, ANHE_245_map, LISY_245_map, PSMA_245_map, RALU_245_map, labels = c('','', '', '', '', '',''), label_size = 2, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))

plot_final <- plot_grid(plot_legend, plot_maps, ncol = 1, rel_heights = c(.1,1))

ggsave("./outputs/supp_figure9.png", plot = plot_final, 
       width = 12, height = 9, dpi = 600)



# ssp370 enm change map ---------------------------------------------------------
ABMA_370_map <- ggplot() + 
  geom_raster(data = ABMA_370_df, aes(x = x, y = y, fill = as.factor(ABMA_370))) + 
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

ANBO_370_map <- ggplot() + 
  geom_raster(data = ANBO_370_df, aes(x = x, y = y, fill = as.factor(ANBO_370))) + 
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

ANHE_370_map <- ggplot() + 
  geom_raster(data = ANHE_370_df, aes(x = x, y = y, fill = as.factor(ANHE_370))) + 
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

LISY_370_map <- ggplot() + 
  geom_raster(data = LISY_370_df, aes(x = x, y = y, fill = as.factor(LISY_370))) + 
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

PSMA_370_map <- ggplot() + 
  geom_raster(data = PSMA_370_df, aes(x = x, y = y, fill = as.factor(PSMA_370))) + 
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

RALU_370_map <- ggplot() + 
  geom_raster(data = RALU_370_df, aes(x = x, y = y, fill = as.factor(RALU_370))) + 
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
  RALU_370_map + 
    scale_fill_manual(name = "Distribution", 
                      values = c('white','steelblue4'),
                      labels = c('','Presence'),
                      na.translate=FALSE) +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
)

plot_legend <- plot_grid(legend_b, labels = c(''))

plot_maps <- plot_grid(ABMA_370_map, ANBO_370_map, ANHE_370_map, LISY_370_map, PSMA_370_map, RALU_370_map, labels = c('','', '', '', '', '',''), label_size = 2, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))

plot_final <- plot_grid(plot_legend, plot_maps, ncol = 1, rel_heights = c(.1,1))

ggsave("./outputs/supp_figure10.png", plot = plot_final, 
       width = 12, height = 9, dpi = 600)

# ssp585 enm change map ----------------------------------------------------------
ABMA_585_map <- ggplot() + 
  geom_raster(data = ABMA_585_df, aes(x = x, y = y, fill = as.factor(ABMA_585))) + 
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

ANBO_585_map <- ggplot() + 
  geom_raster(data = ANBO_585_df, aes(x = x, y = y, fill = as.factor(ANBO_585))) + 
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

ANHE_585_map <- ggplot() + 
  geom_raster(data = ANHE_585_df, aes(x = x, y = y, fill = as.factor(ANHE_585))) + 
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

LISY_585_map <- ggplot() + 
  geom_raster(data = LISY_585_df, aes(x = x, y = y, fill = as.factor(LISY_585))) + 
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

PSMA_585_map <- ggplot() + 
  geom_raster(data = PSMA_585_df, aes(x = x, y = y, fill = as.factor(PSMA_585))) + 
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

RALU_585_map <- ggplot() + 
  geom_raster(data = RALU_585_df, aes(x = x, y = y, fill = as.factor(RALU_585))) + 
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
  RALU_585_map + 
    scale_fill_manual(name = "Distribution", 
                      values = c('white','steelblue4'),
                      labels = c('','Presence'),
                      na.translate=FALSE) +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top")
)

plot_legend <- plot_grid(legend_b, labels = c(''))

plot_maps <- plot_grid(ABMA_585_map, ANBO_585_map, ANHE_585_map, LISY_585_map, PSMA_585_map, RALU_585_map, labels = c('','', '', '', '', '',''), label_size = 2, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))

plot_final <- plot_grid(plot_legend, plot_maps, ncol = 1, rel_heights = c(.1,1))

ggsave("./outputs/figure1.png", plot = plot_final, 
       width = 12, height = 9, dpi = 600)

# migclim -----------------------------------------------------------------


#read in the MigClim raster that was created
migclim_output <- raster('ABMA_test/ABMA_test_raster.asc')

#here are the value ranges from the output
#0 [no color] Cells that have never been occupied and are unsuitable habitat at the end of the simulation. There are several reasons why a cell can remain non-colonized
#1 (black) Cells that belong to the species' initial distribution and that have remained occupied during the entire simulation.
#1 < value < 30000  #Positive values greater than 1 but smaller then 30000 represent cells that have been colonized during the simulation and that remain occupied at the end of the simulation. The value of the cell allows to determine the dispersal step during which it was colonized using the following code: each environmental change step  given a value of 100 and each dispersal step a value of 1. Here are some examples 101 = 1st dispersal step of 1st environmental change step (1 × 1 + 1 × 100 = 101). 102 = 2nd dispersal step of 1st environmental change step (2 × 1 + 1 × 100 = 102). 504 = 4th dispersal step of 5th environmental change step (4 × 1 + 5 × 100 = 504). 1003 = 3rd dispersal step of 10th environmental change step (3 × 1 + 10 × 100 = 101).
#30,000 [pink] Cells that are potentially suitable (i.e. habitat is favorable) but that were not colonized due to dispersal
# Value < 1 [grey] Negative values indicate cells that were once occupied but have become decolonized, because their habitat has turned unsuitable. Their absolute value allows determining the exact time when they have been decolonized using the same code as explained just above.



#I don't want to see each individual time steps colonization and instead just want to summarize, so I will reclassify to simply the values from above, and the plot and name accordingly
migclim_plot <- reclassify(migclim_output, c(-29999, -0.00001, 1, #lost initial
                                             -0.00001,0.00001,2, #never suitable
                                             0.000011, 1.0001, 3, #maintained initial
                                             1.0002, 29999, 4, #suitable, colonized
                                             29999.01,30005,5)) #suitable, vacant
#next, crop to the area of interest
ext <- extent(-135, -110, 30, 60)
migclim_plot <- crop(migclim_plot, ext)

#convert migclim_output to dataframe for ggplot. converting original as well for factor check
migclim_plot_df <- as.data.frame(migclim_plot, xy = TRUE)

#note that need to specify it is a factor
migclim_plot_df$migclim <- as.factor(migclim_plot_df$ABMA_test_raster)

#going to load in country outline and then plot
dmap <- maps::map("world", regions=c('USA', 'Canada'), col="transparent", plot=FALSE, fill = TRUE)

area_poly <- map2SpatialPolygons(dmap, IDs = dmap$names, proj4string=CRS("+proj=longlat +datum=WGS84"))

#set map extent and crop out country bits you don't want

area_poly_map <- crop(area_poly, ext)

migclim_map <- ggplot() +
  geom_tile(data=migclim_plot_df, aes(x=x, y=y, fill=migclim)) +
  scale_fill_manual(values = c("orange", "white", "darkgrey", "darkblue", "green"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_polygon(data = area_poly_map, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  coord_fixed(1.3) + 
  ggtitle("Ambystoma macrodactylum") + 
  theme_map(base_size = 16) + 
  theme(legend.position="bottom") + 
  theme(plot.title = element_text(face = "italic")) + 
  theme(legend.key.width=unit(1, "cm")) +
  theme(legend.title=element_blank())

ggsave("migclim_map.png", plot = migclim_map, width = 12, height = 12, units="in", dpi = 300)


#function to create plots and save each one as a .png
plotstuff <- function(dataset, name) {
  plot_title <- paste("Plot of ", name, sep=" ")
  ggplot(dataset, aes(x=BPcum, y=-log10(pval))) + #change pval to score if desired
    # Show all points
    geom_point( aes(color=as.factor(chr)), alpha=0.8, size=1.3) +
    scale_color_manual(values = rep(c("grey", "skyblue"), 22 )) +
    #add in Bonferroni corrected line
    geom_hline(yintercept=-log10(1.187e-8), color = "darkred") +
    # custom X axis:
    scale_x_continuous( label = axisdf$chr, breaks= axisdf$center ) +
    scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
    # Custom the theme:
    ggtitle(plot_title) +
    theme_classic() +
    theme( 
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  ggsave(filename = paste(plot_title,".png", sep = ""), plot = last_plot())
}

#run function, which will display each plot, but will also create an object so individual plots can be called again

mapply(plotstuff, dataset = auto_lm_env, name = names(auto_lm_env))