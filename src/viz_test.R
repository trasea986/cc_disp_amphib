library(dismo)
library(tidyverse)
library(maptools)
library(ggthemes)
library(rnaturalearthdata)
library(rnaturalearth)
library(sf)
library(cowplot)

#figure 1 is the change in hs 1 and 5
#figure 2 is the migclim for 585
#figure 3 is the migclim for 585, southern range
#supp 6 will be enm 245
#supp 7 will be enm 370
#supp 8 will be enm 585
#supp 9 will be change hs 1 and 5 ssp 245
#supp 10 will be change hs 1 and 5 ssp 370
#supp 11 will be migclim 245
#supp 12 will be migclim 370
#supp 13 will be migclim 245, southern range
#supp 14 will be migclim 370, southern range

#load in the counties for mapping
world <- ne_countries(scale = "medium", returnclass = "sf")

#reproject world map
world_repro <- st_transform(world, CRS("ESRI:102008"))

#create list of species
sp_ls <- c("ABMA", "ANBO", "ANHE", "LISY", "PSMA", "RALU")


# migclim -----------------------------------------------------------------
#here are the value ranges from the output
#0 [no color] Cells that have never been occupied and are unsuitable habitat at the end of the simulation. There are several reasons why a cell can remain non-colonized
#1 (black) Cells that belong to the species' initial distribution and that have remained occupied during the entire simulation.
#1 < value < 30000  #Positive values greater than 1 but smaller then 30000 represent cells that have been colonized during the simulation and that remain occupied at the end of the simulation. The value of the cell allows to determine the dispersal step during which it was colonized using the following code: each environmental change step  given a value of 100 and each dispersal step a value of 1. Here are some examples 101 = 1st dispersal step of 1st environmental change step (1 × 1 + 1 × 100 = 101). 102 = 2nd dispersal step of 1st environmental change step (2 × 1 + 1 × 100 = 102). 504 = 4th dispersal step of 5th environmental change step (4 × 1 + 5 × 100 = 504). 1003 = 3rd dispersal step of 10th environmental change step (3 × 1 + 10 × 100 = 101).
#30,000 [pink] Cells that are potentially suitable (i.e. habitat is favorable) but that were not colonized due to dispersal
# Value < 1 [grey] Negative values indicate cells that were once occupied but have become decolonized, because their habitat has turned unsuitable. Their absolute value allows determining the exact time when they have been decolonized using the same code as explained just above.
#these are color by species, all using the same, so can loop it for each individual image, but want to combine in R

#read in the MigClim raster that was created

ABMA_migclim <- raster('outputs/viz_test/full_ABMA_base1_raster.asc')
ANBO_migclim <- raster('outputs/viz_test/full_ANBO_base1_raster.asc')
ANHE_migclim <- raster('outputs/viz_test/full_ANHE_base1_raster.asc')
LISY_migclim <- raster('outputs/viz_test/full_LISY_base1_raster.asc')
PSMA_migclim <- raster('outputs/viz_test/full_PSMA_base1_raster.asc')
RALU_migclim <- raster('outputs/viz_test/full_RALU_base1_raster.asc')

migclim_stack_245 <- stack(ABMA_migclim, ANBO_migclim, ANHE_migclim, LISY_migclim, PSMA_migclim, RALU_migclim)

migclim_processed_245 <- list()
for (i in 1:6) {
  #aggregate for plotting
  migclim_output <- aggregate(migclim_stack_245[[i]], fact=8, fun = modal)
  
  #I don't want to see each individual time steps colonization and instead just want to summarize, so I will reclassify to simplify the values from above, and the plot and name accordingly
  migclim_plot <- reclassify(migclim_output, c(-29999, -0.00001, 1, #lost initial
                                               -0.00001,0.00001,2, #never suitable
                                               0.000011, 1.0001, 3, #maintained initial
                                               1.0002, 29999, 4, #suitable, colonized
                                               29999.01,30005,5)) #suitable, vacant
  
  
  #convert migclim_output to dataframe for ggplot. converting original as well for factor check
  migclim_plot_df <- as.data.frame(migclim_plot, xy = TRUE, na.rm = TRUE)
  
  #note that need to specify it is a factor
  migclim_processed_245[[i]] <- migclim_plot_df
}

#add names
names(migclim_processed_245) <- c("ABMA_245mig_df", "ANBO_245mig_df", "ANHE_245mig_df", "LISY_245mig_df", "PSMA_245mig_df", "RALU_245mig_df")

list2env(migclim_processed_245, .GlobalEnv)


# migclim 245 -------------------------------------------------------------
#set map extent and crop out country bits you don't want
#need to do as.factor for the fill
AMBA_migclim <- ggplot() + 
  geom_tile(data=ABMA_245mig_df, aes(x=x, y=y, fill=as.factor(full_ABMA_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Ambystoma macrodactylum") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

ANBO_migclim <- ggplot() + 
  geom_tile(data=ANBO_245mig_df, aes(x=x, y=y, fill=as.factor(full_ANBO_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Anaxyrus boreas") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

ANHE_migclim <- ggplot() + 
  geom_tile(data=ANHE_245mig_df, aes(x=x, y=y, fill=as.factor(full_ANHE_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Anaxyrus hemiophrys") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

LISY_migclim <- ggplot() + 
  geom_tile(data=LISY_245mig_df, aes(x=x, y=y, fill=as.factor(full_LISY_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Lithobates sylvaticus") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

PSMA_migclim <- ggplot() + 
  geom_tile(data=PSMA_245mig_df, aes(x=x, y=y, fill=as.factor(full_PSMA_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Pseudacris maculata") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

RALU_migclim <- ggplot() + 
  geom_tile(data=RALU_245mig_df, aes(x=x, y=y, fill=as.factor(full_RALU_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Rana luteiventris") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

legend_b <- get_legend(
  RALU_migclim + 
    scale_fill_manual(name = "", 
                      values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), 
                      labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'),
                      na.translate=FALSE) +
    guides(color = guide_legend(nrow = 1),fill = guide_legend(override.aes = list(color = "black"))) +
    theme(legend.position = "top")
)

plot_legend <- plot_grid(legend_b, labels = c(''))

plot_maps <- plot_grid(ABMA_migclim, ANBO_migclim, ANHE_migclim, LISY_migclim, PSMA_migclim, RALU_migclim, labels = c('','', '', '', '', '',''), label_size = 2, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))

plot_final <- plot_grid(plot_legend, plot_maps, ncol = 1, rel_heights = c(.1,1))

ggsave("./outputs/viz_tes1.png", plot = plot_final, 
       width = 12, height = 9, dpi = 600)





ABMA_migclim <- raster('outputs/viz_test/south_ABMA_base1_raster.asc')
ANBO_migclim <- raster('outputs/viz_test/south_ANBO_base1_raster.asc')
ANHE_migclim <- raster('outputs/viz_test/south_ANHE_base1_raster.asc')
LISY_migclim <- raster('outputs/viz_test/south_LISY_base1_raster.asc')
PSMA_migclim <- raster('outputs/viz_test/south_PSMA_base1_raster.asc')
RALU_migclim <- raster('outputs/viz_test/south_RALU_base1_raster.asc')

migclim_stack_245 <- stack(ABMA_migclim, ANBO_migclim, ANHE_migclim, LISY_migclim, PSMA_migclim, RALU_migclim)

migclim_processed_245 <- list()
for (i in 1:6) {
  #aggregate for plotting
  migclim_output <- aggregate(migclim_stack_245[[i]], fact=8, fun = modal)
  
  #I don't want to see each individual time steps colonization and instead just want to summarize, so I will reclassify to simplify the values from above, and the plot and name accordingly
  migclim_plot <- reclassify(migclim_output, c(-29999, -0.00001, 1, #lost initial
                                               -0.00001,0.00001,2, #never suitable
                                               0.000011, 1.0001, 3, #maintained initial
                                               1.0002, 29999, 4, #suitable, colonized
                                               29999.01,30005,5)) #suitable, vacant
  
  
  #convert migclim_output to dataframe for ggplot. converting original as well for factor check
  migclim_plot_df <- as.data.frame(migclim_plot, xy = TRUE, na.rm = TRUE)
  
  #note that need to specify it is a factor
  migclim_processed_245[[i]] <- migclim_plot_df
}

#add names
names(migclim_processed_245) <- c("ABMA_245mig_df", "ANBO_245mig_df", "ANHE_245mig_df", "LISY_245mig_df", "PSMA_245mig_df", "RALU_245mig_df")

list2env(migclim_processed_245, .GlobalEnv)


# migclim 245 -------------------------------------------------------------
#set map extent and crop out country bits you don't want
#need to do as.factor for the fill
AMBA_migclim <- ggplot() + 
  geom_tile(data=ABMA_245mig_df, aes(x=x, y=y, fill=as.factor(south_ABMA_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Ambystoma macrodactylum") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

ANBO_migclim <- ggplot() + 
  geom_tile(data=ANBO_245mig_df, aes(x=x, y=y, fill=as.factor(south_ANBO_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Anaxyrus boreas") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

ANHE_migclim <- ggplot() + 
  geom_tile(data=ANHE_245mig_df, aes(x=x, y=y, fill=as.factor(south_ANHE_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Anaxyrus hemiophrys") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

LISY_migclim <- ggplot() + 
  geom_tile(data=LISY_245mig_df, aes(x=x, y=y, fill=as.factor(south_LISY_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Lithobates sylvaticus") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

PSMA_migclim <- ggplot() + 
  geom_tile(data=PSMA_245mig_df, aes(x=x, y=y, fill=as.factor(south_PSMA_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Pseudacris maculata") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

RALU_migclim <- ggplot() + 
  geom_tile(data=RALU_245mig_df, aes(x=x, y=y, fill=as.factor(south_RALU_base1_raster))) + 
  scale_fill_manual(values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Rana luteiventris") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) +
  guides(fill = guide_legend(override.aes = list(color = "black")))+
  theme(plot.title = element_text(face="italic"), legend.position = "none")

legend_b <- get_legend(
  RALU_migclim + 
    scale_fill_manual(name = "", 
                      values = c("yellow3", "white", "darkgrey", "darkblue", "green2"), 
                      labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'),
                      na.translate=FALSE) +
    guides(color = guide_legend(nrow = 1),fill = guide_legend(override.aes = list(color = "black"))) +
    theme(legend.position = "top")
)

plot_legend <- plot_grid(legend_b, labels = c(''))

plot_maps <- plot_grid(AMBA_migclim, ANBO_migclim, ANHE_migclim, LISY_migclim, PSMA_migclim, RALU_migclim, labels = c('','', '', '', '', '',''), label_size = 2, ncol = 3, rel_heights = c(1,1,1,1,1,1,1))

plot_final <- plot_grid(plot_legend, plot_maps, ncol = 1, rel_heights = c(.1,1))

ggsave("./outputs/viz_tes1.png", plot = plot_final, 
       width = 12, height = 9, dpi = 600)