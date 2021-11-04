library(dismo)
library(tidyverse)
library(maptools)
library(ggthemes)

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

HS_stacked_245 <- aggregate(HS_stacked_245, fact=8, fun = modal)
HS_stacked_370 <- aggregate(HS_stacked_370, fact=8, fun = modal)
HS_stacked_585 <- aggregate(HS_stacked_585, fact=8, fun = modal)

#create dataframes for ggplot from the rasters
#note that doing this in a list creates very large objects, so again keep broken up
ssp245_list <- list()
for (i in sp_ls) {
  df <- as.data.frame(get(paste(i,"_ssp245", sep='')), xy=TRUE)
  ssp245_list[[i]] <- df
}

ssp370_list <- list()
for (i in sp_ls) {
  df <- as.data.frame(get(paste(i,"_ssp370", sep='')), xy=TRUE)
  ssp370_list[[i]] <- df
}

ssp585_list <- list()
for (i in sp_ls) {
  df <- as.data.frame(get(paste(i,"_ssp585", sep='')), xy=TRUE)
  ssp585_list[[i]] <- df
}

#remove NAs
ssp245_list <- lapply(ssp245_list, function(i){na.omit(i)})
names(ssp245_list) <- c("ABMA_245_df", "ANBO_245_df", "ANHE_245_df", "LISY_245_df", "PSMA_245_df", "RALU_245_df")

ssp370_list <- lapply(ssp370_list, function(i){na.omit(i)})
names(ssp370_list) <- c("ABMA_370_df", "ANBO_370_df", "ANHE_370_df", "LISY_370_df", "PSMA_370_df", "RALU_370_df")

ssp585_list <- lapply(ssp585_list, function(i){na.omit(i)})
names(ssp585_list) <- c("ABMA_585_df", "ANBO_585_df", "ANHE_585_df", "LISY_585_df", "PSMA_585_df", "RALU_585_df")









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