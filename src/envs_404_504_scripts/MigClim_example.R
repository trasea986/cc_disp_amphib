library(tidyverse)
library(dismo)

#SDMTools required for MigClim, but no longer on CRAN
#remotes::install_version("SDMTools", "1.1-221")
library(SDMTools)

#MigClim no longer on CRAN, so you have to manually download the zip and install from the zip file
#note that the versions don't carry all scripts
#general workflow is use R 4.0 for everything but the dispersal models
install.packages("D:/OneDrive/PhD/CSF_ABM_Project/cc_disp_amphib/bin/MigClim_1.6.tar.gz", repos = NULL, type = "source")
#install.packages("D:/OneDrive/PhD/CSF_ABM_Project/cc_disp_amphib/bin/MigClim_1.6.1.tar.gz", repos = NULL, type = "source")
#install.packages("D:/OneDrive/PhD/CSF_ABM_Project/cc_disp_amphib/bin/MigClim_1.6.2.tar.gz", repos = NULL, type = "source")

library(MigClim)


#need to the quantile thresholds. this does not need to be run if this script is run in the same sessions as the maxent output prep script

#set up points and extract
points_all_sp <- read.csv('./outputs/data_proc/cleaned_points.csv')
ABMA_points <- points_all_sp %>% dplyr::filter(species == 'Ambystoma macrodactylum')
ANBO_points <- points_all_sp %>% dplyr::filter(species == 'Anaxyrus boreas')
ANHE_points <- points_all_sp %>% dplyr::filter(species == 'Anaxyrus hemiophrys')
LISY_points <- points_all_sp %>% dplyr::filter(species == 'Lithobates sylvaticus')
PSMA_points <- points_all_sp %>% dplyr::filter(species == 'Pseudacris maculata')
RALU_points <- points_all_sp %>% dplyr::filter(species == 'Rana luteiventris')

#next up is to remove any points with NA predictor variable values.
coordinates(ABMA_points) <- ~decimalLongitude+decimalLatitude
coordinates(ANBO_points) <- ~decimalLongitude+decimalLatitude
coordinates(ANHE_points) <- ~decimalLongitude+decimalLatitude
coordinates(LISY_points) <- ~decimalLongitude+decimalLatitude
coordinates(PSMA_points) <- ~decimalLongitude+decimalLatitude
coordinates(RALU_points) <- ~decimalLongitude+decimalLatitude

#load in present day, background extent limited
ABMA_ini_SDM <- raster('./outputs/maxent/rasters/ABMA_ini_cont.tif')
ANBO_ini_SDM <- raster('./outputs/maxent/rasters/ANBO_ini_cont.tif')
ANHE_ini_SDM <- raster('./outputs/maxent/rasters/ANHE_ini_cont.tif')
LISY_ini_SDM <- raster('./outputs/maxent/rasters/LISY_ini_cont.tif')
PSMA_ini_SDM <- raster('./outputs/maxent/rasters/PSMA_ini_cont.tif')
RALU_ini_SDM <- raster('./outputs/maxent/rasters/RALU_ini_cont.tif')

ABMA_ENM_values <- raster::extract(ABMA_ini_SDM, ABMA_points)
ANBO_ENM_values <- raster::extract(ANBO_ini_SDM, ANBO_points)
ANHE_ENM_values <- raster::extract(ANHE_ini_SDM, ANHE_points)
LISY_ENM_values <- raster::extract(LISY_ini_SDM, LISY_points)
PSMA_ENM_values <- raster::extract(PSMA_ini_SDM, PSMA_points)
RALU_ENM_values <- raster::extract(RALU_ini_SDM, RALU_points)

ABMA_ENM_values <- as.data.frame(ABMA_ENM_values)
ANBO_ENM_values <- as.data.frame(ANBO_ENM_values)
ANHE_ENM_values <- as.data.frame(ANHE_ENM_values)
LISY_ENM_values <- as.data.frame(LISY_ENM_values)
PSMA_ENM_values <- as.data.frame(PSMA_ENM_values)
RALU_ENM_values <- as.data.frame(RALU_ENM_values)

#calculate mean at occupancy value and subtract one standard deviation to determine the thresholds for the initial distribution

ABMA_quant <- quantile(ABMA_ENM_values, probs = 0.10, na.rm = TRUE)
ANBO_quant <- quantile(ANBO_ENM_values, probs = 0.10, na.rm = TRUE)
ANHE_quant <- quantile(ANHE_ENM_values, probs = 0.10, na.rm = TRUE)
LISY_quant <- quantile(LISY_ENM_values, probs = 0.10, na.rm = TRUE)
PSMA_quant <- quantile(PSMA_ENM_values, probs = 0.10, na.rm = TRUE)
RALU_quant <- quantile(RALU_ENM_values, probs = 0.10, na.rm = TRUE)

#next prep step is to bring in and overwrite the initial distribution files to make them have a matching extent compared to the habitat suitability files


#migclim needs to be in the location of all of the files, so copy ini files to location with hs files

i = 'ABMA'

ABMA_ini <- raster('./outputs/maxent/rasters/ssp245/ABMA_ini.tif')
ABMA_hs_ex <- raster('./outputs/maxent/rasters/ssp245/ABMA_hs1.tif')
ABMA_ini_extended <- extend(ABMA_ini, ABMA_hs_ex)
writeRaster(ABMA_ini_extended, filename=paste('./outputs/maxent/rasters/ssp245/',i,'_ini_final.tif', sep=''), filetype = 'GTiff')

setwd("D:/OneDrive/PhD/CSF_ABM_Project/cc_disp_amphib/outputs/maxent/rasters/ssp245")

hs1 <- raster('ABMA_hs1.asc')
hs2<- raster('ABMA_hs2.asc')
hs3<- raster('ABMA_hs3.asc')
hs4<- raster('ABMA_hs4.tif')
hs5<- raster('ABMA_hs5.tif')
ini<- raster('ABMA_ini.tif')
ini<- raster('./outputs/maxent/rasters/ssp245/ABMA_ini_final.tif')

#ran, from bin, the script to create this function manually after running into some issues with the install above
MigClim.migrate(iniDist = "ABMA_ini_final",
                 hsMap="ABMA_hs",
                 rcThreshold = round(as.numeric(ABMA_quant)),
                 envChgSteps=5,
                 dispSteps=1,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ABMA_test", replicateNb=1,
                 overWrite=TRUE,
                 testMode=TRUE, 
                 fullOutput=FALSE, keepTempFiles=TRUE)

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
