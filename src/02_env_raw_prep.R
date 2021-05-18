library(tidyverse)
library(rgdal)
library(terra) #Hijmans created to help speed up some functions from the raster package. This uses SpatRaster objects. Using this going forward.
library(corrplot)
library(caret)

#defining projection object in case this is run not after complete the 01 script
projection <- projection <- "ESRI:102008"

#start by bringing in the final point file for all fo the species
points_all_sp <- read.csv('./outputs/data_proc/cleaned_points.csv')
points_all_sp_spatial <- vect(points_all_sp,geom=c("decimalLongitude", "decimalLatitude"), crs=projection)


#going to move species to first column to match general example data trends / if this was to be exported to run in maxent outside of R
points_all_sp <- points_all_sp %>% dplyr::select(species, everything())

#create list of env data
bio_files <- list.files(path = './data_raw/present', pattern = '*.tif', all.files = TRUE, full.names = TRUE)

#load in the rasters
bio_layers <- rast(bio_files)

#going to do an initial crop of NW hemisphere to speed up reprojection prior to dealing with correlated variables
ext <- rast(xmin=-180, xmax=-25, ymin=10, ymax=180)
crs(ext) <- crs(bio_layers$wc2.1_30s_bio_1)
bio_layers <- crop(bio_layers, ext)

#reproject to albers equal area conic
bio_layers <- terra::project(bio_layers, method="bilinear", mask=TRUE, projection)





#this takes a long time, so I am going to write this stack
#writeRaster(bio1,"./outputs/data_proc/present_reprojected.tif", overwrite=TRUE, gdal=c("COMPRESS=LZW", "TFW=YES","of=COG"), datatype='INT1U')
