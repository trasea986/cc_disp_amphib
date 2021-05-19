library(tidyverse)
library(rgdal)
library(terra) #Hijmans created to help speed up some functions from the raster package. This uses SpatRaster objects. Using this going forward.
library(corrplot)
library(caret)

#defining projection object in case this is run not after complete the 01 script
projection <- "ESRI:102008"

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
ext <- rast(xmin=-180, xmax=-45, ymin=20, ymax=75)
crs(ext) <- crs(bio_layers$wc2.1_30s_bio_1)
bio_layers <- crop(bio_layers, ext)

#reproject points to albers equal area conic for extract and remove correlated variables steps

#trying to project the whole stack failed ("rasterization failed"?), and lapply crashed computer. for loops also froze. because of how long this takes, going straight to write these files to disk and then import

terra::project(bio_layers[[1]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio1.tif", overwrite=TRUE)
terra::project(bio_layers[[2]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio2.tif", overwrite=TRUE)
terra::project(bio_layers[[3]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio3.tif", overwrite=TRUE)
terra::project(bio_layers[[4]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio4.tif", overwrite=TRUE)
terra::project(bio_layers[[5]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio5.tif", overwrite=TRUE)
terra::project(bio_layers[[6]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio6.tif", overwrite=TRUE)
terra::project(bio_layers[[7]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio7.tif", overwrite=TRUE)
terra::project(bio_layers[[8]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio8.tif", overwrite=TRUE)
terra::project(bio_layers[[9]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio9.tif", overwrite=TRUE)
terra::project(bio_layers[[10]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio10.tif", overwrite=TRUE)
terra::project(bio_layers[[11]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio11.tif", overwrite=TRUE)
terra::project(bio_layers[[12]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio12.tif", overwrite=TRUE)
terra::project(bio_layers[[13]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio13.tif", overwrite=TRUE)
terra::project(bio_layers[[14]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio14.tif", overwrite=TRUE)
terra::project(bio_layers[[15]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio15.tif", overwrite=TRUE)
terra::project(bio_layers[[16]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio16.tif", overwrite=TRUE)
terra::project(bio_layers[[17]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio17.tif", overwrite=TRUE)
terra::project(bio_layers[[18]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio18.tif", overwrite=TRUE)
terra::project(bio_layers[[19]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/present_repro/present_reprojected_bio19.tif", overwrite=TRUE)

bio_files_repro <- list.files(path = './outputs/data_proc/present_repro', pattern = '*.tif', all.files = TRUE, full.names = TRUE)

#load in the rasters
bio_layers_repro <- rast(bio_files_repro)


#next we will focus on removing correlated variables
#first, extract the predictors at the point files
extracted_vals <- extract(bio_layers_repro, points_all_sp[2:3])

#convert to df
extracted_df <- as.data.frame(extracted_vals)

#calculate the correlation among our variables at our points
mydata.cor <- cor(extracted_df, method = 'spearman', use = 'complete.obs')

#set up the correlation value cutoff you want to use to make the list of highly correlated variables
hc <- findCorrelation(mydata.cor, cutoff = 0.8)

#sort the list of highlight correlated variables
hc = sort(hc)

#remove the correlated ones from the data frame. This will create what df for
predictors_final_list = extracted_df[,-c(hc)]

#cut out correlated from the raster stack, then save it
predictors_final <- subset(bio_layers_repro, names(predictors_final_list))

#names of rasters in the stack
bio_names <- names(predictors_final)

writeRaster(predictors_final, filetype = 'GTiff', tempdir = "./outputs/data_proc/present_repro_nocor", names = bio_names, overwrite=TRUE)

#next will be cropping each one for model training so that background extent is not too large

#will be done by breaking up the species file and making a prediction_final for each species

points_ABMA <- points_all_sp %>% filter(species == "Ambystoma macrodactylum")
points_ANBO <- points_all_sp %>% filter(species == "Anaxyrus boreas")
points_ANHE <- points_all_sp %>% filter(species == "Anaxyrus hemiophrys")
points_LISY <- points_all_sp %>% filter(species == "Lithobates sylvaticus")
points_PSMA <- points_all_sp %>% filter(species == "Pseudacris maculata")
points_RALU <- points_all_sp %>% filter(species == "Rana luteiventris")

#make copy for working with spatial. note this is redundant to some of the things in the 01 script
points_ABMA_spatial <- points_ABMA
points_ANBO_spatial <- points_ANBO
points_ANHE_spatial <- points_ANHE
points_LISY_spatial <- points_LISY
points_PSMA_spatial <- points_PSMA
points_RALU_spatial <- points_RALU

points_ABMA_spatial <- vect(points_ABMA_spatial, type = 'points', crs = projection, geom=c("decimalLongitude", "decimalLatitude"))

# create circles with a radius of 100 km. This is one of those steps that will really be determined based on the species you are interested in.
ABMA_circles <- buffer(points_ABMA_spatial, 500000)

# define the circles as polygons to dissolve. Note: rgeos package is required to dissolve.
ABMA_poly <- union(ABMA_circles)

#crop the raster stack by this shape