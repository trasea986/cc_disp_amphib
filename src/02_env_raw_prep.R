library(tidyverse)
library(rgdal)
library(terra) #Hijmans created to help speed up some functions from the raster package. This uses SpatRaster objects. Using this going forward.
library(corrplot)
library(caret)
library(sp)

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

# bio_files_repro <- list.files(path = './outputs/data_proc/present_repro', pattern = '*.tif', all.files = TRUE, full.names = TRUE)
# 
# #load in the rasters
# bio_layers_repro <- rast(bio_files_repro)


#next we will focus on removing correlated variables
#first, extract the predictors at the point files
extracted_vals <- extract(bio_layers_repro, points_all_sp[2:3])

#convert to df
extracted_df <- as.data.frame(extracted_vals)

#remove ID column
extracted_df <- extracted_df[,-1]

#calculate the correlation among our variables at our points
mydata.cor <- cor(extracted_df, method = 'spearman', use = 'complete.obs')

#make plot
corrplot(mydata.cor, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, diag=FALSE)

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

terra::writeRaster(predictors_final, './outputs/data_proc/present_final/predictors_final.tiff', filetype = 'GTiff',names = bio_names, overwrite=TRUE)

#and to read in if needed
predictors_final <- rast('./outputs/data_proc/present_final/predictors_final.tiff')

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

#turn points to Spatvector
points_ABMA_spatial <- vect(points_ABMA_spatial, type = 'points', crs = projection, geom=c("decimalLongitude", "decimalLatitude"))
points_ANBO_spatial <- vect(points_ANBO_spatial, type = 'points', crs = projection, geom=c("decimalLongitude", "decimalLatitude"))
points_ANHE_spatial <- vect(points_ANHE_spatial, type = 'points', crs = projection, geom=c("decimalLongitude", "decimalLatitude"))
points_LISY_spatial <- vect(points_LISY_spatial, type = 'points', crs = projection, geom=c("decimalLongitude", "decimalLatitude"))
points_PSMA_spatial <- vect(points_PSMA_spatial, type = 'points', crs = projection, geom=c("decimalLongitude", "decimalLatitude"))
points_RALU_spatial <- vect(points_RALU_spatial, type = 'points', crs = projection, geom=c("decimalLongitude", "decimalLatitude"))

# create circles with a radius of 100 km. This is one of those steps that will really be determined based on the species you are interested in.
ABMA_circles <- buffer(points_ABMA_spatial, 500000)
ANBO_circles <- buffer(points_ANBO_spatial, 500000)
ANHE_circles <- buffer(points_ANHE_spatial, 500000)
LISY_circles <- buffer(points_LISY_spatial, 500000)
PSMA_circles <- buffer(points_PSMA_spatial, 500000)
RALU_circles <- buffer(points_RALU_spatial, 500000)

# define the circles as polygons to dissolve after converting. Note: rgeos package is required to dissolve.
ABMA_poly <- aggregate(ABMA_circles)
ANBO_poly <- aggregate(ANBO_circles)
ANHE_poly <- aggregate(ANHE_circles)
LISY_poly <- aggregate(LISY_circles)
PSMA_poly <- aggregate(PSMA_circles)
RALU_poly <- aggregate(RALU_circles)

#crop the raster stack by this shape
ABMA_predictors <- crop(predictors_final, ABMA_poly)
ANBO_predictors <- crop(predictors_final, ANBO_poly)
ANHE_predictors <- crop(predictors_final, ANHE_poly)
LISY_predictors <- crop(predictors_final, LISY_poly)
PSMA_predictors <- crop(predictors_final, PSMA_poly)
RALU_predictors <- crop(predictors_final, RALU_poly)


#now write final
terra::writeRaster(ABMA_predictors, './outputs/data_proc/present_final/ABMA_predictors_final.tiff', filetype = 'GTiff', overwrite=TRUE)
terra::writeRaster(ANBO_predictors, './outputs/data_proc/present_final/ANBO_predictors_final.tiff', filetype = 'GTiff', overwrite=TRUE)
terra::writeRaster(ANHE_predictors, './outputs/data_proc/present_final/ANHE_predictors_final.tiff', filetype = 'GTiff', overwrite=TRUE)
terra::writeRaster(LISY_predictors, './outputs/data_proc/present_final/LISY_predictors_final.tiff', filetype = 'GTiff', overwrite=TRUE)
terra::writeRaster(PSMA_predictors, './outputs/data_proc/present_final/PSMA_predictors_final.tiff', filetype = 'GTiff', overwrite=TRUE)
terra::writeRaster(RALU_predictors, './outputs/data_proc/present_final/RALU_predictors_final.tiff', filetype = 'GTiff', overwrite=TRUE)

#important note here: we will use the species specific predictors for Maxent model, but then using the predictors_final for predicting distribution

#next up is going to be future data. note that the tiff for each one has the name of years and SSP
#create list of env data
bio_files_future <- list.files(path = './data_raw/future', pattern = '*.tif', all.files = TRUE, full.names = TRUE, recursive = TRUE)

#load in the rasters for the future. 12 files with 19 bioclim layers
bio_layers_future <- lapply(bio_files_future, function(i) {rast(i)})

#view names of the final predictor list
names(predictors_final)

#create subset to pull out of the list of rasters. this is slightly different than the nameing format in bio_names from above
subset_names <- c('wc2_1', 'wc2_18', 'wc2_19', 'wc2_2', 'wc2_3', 'wc2_5', 'wc2_7', 'wc2_8', 'wc2_13', 'wc2_15')

bio_layers_future_sub <- lapply(bio_layers_future, function(i) {subset(i, subset_names)})

#crop all layers to NA to save on disk/computational time, similar to line28
bio_layers_future_crop <- lapply(bio_layers_future_sub, function(i) {crop(i, ext)})        
#reproject all layers fails and fills in NAN for everything but the first layer if mask is on. values seem fine though
bio_layers_future_repro <- lapply(bio_layers_future_crop, function(i) {project(i, projection, method="bilinear")})

#next is resample to match the resolution of the present day files
bio_layers_future_resample <- lapply(bio_layers_future_repro, function(i) {resample(i, predictors_final)})

#going to create the list of names from file list to name and output
future_file_name <- as.data.frame(bio_files_future)

future_file_name <- tidyr::separate(data = future_file_name, col = bio_files_future, into = c('1', '2', '3', '4', '5', '6', '7', '8' , '9', '10', '11', '12', '13', 'SSP-year'), sep = "/")

names(bio_layers_future_resample) <- future_file_name$`SSP-year`

#write raster with lapply gives a subscript error, but works fine outside of lapply? going to just write individually for now. may fix later.
terra::writeRaster(bio_layers_future_resample[[1]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[1]), sep = ''), filetype = 'GTiff')
terra::writeRaster(bio_layers_future_resample[[2]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[2]), sep = ''), filetype = 'GTiff')
terra::writeRaster(bio_layers_future_resample[[3]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[3]), sep = ''), filetype = 'GTiff')
terra::writeRaster(bio_layers_future_resample[[4]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[4]), sep = ''), filetype = 'GTiff')
terra::writeRaster(bio_layers_future_resample[[5]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[5]), sep = ''), filetype = 'GTiff')
terra::writeRaster(bio_layers_future_resample[[6]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[6]), sep = ''), filetype = 'GTiff')
terra::writeRaster(bio_layers_future_resample[[7]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[7]), sep = ''), filetype = 'GTiff')
terra::writeRaster(bio_layers_future_resample[[8]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[8]), sep = ''), filetype = 'GTiff')
terra::writeRaster(bio_layers_future_resample[[9]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[9]), sep = ''), filetype = 'GTiff')
terra::writeRaster(bio_layers_future_resample[[10]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[10]), sep = ''), filetype = 'GTiff')
terra::writeRaster(bio_layers_future_resample[[11]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[11]), sep = ''), filetype = 'GTiff')
terra::writeRaster(bio_layers_future_resample[[12]], filename=paste('./outputs/data_proc/future_final/', names(bio_layers_future_resample[12]), sep = ''), filetype = 'GTiff')