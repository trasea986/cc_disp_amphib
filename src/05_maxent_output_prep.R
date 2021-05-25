#04 must be run first before doing this script

#move the Maxent models out of the list and into the environment. name here is then pulled into prediction loops
model_ls <- c("ABMA_model", "ANBO_model", "ANHE_model", "LISY_model", "PSMA_model", "RALU_model")
names(MaxEnt_list) <- model_ls

#move to env
list2env(MaxEnt_list, .GlobalEnv)

#predict to NA present, and future

for (i in sp_ls) {
  #predict based on the model for present day
output_predict <- predict(predictors_final, get(paste(i,'_model', sep ='')), progress='text')

#write the raster after scaling for MigClim. unless space is constraining, write present prediction to each climate scenario output
output_predict <- output_predict * 1000
terra::writeRaster(output_predict, filename=paste('./outputs/maxent/rasters/ssp245/',i,'_hs1', sep=''), filetype = 'GTiff')
terra::writeRaster(output_predict, filename=paste('./outputs/maxent/rasters/ssp370/',i,'_hs1', sep=''), filetype = 'GTiff')
terra::writeRaster(output_predict, filename=paste('./outputs/maxent/rasters/ssp585/',i,'_hs1', sep=''), filetype = 'GTiff')
}

#will then actually load in the predictors for later steps
ABMA_present_SDM <- raster('./outputs/maxent/rasters/ssp245/ABMA_hs1') 
ANBO_present_SDM <- raster('./outputs/maxent/rasters/ssp245/ANBO_hs1')
ANHE_present_SDM <- raster('./outputs/maxent/rasters/ssp245/ANHE_hs1')
LISY_present_SDM <- raster('./outputs/maxent/rasters/ssp245/LISY_hs1')
PSMA_present_SDM <- raster('./outputs/maxent/rasters/ssp245/PSMA_hs1')
RALU_present_SDM <- raster('./outputs/maxent/rasters/ssp245/RALU_hs1')

#next, predict for the 12 future scenarios
#maybe nested loop for the 3 ssps at a later date

for (i in sp_ls) {
#ssp245
output_predict2 <- predict(`ssp245_2021-2040`, get(paste(i,'model', sep = '')), progress='text')
output_predict2 <- output_predict2 * 1000
terra::writeRaster(output_predict2, filename=paste('./outputs/maxent/rasters/ssp245/',i,'_hs2', sep =''), filetype = 'GTiff')

output_predict3 <- predict(`ssp245_2041-2060`, get(paste(i,'model', sep = '')), progress='text')
output_predict3 <- output_predict3  * 1000
terra::writeRaster(output_predict3, filename=paste('./outputs/maxent/rasters/ssp245/',i,'_hs3', sep =''), filetype = 'GTiff')

output_predict4 <- predict(`ssp245_2061-2080`, get(paste(i,'model', sep = '')), progress='text')
output_predict4 <- output_predict4 * 1000
terra::writeRaster(output_predict4, filename=paste('./outputs/maxent/rasters/ssp245/',i,'_hs4', sep =''), filetype = 'GTiff')

output_predict5 <- predict(`ssp245_2081-2100`, get(paste(i,'model', sep = '')), progress='text')
output_predict5 <- predict * 1000
terra::writeRaster(output_predict5, filename=paste('./outputs/maxent/rasters/ssp245/',i,'_hs5', sep =''), filetype = 'GTiff')

#ssp370
output_predict2 <- predict(`ssp370_2021-2040`, get(paste(i,'model', sep = '')), progress='text')
output_predict2 <- output_predict2 * 1000
terra::writeRaster(output_predict2, filename=paste('./outputs/maxent/rasters/ssp370/',i,'_hs2', sep =''), filetype = 'GTiff')

output_predict3 <- predict(`ssp370_2041-2060`, get(paste(i,'model', sep = '')), progress='text')
output_predict3 <- output_predict3  * 1000
terra::writeRaster(output_predict3, filename=paste('./outputs/maxent/rasters/ssp370/',i,'_hs3', sep =''), filetype = 'GTiff')

output_predict4 <- predict(`ssp370_2061-2080`, get(paste(i,'model', sep = '')), progress='text')
output_predict4 <- output_predict4 * 1000
terra::writeRaster(output_predict4, filename=paste('./outputs/maxent/rasters/ssp370/',i,'_hs4', sep =''), filetype = 'GTiff')

output_predict5 <- predict(`ssp370_2081-2100`, get(paste(i,'model', sep = '')), progress='text')
output_predict5 <- predict * 1000
terra::writeRaster(output_predict5, filename=paste('./outputs/maxent/rasters/ssp370/',i,'_hs5', sep =''), filetype = 'GTiff')

#ssp585
output_predict2 <- predict(`ssp585_2021-2040`, get(paste(i,'model', sep = '')), progress='text')
output_predict2 <- output_predict2 * 1000
terra::writeRaster(output_predict2, filename=paste('./outputs/maxent/rasters/ssp585/',i,'_hs2', sep =''), filetype = 'GTiff')

output_predict3 <- predict(`ssp585_2041-2060`, get(paste(i,'model', sep = '')), progress='text')
output_predict3 <- output_predict3  * 1000
terra::writeRaster(output_predict3, filename=paste('./outputs/maxent/rasters/ssp585/',i,'_hs3', sep =''), filetype = 'GTiff')

output_predict4 <- predict(`ssp585_2061-2080`, get(paste(i,'model', sep = '')), progress='text')
output_predict4 <- output_predict4 * 1000
terra::writeRaster(output_predict4, filename=paste('./outputs/maxent/rasters/ssp585/',i,'_hs4', sep =''), filetype = 'GTiff')

output_predict5 <- predict(`ssp585_2081-2100`, get(paste(i,'model', sep = '')), progress='text')
output_predict5 <- predict * 1000
terra::writeRaster(output_predict5, filename=paste('./outputs/maxent/rasters/ssp585/',i,'_hs5', sep =''), filetype = 'GTiff')

}


#last step is to reclassify the initial raster and make it the initial distribution of the species

#for each species, going to calculate the mean and standard deviation for the ENM values for the present day raster

#also going to take each species specific background data for the initial distribution
for (i in sp_ls) {
  #predict based on the model for present day
  output_predict <- predict(get(paste(i,'_predictors', sep='')), get(paste(i,'_model', sep ='')), progress='text')
  
  #write the raster after scaling for MigClim. unless space is constraining, write present prediction to each climate scenario output
  output_predict <- output_predict * 1000
  terra::writeRaster(output_predict, filename=paste('./outputs/maxent/rasters/',i,'_ini_cont','.tif', sep=''), filetype = 'GTiff')
}


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

mean(ABMA_ENM_values[complete.cases(ABMA_ENM_values), ]) - sd(ABMA_ENM_values[complete.cases(ABMA_ENM_values), ])
mean(ANBO_ENM_values[complete.cases(ANBO_ENM_values), ]) - sd(ANBO_ENM_values[complete.cases(ANBO_ENM_values), ])
mean(ANHE_ENM_values[complete.cases(ANHE_ENM_values), ]) - sd(ANHE_ENM_values[complete.cases(ANHE_ENM_values), ])
mean(LISY_ENM_values[complete.cases(LISY_ENM_values), ]) - sd(LISY_ENM_values[complete.cases(LISY_ENM_values), ])
mean(PSMA_ENM_values[complete.cases(PSMA_ENM_values), ]) - sd(PSMA_ENM_values[complete.cases(PSMA_ENM_values), ])
mean(RALU_ENM_values[complete.cases(RALU_ENM_values), ]) - sd(RALU_ENM_values[complete.cases(RALU_ENM_values), ])

ABMA_ini <- raster::reclassify(ABMA_ini_SDM, c(0, 307, 0,
                                           307.00001, 1000, 1))
ANBO_ini <- reclassify(ANBO_ini_SDM, c(0, 391, 0,
                                        391, 1000, 1))
ANHE_ini <- reclassify(ANHE_ini_SDM, c(0, 347, 0,
                                        347, 1000, 1))
LISY_ini <- reclassify(LISY_ini_SDM, c(0, 461, 0,
                                        461, 1000, 1))
PSMA_ini <- reclassify(PSMA_ini_SDM, c(0, 436, 0,
                                        434, 1000, 1))
RALU_ini <- reclassify(RALU_ini_SDM, c(0, 391, 0,
                                        391, 1000, 1))

writeRaster(ABMA_ini, filename='./outputs/maxent/ABMA_ini', format="GTiff", overwrite = TRUE)
writeRaster(ANBO_ini, filename='./outputs/maxent/ANBO_ini', format="GTiff", overwrite = TRUE)
writeRaster(ANHE_ini, filename='./outputs/maxent/ANHE_ini', format="GTiff", overwrite = TRUE)
writeRaster(LISY_ini, filename='./outputs/maxent/LISY_ini', format="GTiff", overwrite = TRUE)
writeRaster(PSMA_ini, filename='./outputs/maxent/PSMA_ini', format="GTiff", overwrite = TRUE)
writeRaster(RALU_ini, filename='./outputs/maxent/RALU_ini', format="GTiff", overwrite = TRUE)


#last step is to set up the southern range initial distribution file
#going to do this by loading in the IUCN range, buffering around it, and then masking the ENM

#load in each shapefile
ABMA_IUCN <- vect('./data_raw/IUCN_range/ABMA/data_0.shp')
ANBO_IUCN <- vect('./data_raw/IUCN_range/ANBO/data_0.shp')
ANHE_IUCN <- vect('./data_raw/IUCN_range/ANHE/data_0.shp')
LISY_IUCN <- vect('./data_raw/IUCN_range/LISY/data_0.shp')
PSMA_IUCN <- vect('./data_raw/IUCN_range/PSMA/data_0.shp')
RALU_IUCN <- vect('./data_raw/IUCN_range/RALU/data_0.shp')

#reproject all of them
projection <- "ESRI:102008"

#make them a list
range_list <- list(ABMA_IUCN, ANBO_IUCN, ANHE_IUCN, LISY_IUCN, PSMA_IUCN, RALU_IUCN)
names(range_list) <- c("ABMA_IUCN", "ANBO_IUCN", "ANHE_IUCN", "LISY_IUCN", "PSMA_IUCN", "RALU_IUCN")

#reprojection
range_list <- lapply(range_list, function(i){project(i, projection)})

#next, slide them up and then remove overlap
range_list_shifted <- lapply(range_list, function(i){shift(i, dy=250000)})

#visually checking the shift
#plot(range_list$ABMA_IUCN)
#plot(range_list_shifted$ABMA_IUCN, col="red", add=TRUE)

#next we crop across the list by subtracting the original from the shifted

crop_shift <- function(i, j)
{
  x = i - j
  return(x)
}

#and then we crop out anything about mean lat (this corrects for issues where the shift has a lot of vertical overlap)

crop_mean <- function(i, j)
{
  ymax1 <- as.numeric(ymax(j))
  ymin1 <- as.numeric(ymin(j))
  
  lat_mean <- mean(c(ymax1, ymin1))
  
  ext <- extent(xmin(j), xmax(j), ymin(j), lat_mean/2)
  
  x <- crop(i, ext)
  
  return(x)
}


range_list_crop <- mapply(crop_shift, range_list, range_list_shifted)
range_list_final <- mapply(crop_mean, range_list_crop, range_list)

#visually checking the southern edge
#plot(range_list$LISY_IUCN)
#plot(range_list_final$LISY_IUCN, col="red", add=TRUE)

#we then mask the ENM by these shapefiles.

#load in present day, background extent limited, but as spatraster this time
ABMA_ini_SDM <- rast('./outputs/maxent/rasters/ABMA_ini_cont.tif')
ANBO_ini_SDM <- rast('./outputs/maxent/rasters/ANBO_ini_cont.tif')
ANHE_ini_SDM <- rast('./outputs/maxent/rasters/ANHE_ini_cont.tif')
LISY_ini_SDM <- rast('./outputs/maxent/rasters/LISY_ini_cont.tif')
PSMA_ini_SDM <- rast('./outputs/maxent/rasters/PSMA_ini_cont.tif')
RALU_ini_SDM <- rast('./outputs/maxent/rasters/RALU_ini_cont.tif')

ABMA_ini_south_SDM <- mask(ABMA_ini_SDM, range_list_final$ABMA_IUCN)
ANBO_ini_south_SDM <- mask(ANBO_ini_SDM, range_list_final$ANBO_IUCN)
ANHE_ini_south_SDM <- mask(ANHE_ini_SDM, range_list_final$ANHE_IUCN)
LISY_ini_south_SDM <- mask(LISY_ini_SDM, range_list_final$LISY_IUCN)
PSMA_ini_south_SDM <- mask(PSMA_ini_SDM, range_list_final$PSMA_IUCN)
RALU_ini_south_SDM <- mask(RALU_ini_SDM, range_list_final$RALU_IUCN)

#and last, reclassify. classify in terra package was not returning the correct values though, so going switch to rasterlayer type
ABMA_ini_south_SDM <- raster(ABMA_ini_south_SDM)
ANBO_ini_south_SDM <- raster(ANBO_ini_south_SDM)
ANHE_ini_south_SDM <- raster(ANHE_ini_south_SDM)
LISY_ini_south_SDM <- raster(LISY_ini_south_SDM)
PSMA_ini_south_SDM <- raster(PSMA_ini_south_SDM)
RALU_ini_south_SDM <- raster(RALU_ini_south_SDM)

ABMA_ini_south <- reclassify(ABMA_ini_south_SDM, c(-1, 307, 0,
                                               307, 1000, 1))
ANBO_ini_south <- reclassify(ANBO_ini_south_SDM, c(0, 391, 0,
                                       391, 1000, 1))
ANHE_ini_south <- reclassify(ANHE_ini_south_SDM, c(0, 347, 0,
                                       347, 1000, 1))
LISY_ini_south <- reclassify(LISY_ini_south_SDM, c(0, 461, 0,
                                       461, 1000, 1))
PSMA_ini_south <- reclassify(PSMA_ini_south_SDM, c(0, 436, 0,
                                       434, 1000, 1))
RALU_ini_south <- reclassify(RALU_ini_south_SDM, c(0, 391, 0,
                                       391, 1000, 1))

writeRaster(ABMA_ini_south, filename='./outputs/maxent/ABMA_ini_south', format="GTiff", overwrite = TRUE)
writeRaster(ANBO_ini_south, filename='./outputs/maxent/ANBO_ini_south', format="GTiff", overwrite = TRUE)
writeRaster(ANHE_ini_south, filename='./outputs/maxent/ANHE_ini_south', format="GTiff", overwrite = TRUE)
writeRaster(LISY_ini_south, filename='./outputs/maxent/LISY_ini_south', format="GTiff", overwrite = TRUE)
writeRaster(PSMA_ini_south, filename='./outputs/maxent/PSMA_ini_south', format="GTiff", overwrite = TRUE)
writeRaster(RALU_ini_south, filename='./outputs/maxent/RALU_ini_south', format="GTiff", overwrite = TRUE)