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
  terra::writeRaster(output_predict, filename=paste('./outputs/maxent/rasters/ssp245/',i,'_ini_cont', sep=''), filetype = 'GTiff')
  terra::writeRaster(output_predict, filename=paste('./outputs/maxent/rasters/ssp370/',i,'_ini_cont', sep=''), filetype = 'GTiff')
  terra::writeRaster(output_predict, filename=paste('./outputs/maxent/rasters/ssp585/',i,'_ini_cont', sep=''), filetype = 'GTiff')
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
ABMA_ini_SDM <- raster('./outputs/maxent/rasters/ssp245/ABMA_init_cont.tif')
ANBO_ini_SDM <- raster('./outputs/maxent/rasters/ssp245/ANBO_init_cont.tif')
ANHE_ini_SDM <- raster('./outputs/maxent/rasters/ssp245/ANHE_init_cont.tif')
LISY_ini_SDM <- raster('./outputs/maxent/rasters/ssp245/LISY_init_cont.tif')
PSMA_ini_SDM <- raster('./outputs/maxent/rasters/ssp245/PSMA_init_cont.tif')
RALU_ini_SDM <- raster('./outputs/maxent/rasters/ssp245/RALU_init_cont.tif')

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

ABMA_ini <- raster::reclassify(ABMA_present_SDM, c(0, 307, 0,
                                           307.00001, 1000, 1))
ANBO_ini <- reclassify(ANBO_present_SDM, c(0, 391, 0,
                                        391, 1, 1))
ANHE_ini <- reclassify(ANHE_present_SDM, c(0, 347, 0,
                                        347, 1, 1))
LISY_ini <- reclassify(LISY_present_SDM, c(0, 461, 0,
                                        461, 1, 1))
PSMA_ini <- reclassify(PSMA_present_SDM, c(0, 436, 0,
                                        434, 1, 1))
RALU_ini <- reclassify(RALU_present_SDM, c(0, 391, 0,
                                        391, 1, 1))

writeRaster(ABMA_ini, filename='./outputs/maxent/ABMA_ini', format="GTiff", overwrite = TRUE)
writeRaster(ANBO_ini, filename='./outputs/maxent/ANBO_ini', format="GTiff", overwrite = TRUE)
writeRaster(ANHE_ini, filename='./outputs/maxent/ANHE_ini', format="GTiff", overwrite = TRUE)
writeRaster(LISY_ini, filename='./outputs/maxent/LISY_ini', format="GTiff", overwrite = TRUE)
writeRaster(PSMA_ini, filename='./outputs/maxent/PSMA_ini', format="GTiff", overwrite = TRUE)
writeRaster(RALU_ini, filename='./outputs/maxent/RALU_ini', format="GTiff", overwrite = TRUE)


#last step is to set up the souther range initial distribution file
#going to do this by loading in the IUCN range, buffering around it, and then masking the ENM