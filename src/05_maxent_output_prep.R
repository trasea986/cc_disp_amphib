library(terra)
library(dismo)

#load up the models
ABMA_model <- load.model('./outputs/maxent/ABMA/maxent')
ANBO_model <- load.model('./outputs/maxent/ANBO/maxent')
ANHE_model <- load.model('./outputs/maxent/ANHE/maxent')
LISY_model <- load.model('./outputs/maxent/LISY/maxent')
PSMA_model <- load.model('./outputs/maxent/PSMA/maxent')
RALU_model <- load.model('./outputs/maxent/RALU/maxent')

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


#next, predict for the 12 future scenarios

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

}

#ssp370

#ssp585



#last step is to reclassify the initial raster and make it the initial distribution of the species
ABMA_ini <- reclassify(ABMA_predict1, c(0, 0.25, 0,
                                     0.25, 1, 1))

writeRaster(ABMA_ini, filename='./outputs/maxent/ABMA_ini', format="GTiff", overwrite = TRUE)