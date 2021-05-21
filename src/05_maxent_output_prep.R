#must run 04_maxent.R before running this script

#predict to NA present, and future
ABMA_predict1 <- predict(predictors_final, ABMA_model, progress='text')

#write the raster after scaling for MigClim. unless space is constraining, write present prediction to each climate scenario output
ABMA_predict1 <- predict * 1000
terra::writeRaster(ABMA_predict1, filename='./outputs/maxent/rasters/ssp245/ABMA_hs1', filetype = 'GTiff')
terra::writeRaster(ABMA_predict1, filename='./outputs/maxent/rasters/ssp370/ABMA_hs1', filetype = 'GTiff')
terra::writeRaster(ABMA_predict1, filename='./outputs/maxent/rasters/ssp585/ABMA_hs1', filetype = 'GTiff')

#next, predict for the 12 future scenarios
#ssp245
ABMA_predict2 <- predict(`ssp245_2021-2040`, ABMA_model, progress='text')
ABMA_predict2 <- predict * 1000
terra::writeRaster(ABMA_predict1, filename='./outputs/maxent/rasters/ssp245/ABMA_hs2', filetype = 'GTiff')
ABMA_predict3 <- predict(`ssp245_2041-2060`, ABMA_model, progress='text')
ABMA_predict3 <- predict * 1000
terra::writeRaster(ABMA_predict3, filename='./outputs/maxent/rasters/ssp245/ABMA_hs3', filetype = 'GTiff')
ABMA_predict4 <- predict(`ssp245_2061-2080`, ABMA_model, progress='text')
ABMA_predict4 <- predict * 1000
terra::writeRaster(ABMA_predict4, filename='./outputs/maxent/rasters/ssp245/ABMA_hs4', filetype = 'GTiff')
ABMA_predict5 <- predict(`ssp245_2081-2100`, ABMA_model, progress='text')
ABMA_predict5 <- predict * 1000
terra::writeRaster(ABMA_predict5, filename='./outputs/maxent/rasters/ssp245/ABMA_hs5', filetype = 'GTiff')

#ssp370
ABMA_predict2 <- predict(`ssp370_2021-2040`, ABMA_model, progress='text')
ABMA_predict2 <- predict * 1000
terra::writeRaster(ABMA_predict1, filename='./outputs/maxent/rasters/ssp370/ABMA_hs2', filetype = 'GTiff')
ABMA_predict3 <- predict(`ssp370_2041-2060`, ABMA_model, progress='text')
ABMA_predict3 <- predict * 1000
terra::writeRaster(ABMA_predict3, filename='./outputs/maxent/rasters/ssp370/ABMA_hs3', filetype = 'GTiff')
ABMA_predict4 <- predict(`ssp370_2061-2080`, ABMA_model, progress='text')
ABMA_predict4 <- predict * 1000
terra::writeRaster(ABMA_predict4, filename='./outputs/maxent/rasters/ssp370/ABMA_hs4', filetype = 'GTiff')
ABMA_predict5 <- predict(`ssp370_2081-2100`, ABMA_model, progress='text')
ABMA_predict5 <- predict * 1000
terra::writeRaster(ABMA_predict5, filename='./outputs/maxent/rasters/ssp370/ABMA_hs5', filetype = 'GTiff')

#ssp585
ABMA_predict2 <- predict(`ssp585_2021-2040`, ABMA_model, progress='text')
ABMA_predict2 <- predict * 1000
terra::writeRaster(ABMA_predict1, filename='./outputs/maxent/rasters/ssp585/ABMA_hs2', filetype = 'GTiff')
ABMA_predict3 <- predict(`ssp585_2041-2060`, ABMA_model, progress='text')
ABMA_predict3 <- predict * 1000
terra::writeRaster(ABMA_predict3, filename='./outputs/maxent/rasters/ssp585/ABMA_hs3', filetype = 'GTiff')
ABMA_predict4 <- predict(`ssp585_2061-2080`, ABMA_model, progress='text')
ABMA_predict4 <- predict * 1000
terra::writeRaster(ABMA_predict4, filename='./outputs/maxent/rasters/ssp585/ABMA_hs4', filetype = 'GTiff')
ABMA_predict5 <- predict(`ssp585_2081-2100`, ABMA_model, progress='text')
ABMA_predict5 <- predict * 1000
terra::writeRaster(ABMA_predict5, filename='./outputs/maxent/rasters/ssp585/ABMA_hs5', filetype = 'GTiff')

#last step is to reclassify the initial raster and make it the initial distribution of the species
ABMA_ini <- reclassify(ABMA_predict1, c(0, 0.25, 0,
                                     0.25, 1, 1))

writeRaster(ABMA_ini, filename='./outputs/maxent/ABMA_ini', format="GTiff", overwrite = TRUE)