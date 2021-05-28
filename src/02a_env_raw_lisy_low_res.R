#02a_env_raw_prep is to prep layers specifically for LISY when using ENMeval. The 500m causes 1 TB cluster to crash

library(terra)
library(tidyverse)

projection <- "ESRI:102008"

#read in the final rasters from before to get the names of the ones to keep
LISY_predictors <- raster::stack('./outputs/data_proc/present_final/LISY_predictors_final.tiff')

#load lower resolution at about 2.5 minutes (the coarser WorldClim scale)
#because of memory limitations of 1 TB on the cluster, need to use a coarser resolution for LISY
lisy_files <- list.files(path = './data_raw/present_LISY_low_res', pattern = '*.tif', all.files = TRUE, full.names = TRUE)

lisy_rast <- rast(lisy_files)

lisy_predictors_low_res <- subset(lisy_rast, c("wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_18", "wc2.1_2.5m_bio_19", "wc2.1_2.5m_bio_2", "wc2.1_2.5m_bio_3", "wc2.1_2.5m_bio_5","wc2.1_2.5m_bio_7", "wc2.1_2.5m_bio_8", "wc2.1_2.5m_bio_13","wc2.1_2.5m_bio_15"))

terra::project(lisy_predictors_low_res[[1]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/LISY_ENMeval/repro/present_reprojected_bio_1.tif", overwrite=TRUE)
terra::project(lisy_predictors_low_res[[2]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/LISY_ENMeval/repro/present_reprojected_bio_18.tif", overwrite=TRUE)
terra::project(lisy_predictors_low_res[[3]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/LISY_ENMeval/repro/present_reprojected_bio_19.tif", overwrite=TRUE)
terra::project(lisy_predictors_low_res[[4]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/LISY_ENMeval/repro/present_reprojected_bio_2.tif", overwrite=TRUE)
terra::project(lisy_predictors_low_res[[5]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/LISY_ENMeval/repro/present_reprojected_bio_3.tif", overwrite=TRUE)
terra::project(lisy_predictors_low_res[[6]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/LISY_ENMeval/repro/present_reprojected_bio_5.tif", overwrite=TRUE)
terra::project(lisy_predictors_low_res[[7]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/LISY_ENMeval/repro/present_reprojected_bio_7.tif", overwrite=TRUE)
terra::project(lisy_predictors_low_res[[8]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/LISY_ENMeval/repro/present_reprojected_bio_8.tif", overwrite=TRUE)
terra::project(lisy_predictors_low_res[[9]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/LISY_ENMeval/repro/present_reprojected_bio_13.tif", overwrite=TRUE)
terra::project(lisy_predictors_low_res[[10]], projection, method="bilinear", mask=TRUE, filename = "./outputs/data_proc/LISY_ENMeval/repro/present_reprojected_bio_15.tif", overwrite=TRUE)


lisy_repro <- list.files(path = './outputs/data_proc/LISY_ENMeval/repro',  pattern = '*.tif', all.files = TRUE, full.names = TRUE)

lisy_repro <- rast(lisy_repro)

lisy_final_low_res <- crop(lisy_repro, LISY_predictors)

terra::writeRaster(lisy_final_low_res, './outputs/data_proc/LISY_ENMeval/LISY_low_res_predictors_final.tiff', filetype = 'GTiff', overwrite=TRUE)
