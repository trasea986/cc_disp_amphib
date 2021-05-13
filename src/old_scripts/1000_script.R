#raster calc to get to 1000
library(dismo)
library(ENMeval)

setwd("D:/OneDrive/PhD/CSF_ABM_Project/Maxent Outputs/Tiff_for_MigClim")

files <- list.files(path="D:/OneDrive/PhD/CSF_ABM_Project/Maxent Outputs/Tiff_for_MigClim", pattern=".tif", full.names=TRUE)

envs_tif <- stack(files)

envs_tif_1000 <- envs_tif * 1000

setwd("D:/OneDrive/PhD/CSF_ABM_Project/Maxent Outputs/Tiff_for_MigClim/1000 Scale")

writeRaster(envs_tif_1000,
names(envs_tif_1000), 
 format="GTiff", ## the output format
  bylayer=TRUE, ## this will save a series of layers
  NAflag=-9999, ##setting NA value for ascii, default is some weird number: -3.4e+38
 overwrite=T)

#unfortunately project and resampling doesn't work with files this large
RALU_P <- raster("RALU_Presence.tif")

files <- list.files(path="D:/OneDrive/PhD/CSF_ABM_Project/Maxent Outputs/Tiff_for_MigClim/1000 Scale", pattern=".tif", full.names=TRUE)

stack_for_align <- stack(files, quick=TRUE)

proj_stack <- projectRaster(RALU_P, stack_for_align)

agg_stack <- resample(stack_for_align)
