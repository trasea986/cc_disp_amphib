library(tidyverse)
#MigClim.userGuide() will open up the manual
#MigClim no longer on CRAN, so you have to manually download the zip and install from the zip file

#remotes::install_version("SDMTools", "1.1-221")
#remotes::install_version("MigClim", "1.6.1")

#library(SDMTools)
library(MigClim)
library(raster)



#library(doParallel)

#for parallel processing
#mc <- makeCluster(detectCores())
#registerDoParallel(mc)

#MigClim no longer on CRAN, so you have to manually download the zip and install from the zip file
#note that the versions don't carry all scripts
#general workflow is use R 4.0 for everything but the dispersal models


#MIGCLIM. Note need to manually run creating the function from the 1.6 zip file. The 1.6.2 only has updates/changes.


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

setwd("./outputs/maxent/rasters/ssp245")


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
                testMode=FALSE, 
                fullOutput=FALSE, keepTempFiles=TRUE)

MigClim.migrate (iniDist = "ABMA_ini",
                 hsMap="hs_map",
                 rcThreshold = 250,
                 envChgSteps=1,
                 dispSteps=5,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ABMA_test2", replicateNb=1,
                 overWrite=TRUE,
                 testMode=FALSE, 
                 fullOutput=FALSE, keepTempFiles=TRUE)