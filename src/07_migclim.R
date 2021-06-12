library(tidyverse)
#MigClim.userGuide() will open up the manual
#MigClim no longer on CRAN, so you have to manually download the zip and install from the zip file

#remotes::install_version("SDMTools", "1.1-221")
#remotes::install_version("MigClim", "1.6.1")

#library(SDMTools)
library(MigClim)
library(raster)

#these take a long time, so best to run in parallel
#option one (and Erich's preferred method)
#library(doParallel)
#for parallel processing
#mc <- makeCluster(detectCores())
#registerDoParallel(mc)

#option 2, and what I have used in the past
library(future.apply)
multicore(workers = 12)
plan(multicore) #for Windows machines use (multiprocess)


#need to the quantile thresholds. this does not need to be run if this script is run in the same sessions as the maxent output prep script

sp_ls <- c("ABMA", "ANBO", "ANHE", "LISY", "PSMA", "RALU")

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

#next prep step is to bring in and overwrite the initial discrete distribution files to make them have a matching extent compared to the habitat suitability files

for (i in sp_ls){
  ini <- raster(paste('./outputs/maxent/rasters/ssp245/',i,'_ini.tif', sep = ''))
  ex <- raster(paste('./outputs/maxent/rasters/ssp245/',i,'_hs1.tif', sep = ''))
  ini_extended <- extend(ini, ex)
  writeRaster(ini_extended, filename=paste('./outputs/maxent/rasters/ssp245/',i,'_ini_final.tif', sep=''), filetype = 'GTiff')
  writeRaster(ini_extended, filename=paste('./outputs/maxent/rasters/ssp370/',i,'_ini_final.tif', sep=''), filetype = 'GTiff') 
  writeRaster(ini_extended, filename=paste('./outputs/maxent/rasters/ssp585/',i,'_ini_final.tif', sep=''), filetype = 'GTiff') 
  
  ini_south <- raster(paste('./outputs/maxent/rasters/ssp245/',i,'_ini_south.tif', sep =''))
  ini_extended <- extend(ini, ex)
  writeRaster(ini_extended, filename=paste('./outputs/maxent/rasters/ssp245/',i,'_ini_south_final.tif', sep=''), filetype = 'GTiff')
  writeRaster(ini_extended, filename=paste('./outputs/maxent/rasters/ssp370/',i,'_ini_south_final.tif', sep=''), filetype = 'GTiff') 
  writeRaster(ini_extended, filename=paste('./outputs/maxent/rasters/ssp585/',i,'_ini_south_final.tif', sep=''), filetype = 'GTiff') 
}


#migclim needs to be in the location of all of the files, so copy ini files to location with hs files
#move to location with the hs and ini files
setwd("./outputs/maxent/rasters/ssp245")
setwd("./outputs/maxent/rasters/ssp370")

#run a test for each species that is short, to create the asc files MigClim will actually use

###AFTER RUNNING TEST: Make sure to move the .tif files to a new directory or else MigClim.migrate will convert every time, which is very slow

future_lapply(sp_ls, function(i) {
  
  start <- (paste(i, 'start', Sys.time()))
  
  MigClim.migrate(iniDist = paste(i,"_ini_final", sep = ''),
                  hsMap=paste(i,'_hs', sep = ''),
                  rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                  envChgSteps=5,
                  dispSteps=1,
                  dispKernel=c(.1),
                  iniMatAge=1, 
                  propaguleProd=c(1),
                  lddFreq=0.05, 
                  lddMinDist=3, 
                  lddMaxDist=4,
                  simulName=paste(i,'_test', sep = ''), 
                  replicateNb=1,
                  overWrite=TRUE,
                  testMode=FALSE, 
                  fullOutput=FALSE, 
                  keepTempFiles=TRUE)
  
  unlink(paste('./',i,'_test/',i,'_test_raster.asc', sep =''))
  
  MigClim.migrate(iniDist = paste(i,"_ini_south_final", sep = ''),
                  hsMap=paste(i,'_hs', sep = ''),
                  rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                  envChgSteps=5,
                  dispSteps=1,
                  dispKernel=c(.1),
                  iniMatAge=1, 
                  propaguleProd=c(1),
                  lddFreq=0.05, 
                  lddMinDist=3, 
                  lddMaxDist=4,
                  simulName=paste(i,'_south_test', sep = ''), 
                  replicateNb=1,
                  overWrite=TRUE,
                  testMode=FALSE, 
                  fullOutput=FALSE, 
                  keepTempFiles=TRUE)
  
  unlink(paste('./',i,'_south_test/',i,'_south_test_raster.asc', sep =''))
  
  end <- paste(i, 'end', Sys.time())
  
  time <- rbind(start, end)
  
  write.table(time, "time.csv", sep = ",", col.names = FALSE, append = TRUE)
}, future.seed = TRUE)

