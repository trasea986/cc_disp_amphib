#this cuts out some of the testing parts from 07 so that this can just get dumped and run on the cluster

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

ABMA_quant <- quantile(ABMA_ENM_values, probs = 0.10, na.rm = TRUE)
ANBO_quant <- quantile(ANBO_ENM_values, probs = 0.10, na.rm = TRUE)
ANHE_quant <- quantile(ANHE_ENM_values, probs = 0.10, na.rm = TRUE)
LISY_quant <- quantile(LISY_ENM_values, probs = 0.10, na.rm = TRUE)
PSMA_quant <- quantile(PSMA_ENM_values, probs = 0.10, na.rm = TRUE)
RALU_quant <- quantile(RALU_ENM_values, probs = 0.10, na.rm = TRUE)

#next prep step is to bring in and overwrite the initial discrete distribution files to make them have a matching extent compared to the habitat suitability files



#migclim needs to be in the location of all of the files, so copy ini files to location with hs files
#move to location with the hs and ini files
setwd("./outputs/maxent/rasters/ssp245")

#Next, run all of the scenarios from the original list/script

print("Starting MigClim 245")

#Note LDD distance must not overlap with dispersal kernal, so for purposes of sensitivity setting rate and LDD to zero for base model, as opposed to increasing LDD distance in tandem with the dispersal kernal max distance being tested

future_lapply(sp_ls, function(i) {
  
  MigClim.migrate(iniDist = paste(i,"_ini_final", sep = ''),
                  hsMap=paste(i,'_hs', sep = ''),
                  rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                  envChgSteps=5,
                  dispSteps=20,
                  dispKernel=c(.1),
                  iniMatAge=1, 
                  propaguleProd=c(1),
                  #lddFreq=0.05, 
                  #lddMinDist=3, 
                  #lddMaxDist=4,
                  simulName=paste("full_",i,'_base', sep = ''), 
                  replicateNb=5,
                  overWrite=TRUE,
                  testMode=FALSE, 
                  fullOutput=FALSE, 
                  keepTempFiles=TRUE)
  
  
  print('Base case done')
  
  ###########age
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=2, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age2/full_',i,'_age21_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age22_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age23_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age24_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=3, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age3', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age3/full_',i,'_age31_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age32_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age33_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age34_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age35_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=4, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age4/full_',i,'_age41_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age42_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age43_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age44_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=5, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=9,
                   simulName=paste("full_",i,'_age5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age5/full_',i,'_age51_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age52_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age53_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age54_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=6, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age6/full_',i,'_age61_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age62_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age63_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age64_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age65_raster.asc', sep =''))
  
  print('Age done')
  
  ###########rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.2),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate21_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate22_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate23_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate24_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.4),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate41_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate42_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate43_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate44_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.6),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate61_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate62_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate63_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate64_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.8),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate81_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate82_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate83_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate84_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate85_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(1),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate105_raster.asc', sep =''))
  
  print('Rate done')
  
  ###########distance
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.33,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.45,.20,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance151_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance152_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance153_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance154_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.684,.468,.320,.219,.150,.102),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance30', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance301_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance302_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance303_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance304_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance305_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.78, 0.61, 0.47, 0.37, 0.29, 0.22, 0.17, 0.14, 0.11),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance45', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance451_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance452_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance453_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance454_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance455_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.83, 0.68, 0.57, 0.47, 0.39, 0.32, 0.26, 0.22, 0.18, 0.15, 0.12, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance60', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance601_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance602_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance603_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance604_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance605_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.90, 0.81, 0.73, 0.66, 0.59, 0.53, 0.48, 0.43, 0.39, 0.35, 0.32, 0.28, 0.26, 0.23, 0.21, 0.19, 0.17, 0.15, 0.14, 0.12, 0.11, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance110', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1105_raster.asc', sep =''))
  
  print('Distance done')
  
  ###########ldd rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate05', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate051_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate052_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate053_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate054_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate055_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.10, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.15, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate151_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate152_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate153_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate154_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.20, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate20', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate201_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate202_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate203_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate204_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate205_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.25, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate25', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate251_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate252_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate253_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate254_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate255_raster.asc', sep =''))
  
  print('LDD Rate Done')
  
  ###########ldd dist
  
    MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist41_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist42_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist43_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist44_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=4, lddMaxDist=5,
                   simulName=paste("full_",i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist51_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist52_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist53_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist54_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=5, lddMaxDist=6,
                   simulName=paste("full_",i,'_ldddist6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist61_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist62_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist63_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist64_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=6, lddMaxDist=7,
                   simulName=paste("full_",i,'_ldddist7', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist71_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist72_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist73_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist74_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist75_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=7, lddMaxDist=8,
                   simulName=paste("full_",i,'_ldddist8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist81_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist82_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist83_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist84_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist85_raster.asc', sep =''))
  
  print('LDD Distance Done')
  
  print('All Done Full Range')
  
  
  
  
  
  
  
  
  #########################south models
  
  
  MigClim.migrate(iniDist = paste(i,"_ini_south_final", sep = ''),
                  hsMap=paste(i,'_hs', sep = ''),
                  rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                  envChgSteps=5,
                  dispSteps=20,
                  dispKernel=c(.1),
                  iniMatAge=1, 
                  propaguleProd=c(1),
                  #lddFreq=0.05, 
                  #lddMinDist=3, 
                  #lddMaxDist=4,
                  simulName=paste('south_',i,'_base', sep = ''), 
                  replicateNb=5,
                  overWrite=TRUE,
                  testMode=FALSE, 
                  fullOutput=FALSE, 
                  keepTempFiles=TRUE)
  
  
  print('Base case done')
  
  ###########age
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=2, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age2/','south_',i,'_age21_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age22_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age23_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age24_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=3, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age3', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age3/south_',i,'_age31_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age32_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age33_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age34_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age35_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=4, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age4/south_',i,'_age41_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age42_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age43_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age44_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=5, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age5/south_',i,'_age51_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age52_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age53_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age54_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=6, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age6/south_',i,'_age61_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age62_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age63_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age64_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age65_raster.asc', sep =''))
  
  print('Age done')
  
  ###########rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.2),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate21_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate22_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate23_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate24_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.4),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate41_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate42_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate43_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate44_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.6),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate61_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate62_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate63_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate64_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.8),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate81_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate82_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate83_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate84_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate85_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(1),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate105_raster.asc', sep =''))
  
  print('Rate done')
  
  ###########distance
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.33,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.45,.20,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance151_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance152_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance153_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance154_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.684,.468,.320,.219,.150,.102),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance30', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance301_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance302_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance303_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance304_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance305_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.78, 0.61, 0.47, 0.37, 0.29, 0.22, 0.17, 0.14, 0.11),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance45', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance451_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance452_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance453_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance454_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance455_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.83, 0.68, 0.57, 0.47, 0.39, 0.32, 0.26, 0.22, 0.18, 0.15, 0.12, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance60', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance601_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance602_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance603_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance604_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance605_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.90, 0.81, 0.73, 0.66, 0.59, 0.53, 0.48, 0.43, 0.39, 0.35, 0.32, 0.28, 0.26, 0.23, 0.21, 0.19, 0.17, 0.15, 0.14, 0.12, 0.11, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance110', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1105_raster.asc', sep =''))
  
  print('Distance done')
  
  ###########ldd rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate05', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate051_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate052_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate053_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate054_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate055_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.10, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.15, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate151_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate152_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate153_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate154_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.20, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate20', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate201_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate202_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate203_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate204_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate205_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.25, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate25', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate251_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate252_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate253_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate254_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate255_raster.asc', sep =''))
  
  print('LDD Rate Done')
  
    MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist41_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist42_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist43_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist44_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist45_raster.asc', sep =''))
  
  ###########ldd dist
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=4, lddMaxDist=5,
                   simulName=paste('south_',i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist51_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist52_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist53_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist54_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=5, lddMaxDist=6,
                   simulName=paste('south_',i,'_ldddist6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist61_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist62_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist63_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist64_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=6, lddMaxDist=7,
                   simulName=paste('south_',i,'_ldddist7', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist71_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist72_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist73_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist74_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist75_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=7, lddMaxDist=8,
                   simulName=paste('south_',i,'_ldddist8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist81_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist82_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist83_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist84_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist85_raster.asc', sep =''))
  
  print('LDD Distance Done')
  
  print('All Done Southern Range')
  
}, future.seed = TRUE)

print("ssp245 done")
setwd("../ssp370")

future_lapply(sp_ls, function(i) {
  
  MigClim.migrate(iniDist = paste(i,"_ini_final", sep = ''),
                  hsMap=paste(i,'_hs', sep = ''),
                  rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                  envChgSteps=5,
                  dispSteps=20,
                  dispKernel=c(.1),
                  iniMatAge=1, 
                  propaguleProd=c(1),
                  #lddFreq=0.05, 
                  #lddMinDist=3, 
                  #lddMaxDist=4,
                  simulName=paste("full_",i,'_base', sep = ''), 
                  replicateNb=5,
                  overWrite=TRUE,
                  testMode=FALSE, 
                  fullOutput=FALSE, 
                  keepTempFiles=TRUE)
  
  
  print('Base case done')
  
  ###########age
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=2, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age2/full_',i,'_age21_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age22_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age23_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age24_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=3, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age3', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age3/full_',i,'_age31_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age32_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age33_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age34_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age35_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=4, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age4/full_',i,'_age41_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age42_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age43_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age44_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=5, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=9,
                   simulName=paste("full_",i,'_age5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age5/full_',i,'_age51_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age52_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age53_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age54_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=6, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age6/full_',i,'_age61_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age62_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age63_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age64_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age65_raster.asc', sep =''))
  
  print('Age done')
  
  ###########rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.2),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate21_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate22_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate23_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate24_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.4),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate41_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate42_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate43_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate44_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.6),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate61_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate62_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate63_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate64_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.8),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate81_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate82_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate83_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate84_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate85_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(1),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate105_raster.asc', sep =''))
  
  print('Rate done')
  
  ###########distance
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.33,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.45,.20,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance151_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance152_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance153_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance154_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.684,.468,.320,.219,.150,.102),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance30', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance301_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance302_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance303_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance304_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance305_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.78, 0.61, 0.47, 0.37, 0.29, 0.22, 0.17, 0.14, 0.11),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance45', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance451_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance452_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance453_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance454_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance455_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.83, 0.68, 0.57, 0.47, 0.39, 0.32, 0.26, 0.22, 0.18, 0.15, 0.12, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance60', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance601_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance602_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance603_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance604_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance605_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.90, 0.81, 0.73, 0.66, 0.59, 0.53, 0.48, 0.43, 0.39, 0.35, 0.32, 0.28, 0.26, 0.23, 0.21, 0.19, 0.17, 0.15, 0.14, 0.12, 0.11, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance110', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1105_raster.asc', sep =''))
  
  print('Distance done')
  
  ###########ldd rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate05', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate051_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate052_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate053_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate054_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate055_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.10, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.15, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate151_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate152_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate153_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate154_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.20, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate20', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate201_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate202_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate203_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate204_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate205_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.25, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate25', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate251_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate252_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate253_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate254_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate255_raster.asc', sep =''))
  
  print('LDD Rate Done')
  
  ###########ldd dist
  
    MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist41_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist42_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist43_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist44_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=4, lddMaxDist=5,
                   simulName=paste("full_",i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist51_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist52_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist53_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist54_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=5, lddMaxDist=6,
                   simulName=paste("full_",i,'_ldddist6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist61_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist62_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist63_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist64_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=6, lddMaxDist=7,
                   simulName=paste("full_",i,'_ldddist7', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist71_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist72_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist73_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist74_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist75_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=7, lddMaxDist=8,
                   simulName=paste("full_",i,'_ldddist8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist81_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist82_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist83_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist84_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist85_raster.asc', sep =''))
  
  print('LDD Distance Done')
  
  print('All Done Full Range')
  
  
  
  
  
  
  
  
  #########################south models
  
  
  MigClim.migrate(iniDist = paste(i,"_ini_south_final", sep = ''),
                  hsMap=paste(i,'_hs', sep = ''),
                  rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                  envChgSteps=5,
                  dispSteps=20,
                  dispKernel=c(.1),
                  iniMatAge=1, 
                  propaguleProd=c(1),
                  #lddFreq=0.05, 
                  #lddMinDist=3, 
                  #lddMaxDist=4,
                  simulName=paste('south_',i,'_base', sep = ''), 
                  replicateNb=5,
                  overWrite=TRUE,
                  testMode=FALSE, 
                  fullOutput=FALSE, 
                  keepTempFiles=TRUE)
  
  
  print('Base case done')
  
  ###########age
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=2, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age2/','south_',i,'_age21_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age22_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age23_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age24_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=3, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age3', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age3/south_',i,'_age31_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age32_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age33_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age34_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age35_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=4, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age4/south_',i,'_age41_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age42_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age43_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age44_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=5, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age5/south_',i,'_age51_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age52_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age53_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age54_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=6, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age6/south_',i,'_age61_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age62_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age63_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age64_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age65_raster.asc', sep =''))
  
  print('Age done')
  
  ###########rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.2),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate21_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate22_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate23_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate24_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.4),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate41_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate42_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate43_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate44_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.6),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate61_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate62_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate63_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate64_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.8),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate81_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate82_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate83_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate84_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate85_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(1),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate105_raster.asc', sep =''))
  
  print('Rate done')
  
  ###########distance
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.33,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.45,.20,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance151_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance152_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance153_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance154_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.684,.468,.320,.219,.150,.102),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance30', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance301_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance302_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance303_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance304_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance305_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.78, 0.61, 0.47, 0.37, 0.29, 0.22, 0.17, 0.14, 0.11),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance45', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance451_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance452_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance453_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance454_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance455_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.83, 0.68, 0.57, 0.47, 0.39, 0.32, 0.26, 0.22, 0.18, 0.15, 0.12, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance60', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance601_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance602_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance603_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance604_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance605_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.90, 0.81, 0.73, 0.66, 0.59, 0.53, 0.48, 0.43, 0.39, 0.35, 0.32, 0.28, 0.26, 0.23, 0.21, 0.19, 0.17, 0.15, 0.14, 0.12, 0.11, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance110', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1105_raster.asc', sep =''))
  
  print('Distance done')
  
  ###########ldd rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate05', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate051_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate052_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate053_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate054_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate055_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.10, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.15, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate151_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate152_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate153_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate154_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.20, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate20', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate201_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate202_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate203_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate204_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate205_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.25, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate25', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate251_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate252_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate253_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate254_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate255_raster.asc', sep =''))
  
  print('LDD Rate Done')
  
    MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist41_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist42_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist43_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist44_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist45_raster.asc', sep =''))
  
  ###########ldd dist
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=4, lddMaxDist=5,
                   simulName=paste('south_',i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist51_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist52_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist53_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist54_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=5, lddMaxDist=6,
                   simulName=paste('south_',i,'_ldddist6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist61_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist62_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist63_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist64_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=6, lddMaxDist=7,
                   simulName=paste('south_',i,'_ldddist7', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist71_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist72_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist73_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist74_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist75_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=7, lddMaxDist=8,
                   simulName=paste('south_',i,'_ldddist8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist81_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist82_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist83_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist84_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist85_raster.asc', sep =''))
  
  print('LDD Distance Done')
  
  print('All Done Southern Range')
  
}, future.seed = TRUE)

print("ssp370 done")
setwd("../ssp585")

future_lapply(sp_ls, function(i) {
  
  MigClim.migrate(iniDist = paste(i,"_ini_final", sep = ''),
                  hsMap=paste(i,'_hs', sep = ''),
                  rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                  envChgSteps=5,
                  dispSteps=20,
                  dispKernel=c(.1),
                  iniMatAge=1, 
                  propaguleProd=c(1),
                  #lddFreq=0.05, 
                  #lddMinDist=3, 
                  #lddMaxDist=4,
                  simulName=paste("full_",i,'_base', sep = ''), 
                  replicateNb=5,
                  overWrite=TRUE,
                  testMode=FALSE, 
                  fullOutput=FALSE, 
                  keepTempFiles=TRUE)
  
  
  print('Base case done')
  
  ###########age
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=2, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age2/full_',i,'_age21_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age22_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age23_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age24_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age2/full_',i,'_age25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=3, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age3', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age3/full_',i,'_age31_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age32_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age33_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age34_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age3/full_',i,'_age35_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=4, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age4/full_',i,'_age41_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age42_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age43_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age44_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age4/full_',i,'_age45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=5, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=9,
                   simulName=paste("full_",i,'_age5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age5/full_',i,'_age51_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age52_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age53_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age54_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age5/full_',i,'_age55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=6, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_age6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_age6/full_',i,'_age61_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age62_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age63_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age64_raster.asc', sep =''))
  unlink(paste('./full_',i,'_age6/full_',i,'_age65_raster.asc', sep =''))
  
  print('Age done')
  
  ###########rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.2),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate21_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate22_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate23_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate24_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate2/full_',i,'_rate25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.4),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate41_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate42_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate43_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate44_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate4/full_',i,'_rate45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.6),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate61_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate62_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate63_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate64_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate6/full_',i,'_rate65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.8),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate81_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate82_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate83_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate84_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate8/full_',i,'_rate85_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(1),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_rate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_rate10/full_',i,'_rate105_raster.asc', sep =''))
  
  print('Rate done')
  
  ###########distance
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.33,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance10/full_',i,'_distance105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.45,.20,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance151_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance152_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance153_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance154_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance15/full_',i,'_distance155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.684,.468,.320,.219,.150,.102),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance30', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance301_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance302_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance303_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance304_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance30/full_',i,'_distance305_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.78, 0.61, 0.47, 0.37, 0.29, 0.22, 0.17, 0.14, 0.11),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance45', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance451_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance452_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance453_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance454_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance45/full_',i,'_distance455_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.83, 0.68, 0.57, 0.47, 0.39, 0.32, 0.26, 0.22, 0.18, 0.15, 0.12, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance60', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance601_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance602_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance603_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance604_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance60/full_',i,'_distance605_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.90, 0.81, 0.73, 0.66, 0.59, 0.53, 0.48, 0.43, 0.39, 0.35, 0.32, 0.28, 0.26, 0.23, 0.21, 0.19, 0.17, 0.15, 0.14, 0.12, 0.11, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_distance110', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_distance110/full_',i,'_distance1105_raster.asc', sep =''))
  
  print('Distance done')
  
  ###########ldd rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate05', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate051_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate052_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate053_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate054_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate05/full_',i,'_lddrate055_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.10, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate101_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate102_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate103_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate104_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate10/full_',i,'_lddrate105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.15, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate151_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate152_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate153_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate154_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate15/full_',i,'_lddrate155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.20, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate20', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate201_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate202_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate203_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate204_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate20/full_',i,'_lddrate205_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.25, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_lddrate25', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate251_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate252_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate253_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate254_raster.asc', sep =''))
  unlink(paste('./full_',i,'_lddrate25/full_',i,'_lddrate255_raster.asc', sep =''))
  
  print('LDD Rate Done')
  
  ###########ldd dist
  
    MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste("full_",i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist41_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist42_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist43_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist44_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist4/full_',i,'_ldddist45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=4, lddMaxDist=5,
                   simulName=paste("full_",i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist51_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist52_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist53_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist54_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist5/full_',i,'_ldddist55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=5, lddMaxDist=6,
                   simulName=paste("full_",i,'_ldddist6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist61_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist62_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist63_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist64_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist6/full_',i,'_ldddist65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=6, lddMaxDist=7,
                   simulName=paste("full_",i,'_ldddist7', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist71_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist72_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist73_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist74_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist7/full_',i,'_ldddist75_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=7, lddMaxDist=8,
                   simulName=paste("full_",i,'_ldddist8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist81_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist82_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist83_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist84_raster.asc', sep =''))
  unlink(paste('./full_',i,'_ldddist8/full_',i,'_ldddist85_raster.asc', sep =''))
  
  print('LDD Distance Done')
  
  print('All Done Full Range')
  
  
  
  
  
  
  
  
  #########################south models
  
  
  MigClim.migrate(iniDist = paste(i,"_ini_south_final", sep = ''),
                  hsMap=paste(i,'_hs', sep = ''),
                  rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                  envChgSteps=5,
                  dispSteps=20,
                  dispKernel=c(.1),
                  iniMatAge=1, 
                  propaguleProd=c(1),
                  #lddFreq=0.05, 
                  #lddMinDist=3, 
                  #lddMaxDist=4,
                  simulName=paste('south_',i,'_base', sep = ''), 
                  replicateNb=5,
                  overWrite=TRUE,
                  testMode=FALSE, 
                  fullOutput=FALSE, 
                  keepTempFiles=TRUE)
  
  
  print('Base case done')
  
  ###########age
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=2, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age2/','south_',i,'_age21_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age22_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age23_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age24_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age2/','south_',i,'_age25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=3, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age3', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age3/south_',i,'_age31_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age32_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age33_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age34_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age3/south_',i,'_age35_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=4, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age4/south_',i,'_age41_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age42_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age43_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age44_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age4/south_',i,'_age45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=5, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age5/south_',i,'_age51_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age52_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age53_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age54_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age5/south_',i,'_age55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=6, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_age6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_age6/south_',i,'_age61_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age62_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age63_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age64_raster.asc', sep =''))
  unlink(paste('./south_',i,'_age6/south_',i,'_age65_raster.asc', sep =''))
  
  print('Age done')
  
  ###########rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.2),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate2', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate21_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate22_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate23_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate24_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate2/south_',i,'_rate25_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.4),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate4', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate41_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate42_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate43_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate44_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate4/south_',i,'_rate45_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.6),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate61_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate62_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate63_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate64_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate6/south_',i,'_rate65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.8),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate81_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate82_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate83_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate84_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate8/south_',i,'_rate85_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(1),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_rate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_rate10/south_',i,'_rate105_raster.asc', sep =''))
  
  print('Rate done')
  
  ###########distance
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.33,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance10/south_',i,'_distance105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.45,.20,.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance151_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance152_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance153_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance154_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance15/south_',i,'_distance155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.684,.468,.320,.219,.150,.102),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance30', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance301_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance302_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance303_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance304_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance30/south_',i,'_distance305_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.78, 0.61, 0.47, 0.37, 0.29, 0.22, 0.17, 0.14, 0.11),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance45', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance451_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance452_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance453_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance454_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance45/south_',i,'_distance455_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.83, 0.68, 0.57, 0.47, 0.39, 0.32, 0.26, 0.22, 0.18, 0.15, 0.12, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance60', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance601_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance602_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance603_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance604_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance60/south_',i,'_distance605_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(0.90, 0.81, 0.73, 0.66, 0.59, 0.53, 0.48, 0.43, 0.39, 0.35, 0.32, 0.28, 0.26, 0.23, 0.21, 0.19, 0.17, 0.15, 0.14, 0.12, 0.11, 0.10),
                   iniMatAge=1, propaguleProd=c(1),
                   #lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_distance110', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_distance110/south_',i,'_distance1105_raster.asc', sep =''))
  
  print('Distance done')
  
  ###########ldd rate
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate05', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate051_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate052_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate053_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate054_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate05/south_',i,'_lddrate055_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.10, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate10', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate101_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate102_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate103_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate104_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate10/south_',i,'_lddrate105_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.15, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate15', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate151_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate152_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate153_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate154_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate15/south_',i,'_lddrate155_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.20, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate20', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate201_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate202_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate203_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate204_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate20/south_',i,'_lddrate205_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.25, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_lddrate25', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate251_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate252_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate253_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate254_raster.asc', sep =''))
  unlink(paste('./south_',i,'_lddrate25/south_',i,'_lddrate255_raster.asc', sep =''))
  
  print('LDD Rate Done')
  
    MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                   simulName=paste('south_',i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist41_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist42_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist43_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist44_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist4/south_',i,'_ldddist45_raster.asc', sep =''))
  
  ###########ldd dist
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=4, lddMaxDist=5,
                   simulName=paste('south_',i,'_ldddist5', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist51_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist52_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist53_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist54_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist5/south_',i,'_ldddist55_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=5, lddMaxDist=6,
                   simulName=paste('south_',i,'_ldddist6', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist61_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist62_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist63_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist64_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist6/south_',i,'_ldddist65_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=6, lddMaxDist=7,
                   simulName=paste('south_',i,'_ldddist7', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist71_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist72_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist73_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist74_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist7/south_',i,'_ldddist75_raster.asc', sep =''))
  
  MigClim.migrate (iniDist = paste(i,"_ini_south_final", sep = ''),
                   hsMap=paste(i,'_hs', sep = ''),
                   rcThreshold = round(as.numeric(get(paste(i,'_quant', sep = '')))),
                   envChgSteps=5,
                   dispSteps=20,
                   dispKernel=c(.1),
                   iniMatAge=1, propaguleProd=c(1),
                   lddFreq=0.05, lddMinDist=7, lddMaxDist=8,
                   simulName=paste('south_',i,'_ldddist8', sep = ''), replicateNb=5, overWrite=TRUE,
                   testMode=FALSE, fullOutput=FALSE, keepTempFiles=TRUE)
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist81_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist82_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist83_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist84_raster.asc', sep =''))
  unlink(paste('./south_',i,'_ldddist8/south_',i,'_ldddist85_raster.asc', sep =''))
  
  print('LDD Distance Done')
  
  print('All Done Southern Range')
  
}, future.seed = TRUE)

print("ssp585 done")