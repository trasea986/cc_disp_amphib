library(terra)
library(dismo)
library(tidyverse)

#start with env variables
#load in rasters for creating the models.
ABMA_predictors <- stack('./outputs/data_proc/present_final/ABMA_predictors_final.tiff')
ANBO_predictors <- stack('./outputs/data_proc/present_final/ANBO_predictors_final.tiff')
ANHE_predictors <- stack('./outputs/data_proc/present_final/ANHE_predictors_final.tiff')
LISY_predictors <- stack('./outputs/data_proc/present_final/LISY_predictors_final.tiff')
PSMA_predictors <- stack('./outputs/data_proc/present_final/PSMA_predictors_final.tiff')
RALU_predictors <- stack('./outputs/data_proc/present_final/RALU_predictors_final.tiff')

#load in rasters for predicting steps
#present day prediction
predictors_final <- stack('./outputs/data_proc/present_final/predictors_final.tiff')

#future prediction
future_files <- list.files(path = './outputs/data_proc/future_final', pattern = '*.tif', all.files = TRUE, full.names = TRUE)

#load in the rasters
future_layers <- lapply(future_files, function(i){stack(i)})

#assign names to the list. this is based on the order of "source" attribute, which is only visible when bringing in above with rast/a SpatRaster
names(future_layers) <- c("ssp245_2021-2040", "ssp245_2041-2060", "ssp245_2061-2080", "ssp245_2081-2100", "ssp370_2021-2040", "ssp370_2041-2060", "ssp370_2061-2080", "ssp370_2081-2100", "ssp585_2021-2040", "ssp585_2041-2060", "ssp585_2061-2080", "ssp585_2081-2100")

list2env(future_layers, .GlobalEnv)

#last, need to rename present day to match future
layer_names <- c('wc2_1', 'wc2_18', 'wc2_19', 'wc2_2', 'wc2_3', 'wc2_5', 'wc2_7', 'wc2_8', 'wc2_13', 'wc2_15')

predictors_final <- setNames(predictors_final, layer_names)
ABMA_predictors <- setNames(ABMA_predictors, layer_names)
ANBO_predictors <- setNames(ANBO_predictors, layer_names)
ANHE_predictors <- setNames(ANHE_predictors, layer_names)
LISY_predictors <- setNames(LISY_predictors, layer_names)
PSMA_predictors <- setNames(PSMA_predictors, layer_names)
RALU_predictors <- setNames(RALU_predictors, layer_names)

#next, bring in the point file
points_all_sp <- read.csv('./outputs/data_proc/cleaned_points.csv')

#next up is to remove any points with NA predictor variable values.
coordinates(points_all_sp) <- ~decimalLongitude+decimalLatitude
rast_values <- raster::extract(predictors_final, points_all_sp)
points_all_sp <- as.data.frame(points_all_sp)
points_all_sp <- cbind(points_all_sp,rast_values)

#use this to get grand total... 20 points need to be dropped
sum(!complete.cases(points_all_sp))

points_all_sp <- points_all_sp[complete.cases(points_all_sp), ]

#need to break up points to individual species
ABMA_points <- points_all_sp %>% dplyr::filter(species == 'Ambystoma macrodactylum')
ANBO_points <- points_all_sp %>% dplyr::filter(species == 'Anaxyrus boreas')
ANHE_points <- points_all_sp %>% dplyr::filter(species == 'Anaxyrus hemiophrys')
LISY_points <- points_all_sp %>% dplyr::filter(species == 'Lithobates sylvaticus')
PSMA_points <- points_all_sp %>% dplyr::filter(species == 'Pseudacris maculata')
RALU_points <- points_all_sp %>% dplyr::filter(species == 'Rana luteiventris')

#maxent only wants points
ABMA_points <- ABMA_points %>% select('decimalLongitude', 'decimalLatitude')
ANBO_points <- ANBO_points %>% select('decimalLongitude', 'decimalLatitude') 
ANHE_points <- ANHE_points %>% select('decimalLongitude', 'decimalLatitude') 
LISY_points <- LISY_points %>% select('decimalLongitude', 'decimalLatitude') 
PSMA_points <- PSMA_points %>% select('decimalLongitude', 'decimalLatitude') 
RALU_points <- RALU_points %>% select('decimalLongitude', 'decimalLatitude')


#one last thing before running Maxent, going to check bioclim1 (mean annual temperature) and make a graph to show the differences through time across the three SSPs. this also helps with checking to make sure order/names didn't get messed up across North America.

# next is to run maxent.
#give java extra memory
options(java.parameters = "-Xmx4g" )

#species shorthand... maybe port this at the start to speed up some of the tasks above.

sp_ls <- c("ABMA", "ANBO", "ANHE", "LISY", "PSMA", "RALU")

ABMA_model <-maxent(x=ABMA_predictors, p=ABMA_points, removeDuplicates=TRUE, args=c(
  'maximumbackground=10000',
  'defaultprevalence=1.00',
  'betamultiplier=0.5',
  'plots=true',
  'pictures=true',
  'randomtestpoints=30',
  'linear=true',
  'quadratic=true',
  'product=false',
  'threshold=false',
  'hinge=false',
  'threads=6',
  'responsecurves=true',
  'jackknife=true',
  'askoverwrite=false'
),
path = "./outputs/maxent/ABMA")



