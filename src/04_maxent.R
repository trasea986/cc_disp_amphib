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


# next is to run maxent.
#give java extra memory
options(java.parameters = "-Xmx4g" )

#species shorthand... maybe port this to the start to speed up some of the tasks above.

sp_ls <- c("ABMA", "ANBO", "ANHE", "LISY", "PSMA", "RALU")

#create a loop to run through all species

#create empty list to save the MaxEnt objects if running in a loop
#MaxEnt_list <- list()

#originally looped this, but each one has its optimized RM and features post-ENMeval. Also set random number of points for training as 20% of total point number

#this is example code for the loop

#for (i in sp_ls) {
#  output_model <- 
#    maxent(x=get(paste(i,'_predictors', sep ='')),
#           p=get(paste(i,'_points', sep ='')),
#           removeDuplicates=TRUE, args=c(
#    'maximumbackground=10000',
#    'defaultprevalence=1.00',
#    'betamultiplier=0.5',
#    'plots=true',
#    'pictures=true',
#    'randomtestpoints=30',
#    'linear=true',
#    'quadratic=true',
#    'product=false',
#    'threshold=false',
#    'hinge=false',
#    'threads=6',
#    'responsecurves=true',
#    'jackknife=true',
#    'askoverwrite=false'
#  ),
#  path = paste("./outputs/maxent/",i, sep =''))
  
#  MaxEnt_list[[i]] <- output_model
#}


ABMA_model <- 
      maxent(x=ABMA_predictors,
             p=ABMA_points,
             removeDuplicates=TRUE, args=c(
      'maximumbackground=10000',
      'defaultprevalence=1.00',
      'betamultiplier=1',
      'plots=true',
      'pictures=true',
      'linear=false',
      'quadratic=false',
    'product=false',
    'threshold=false',
    'hinge=true',
    'threads=6',
    'responsecurves=true',
    'jackknife=true',
    'askoverwrite=false',
    'replicates=10',
    'replicatetype=crossvalidate'),
    path = './outputs/maxent/ABMA')


ANBO_model <- 
  maxent(x=ANBO_predictors,
         p=ANBO_points,
         removeDuplicates=TRUE, args=c(
           'maximumbackground=10000',
           'defaultprevalence=1.00',
           'betamultiplier=1',
           'plots=true',
           'pictures=true',
           'linear=true',
           'quadratic=true',
           'product=false',
           'threshold=false',
           'hinge=true',
           'threads=6',
           'responsecurves=true',
           'jackknife=true',
           'askoverwrite=false',
           'replicates=10',
           'replicatetype=crossvalidate'),
         path = './outputs/maxent/ANBO')

#ANHE is tricky, based on weights, two very similar models at almost 0.5 and 0.5, went with the one with the lower number of coefficients

ANHE_model <- 
  maxent(x=ANHE_predictors,
         p=ANHE_points,
         removeDuplicates=TRUE, args=c(
           'maximumbackground=10000',
           'defaultprevalence=1.00',
           'betamultiplier=1',
           'plots=true',
           'pictures=true',
           'linear=true',
           'quadratic=true',
           'product=false',
           'threshold=false',
           'hinge=true',
           'threads=6',
           'responsecurves=true',
           'jackknife=true',
           'askoverwrite=false',
           'replicates=10',
           'replicatetype=crossvalidate'),
         path = './outputs/maxent/ANHE')

LISY_model <- 
  maxent(x=LISY_predictors,
         p=LISY_points,
         removeDuplicates=TRUE, args=c(
           'maximumbackground=10000',
           'defaultprevalence=1.00',
           'betamultiplier=0.5',
           'plots=true',
           'pictures=true',
           'linear=true',
           'quadratic=true',
           'product=false',
           'threshold=false',
           'hinge=true',
           'threads=6',
           'responsecurves=true',
           'jackknife=true',
           'askoverwrite=false',
           'replicates=10',
           'replicatetype=crossvalidate'),
         path = './outputs/maxent/LISY')

PSMA_model <- 
  maxent(x=PSMA_predictors,
         p=PSMA_points,
         removeDuplicates=TRUE, args=c(
           'maximumbackground=10000',
           'defaultprevalence=1.00',
           'betamultiplier=0.5',
           'plots=true',
           'pictures=true',
           'linear=false',
           'quadratic=false',
           'product=false',
           'threshold=false',
           'hinge=true',
           'threads=6',
           'responsecurves=true',
           'jackknife=true',
           'askoverwrite=false',
           'replicates=10',
           'replicatetype=crossvalidate'),
         path = './outputs/maxent/PSMA')

RALU_model <- 
  maxent(x=RALU_predictors,
         p=RALU_points,
         removeDuplicates=TRUE, args=c(
           'maximumbackground=10000',
           'defaultprevalence=1.00',
           'betamultiplier=2',
           'plots=true',
           'pictures=true',
           'linear=true',
           'quadratic=true',
           'product=false',
           'threshold=false',
           'hinge=true',
           'threads=6',
           'responsecurves=true',
           'jackknife=true',
           'askoverwrite=false',
           'replicates=10',
           'replicatetype=crossvalidate'),
         path = './outputs/maxent/RALU')

#before doing predictions, going to pull in the variable importance and write to csv.

MaxEnt_list <- list(ABMA_model, ANBO_model, ANHE_model, LISY_model, PSMA_model, RALU_model)


write.csv(MaxEnt_list[[1]]@results, './outputs/maxent/ABMA_result.csv')
write.csv(MaxEnt_list[[2]]@results, './outputs/maxent/ANBO_result.csv')
write.csv(MaxEnt_list[[3]]@results, './outputs/maxent/ANHE_result.csv')
write.csv(MaxEnt_list[[4]]@results, './outputs/maxent/LISY_result.csv')
write.csv(MaxEnt_list[[5]]@results, './outputs/maxent/PSMA_result.csv')
write.csv(MaxEnt_list[[6]]@results, './outputs/maxent/RALU_result.csv')