library(terra)

#defining projection object in case this is run not after complete the 01 script
projection <- "ESRI:102008"

#start by bringing in the final point file for all fo the species
points_all_sp <- read.csv('./outputs/data_proc/cleaned_points.csv')

#read in all of the model predictors which were cropped per species
ABMA_predictors <- rast('./outputs/data_proc/present_final/ABMA_predictors_final.tiff')
ANBO_predictors <- rast('./outputs/data_proc/present_final/ANBO_predictors_final.tiff')
ANHE_predictors <- rast('./outputs/data_proc/present_final/ANHE_predictors_final.tiff')
LISY_predictors <- rast('./outputs/data_proc/present_final/LISY_predictors_final.tiff')
PSMA_predictors <- rast('./outputs/data_proc/present_final/PSMA_predictors_final.tiff')
RALU_predictors <- rast('./outputs/data_proc/present_final/RALU_predictors_final.tiff')