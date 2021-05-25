library(terra)
library(tidyverse)
library(ENMeval)
#for some reason the next two are required by ENMeval but did not install or load when I updated the package, so manual load
library(ecospat)
library(rangeModelMetadata)

#start by bringing in the final point file for all fo the species
points_all_sp <- read.csv('./outputs/data_proc/cleaned_points.csv')

#need to break up points to individual species
ABMA_points <- points_all_sp %>% dplyr::filter(species == 'Ambystoma macrodactylum')
ANBO_points <- points_all_sp %>% dplyr::filter(species == 'Anaxyrus boreas')
ANHE_points <- points_all_sp %>% dplyr::filter(species == 'Anaxyrus hemiophrys')
LISY_points <- points_all_sp %>% dplyr::filter(species == 'Lithobates sylvaticus')
PSMA_points <- points_all_sp %>% dplyr::filter(species == 'Pseudacris maculata')
RALU_points <- points_all_sp %>% dplyr::filter(species == 'Rana luteiventris')

ABMA_points$species <- NULL
ANBO_points$species <- NULL
ANHE_points$species <- NULL
LISY_points$species <- NULL
PSMA_points$species <- NULL
RALU_points$species <- NULL

#read in all of the model predictors which were cropped per species
#note that raster stacks are needed here and that SpatRaster does not work
ABMA_predictors <- raster::stack('./outputs/data_proc/present_final/ABMA_predictors_final.tiff')
ANBO_predictors <- raster::stack('./outputs/data_proc/present_final/ANBO_predictors_final.tiff')
ANHE_predictors <- raster::stack('./outputs/data_proc/present_final/ANHE_predictors_final.tiff')
LISY_predictors <- raster::stack('./outputs/data_proc/present_final/LISY_predictors_final.tiff')
PSMA_predictors <- raster::stack('./outputs/data_proc/present_final/PSMA_predictors_final.tiff')
RALU_predictors <- raster::stack('./outputs/data_proc/present_final/RALU_predictors_final.tiff')

#Next is to run ENMeval to check regularization multiplier values
#set up model list to test
tune_args_list  <- list(fc = c("L","Q","LQ","LQH", "H"), rm = c(0.5,1:4))

#running enmeval independently to make watching progress easier
enmeval_results_ABMA <- ENMevaluate(ABMA_points, ABMA_predictors, n.bg=10000, tune.args = tune_args_list, partitions='checkerboard2', algorithm='maxnet')
#pull out the best model
#this shows best regularization and fitting types
ABMA_eval <- eval.results(enmeval_results_ABMA)
#write the best model
write.csv(ABMA_eval, "./outputs/migclim/ABMA.csv")



enmeval_results_ANBO <- ENMevaluate(ANBO_points, ANBO_predictors, n.bg=10000, tune.args = tune_args_list, partitions='checkerboard2', algorithm='maxnet')
ANBO_eval <- eval.results(enmeval_results_ANBO)
write.csv(ANBO_eval, "./outputs/migclim/ANBO.csv")

enmeval_results_ANHE <- ENMevaluate(ANHE_points, ANHE_predictors, n.bg=10000, tune.args = tune_args_list, partitions='checkerboard2', algorithm='maxnet')
ANHE_eval <- eval.results(enmeval_results_ANHE)
write.csv(ANHE_eval, "./outputs/migclim/ANHE.csv")

enmeval_results_LISY <- ENMevaluate(LISY_points, LISY_predictors, n.bg=10000, tune.args = tune_args_list, partitions='checkerboard2', algorithm='maxnet')
LISY_eval <- eval.results(enmeval_results_LISY)
write.csv(LISY_eval, "./outputs/migclim/LISY.csv")

enmeval_results_PSMA <- ENMevaluate(PSMA_points, PSMA_predictors, n.bg=10000, tune.args = tune_args_list, partitions='checkerboard2', algorithm='maxnet')
PSMA_eval <- eval.results(enmeval_results_PSMA)
write.csv(PSMA_eval, "./outputs/migclim/PSMA.csv")

enmeval_results_RALU <- ENMevaluate(RALU_points, RALU_predictors, n.bg=10000, tune.args = tune_args_list, partitions='checkerboard2', algorithm='maxnet')
RALU_eval <- eval.results(enmeval_results_RALU)
write.csv(RALU_eval, "./outputs/migclim/RALU.csv")






