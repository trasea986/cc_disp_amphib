#make sure you install gbif from GitHub and not using the one on CRAN
#remotes::install_github("ropensci/rgbif")

library(rgbif)
library(taxize)
library(rnaturalearthdata)
library(rnaturalearth)
library(CoordinateCleaner)
library(dismo)
library(maps)
library(mapproj)
library(ggthemes)
library(sf)
library(tidyverse)


#load in gbif points
sp_list <- c("Ambystoma macrodactylum", "Lithobates sylvaticus", "Rana luteiventris", "Anaxyrus hemiophrys", "Anaxyrus boreas", "Pseudacris maculata")

#need to run 01a_credentials.R first, to set up objects for login info

gbif_taxon_keys <- 
  sp_list %>%
  get_gbifid_(method="backbone") # match names to the GBIF backbone to get taxonkeys

#take the list of info and pull out just species key
keys <- as.data.frame(unlist(lapply(gbif_taxon_keys, "[", , "specieskey")))

colnames(keys) = c("key")

#remove duplicated ABMA key
keys <- unique(keys$key)

#prepare download list from gbif
occ_prep <- occ_download(
  pred_in("taxonKey", keys),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)

#this can take a while, to check status
occ_download_list(user=user,pwd=pwd)

occ_get <- occ_download_get(occ_prep, path = "./data_raw/points/", overwrite = TRUE)

DOI_list <- gbif_citation(occ_get)

#write DOI
#write.csv(DOI_list$download, file = "./data_raw/points/DOI.csv")

occ <- occ_download_import(occ_get)

occ_df <- as.data.frame(occ)

#can also load in the actual downloaded csv file
occ_df <- read.delim("./data_raw/points/0278575-200613084148143.csv", row.names=NULL)

#check for taxonomy issues, unique function pull out all unique values
unique(occ_df$species)

occ_df <- occ_df %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, individualCount, gbifID, family, taxonRank, year, coordinateUncertaintyInMeters, basisOfRecord, institutionCode)

#remove records without coordinates, or really uncertain coordinates
occ_df <- occ_df %>%
  filter(!is.na(decimalLongitude)) %>%
  filter(!is.na(decimalLatitude)) %>%
  filter(coordinateUncertaintyInMeters < 1000)

#remove records based off of time, WorldClim "present" = 1970-2000s
occ_df <- occ_df %>%
  filter(year >= 1970)

#remove duplicate rows based on lon/lat
occ_df <- occ_df %>%
  distinct(decimalLongitude, decimalLatitude, .keep_all=TRUE)

#check for points with other issues
flags <- clean_coordinates(x = occ_df,
                           lon ="decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("centroids", "equal", "gbif", "institutions", "zeros", "seas"))

#remove flagged points
occ_df <- occ_df[flags$.summary,]



#reproject points to albers equal area

#set project projection
projection <- "ESRI:102008"

points_spatial <- occ_df
coordinates(points_spatial) <- ~decimalLongitude+decimalLatitude
projection(points_spatial) <- CRS('+proj=longlat +datum=WGS84')
points_spatial <- spTransform(points_spatial, projection)

#next up is correcting for sampling bias
r <- raster(points_spatial)

# set the resolution of the cells t 2.5 kilo meters
res(r) <- 2500

# expand (extend the extent) of the RasterLayer a little.
r <- extend(r, extent(r)+10000)

# sample random points for each species
points_spatial_df <- as.data.frame(points_spatial)

points_ABMA <- points_spatial_df %>% filter(species == "Ambystoma macrodactylum")
points_ANBO <- points_spatial_df %>% filter(species == "Anaxyrus boreas")
points_ANHE <- points_spatial_df %>% filter(species == "Anaxyrus hemiophrys")
points_LISY <- points_spatial_df %>% filter(species == "Lithobates sylvaticus")
points_PSMA <- points_spatial_df %>% filter(species == "Pseudacris maculata")
points_RALU <- points_spatial_df %>% filter(species == "Rana luteiventris")

coordinates(points_ABMA) <- ~decimalLongitude+decimalLatitude
coordinates(points_ANBO) <- ~decimalLongitude+decimalLatitude
coordinates(points_ANHE) <- ~decimalLongitude+decimalLatitude
coordinates(points_LISY) <- ~decimalLongitude+decimalLatitude
coordinates(points_PSMA) <- ~decimalLongitude+decimalLatitude
coordinates(points_RALU) <- ~decimalLongitude+decimalLatitude

projection(points_ABMA) <- CRS(projection)
projection(points_ANBO) <- CRS(projection)
projection(points_ANHE) <- CRS(projection)
projection(points_LISY) <- CRS(projection)
projection(points_PSMA) <- CRS(projection)
projection(points_RALU) <- CRS(projection)

#only sampling one per grid
points_ABMA_final <- gridSample(points_ABMA, r, n=1)
points_ANBO_final <- gridSample(points_ANBO, r, n=1)
points_ANHE_final <- gridSample(points_ANHE, r, n=1)
points_LISY_final <- gridSample(points_LISY, r, n=1)
points_PSMA_final <- gridSample(points_PSMA, r, n=1)
points_RALU_final <- gridSample(points_RALU, r, n=1)

#combine then all after adding species designation
points_ABMA_final_df <- as.data.frame(points_ABMA_final)
points_ABMA_final_df$species <- "Ambystoma macrodactylum"

points_ANBO_final_df <- as.data.frame(points_ANBO_final)
points_ANBO_final_df$species <- "Anaxyrus boreas"

points_ANHE_final_df <- as.data.frame(points_ANHE_final)
points_ANHE_final_df$species <- "Anaxyrus hemiophrys"

points_LISY_final_df <- as.data.frame(points_LISY_final)
points_LISY_final_df$species <- "Lithobates sylvaticus"

points_PSMA_final_df <- as.data.frame(points_PSMA_final)
points_PSMA_final_df$species <- "Pseudacris maculata"

points_RALU_final_df <- as.data.frame(points_RALU_final)
points_RALU_final_df$species <- "Rana luteiventris"

occ_df_final <- rbind(points_ABMA_final_df, points_ANBO_final_df, points_ANHE_final_df, points_LISY_final_df, points_PSMA_final_df, points_RALU_final_df)

#tally point count
point_count <- occ_df_final %>%
  group_by(species) %>%
  tally()

#write table of points used count and the input data
#write.csv(point_count, file="./outputs/supp_table1.csv", row.names = FALSE)
#write.csv(occ_df_final, file="./outputs/data_proc/cleaned_points.csv", row.names = FALSE)

#make plot to check points

#load in the counties for mapping
world <- ne_countries(scale = "medium", returnclass = "sf")

#reproject world map
world_repro <- st_transform(world, CRS("ESRI:102008"))

#take reprojected points_final and make a dataframe

supp_fig1 <- ggplot() +
  geom_sf(data=world_repro, color = "black", fill = "white", alpha=0.8) +
  geom_point(data = occ_df_final, aes(x = decimalLongitude, y = decimalLatitude, color = species), size = 1.5, alpha = 0.5) +
  scale_colour_manual(values = c("orange", "blue3", "grey35", "green4", "deeppink", "purple")) +
  facet_wrap(~species)+
  xlab("Longitude") +
  ylab("Latitude") +
  ylim(-1100471, 4736542)+
  scale_x_continuous(limits = c(-3530000, 2670000), breaks = seq(-140, -60, by = 20)) +
  theme_classic(base_size = 15) + #base font size
  theme(legend.position = "none", strip.text = element_text(face = "italic"))

#looks like one L. sylvaticus point is wrong/in Europe, hence the above sort to find the limits, but will not have environment data to go with it for actual model.

ggsave(filename = "./outputs/supp_figure1.jpeg", plot = supp_fig1, , width = 12, height = 12)
