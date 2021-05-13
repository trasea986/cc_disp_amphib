#dan viz help


library(ggmap)
library(maps)
library(mapdata)
library(cowplot)
library(ggthemes)
library(sf)
library(RColorBrewer)
library(maptools)
library(FRK)

extent_dan <- extent(-125, -111, 42, 49)

#I need to take my predict map back to lat/long

predict_repro_lat_long <- projectRaster(predict, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#crop the ENM predict map by desired extent

predict_map_crop <- crop(predict_repro_lat_long, extent_dan)

#reproduce to the projection Dan is using

predict_map_repro <- projectRaster(predict_map_crop, crs="+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-117 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83")

#check plot
plot(predict_map_repro)

#convert to df for ggplot
predict_map_repro_df <- as.data.frame(predict_map_repro, xy=TRUE)

#pull in map data
states_plot <- c("washington", "oregon", "idaho", "montana")
dmap <- map("state", regions=states_plot, col="transparent", plot=FALSE, fill = TRUE)
area_poly <- map2SpatialPolygons(dmap, IDs=dmap$names, , proj4string=CRS("+proj=longlat +datum=WGS84"))

#then reproject state lines to match the prediction raster's crs
area_poly_repro <- spTransform(area_poly, crs(predict_map_repro))
plot(area_poly_repro)

#make plot
ggplot() +
  geom_tile(data=predict_map_repro_df, aes(x=x, y=y, fill=layer), alpha=0.8) + #plot the ENM
  scale_fill_gradient2("ENM Value",
                       low = 'aquamarine', high = 'aquamarine4',
                       na.value = NA) + #use a gradient from light to dark (4) colors darkgrey county lines
  geom_polygon(data = area_poly_repro, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  coord_fixed(1.3) + #establish coordinates
  xlim(-677908, 500000)+
  ggtitle("Dan, 2020") + #title of the plot
  theme_map(base_size = 16) + #base font size
  theme(legend.position="bottom") + #where to put the legend
  theme(plot.title = element_text(face = "italic")) + #use italics for the ggtitle
  theme(legend.key.width=unit(1, "cm")) #how big the legend key should be
