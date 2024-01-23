
# ABOUT THIS SCRIPT -------------------------------------------------------

# Author: Abby Williams
# Created: 15/01/2024
# Purpose: extract shapefile from image colours for map digitisation

# SETUP -------------------------------------------------------------------

library(raster)
library(dplyr)
library(ggplot2)
library(terra)
library(sf)
library(spex)


# CODE -----------------------------------------------------------

# import raster *** EDIT FILE PATH ***

rast <- terra::rast('C:/Users/Abby Williams/Desktop/Georeferencing/ALL PROJECTS/1775/1775_goreferenced.tif')

# plot image

plotRGB(rast)

# crop if needed
# 
# rast
# rast_extent <- extent()
# rast <- crop(rast,rast_extent)
# plot(rast)

# identify most common combinations of rgb codes to identify reference region colour

rast_df <- as.data.frame(rast)

rast_df <- rast_df[1:3] # removing extra bands if present

colnames(rast_df) <- c('band1','band2','band3')

rast_df %>% 
  group_by(band1,band2,band3) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  print(n=30)

# *** CHOOSE CORRECT COLOUR ***

target_rgb <- c(255,109,235)

# display colour to check it's correct

ggplot()+
  theme(panel.background = element_rect(fill=rgb(target_rgb[1]/255, target_rgb[2]/255, target_rgb[3]/255)))

# *** DEFINE APPROPRIATE TOLERANCE INTERVAL ***

tolerance <- 30

# select pixels with correct RGB values

matching_pixels <- which(
  rast_df$band1 >= target_rgb[1] - tolerance &
    rast_df$band1 <= target_rgb[1] + tolerance &
    rast_df$band2 >= target_rgb[2] - tolerance &
    rast_df$band2 <= target_rgb[2] + tolerance &
    rast_df$band3 >= target_rgb[3] - tolerance &
    rast_df$band3 <= target_rgb[3] + tolerance,
  arr.ind = TRUE
)

# make blank copy of original raster

binary_rast <- raster(rast)
binary_rast[] <- NA

# assign pixels of interest to 1

binary_rast[matching_pixels] <- 1
plot(binary_rast,col='blue')

# make polygon

reference_polygon <- rasterToPolygons(binary_rast,dissolve=T,digits=6) # may need to play with number of digits here
plot(reference_polygon)

# plot polygon with original map to check

plotRGB(rast)
plot(reference_polygon,col='red',add=T)

# export *** EDIT FILE PATH ***

shapefile(reference_polygon, 'C:/Users/Abby Williams/Desktop/Georeferencing/Shapefiles_current/1775_reference.shp',overwrite=T)


