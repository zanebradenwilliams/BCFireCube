##BC Wildfire Fire Zones

##Initialization: Download BC Wildfire Fire Zones from https://catalogue.data.gov.bc.ca/dataset/217bea05-4a6f-4ef8-8eb8-9b352e90828f

##Store data in /BCFireCube/Data/Firezone

## Step 1: Call libraries

library(sf)
library(terra)
library(tidyterra)

## Step 2: Call data and rename variables

#load admin boundaries
admin <- st_read("Data/Firezone/DRPMFFRZNS_polygon.shp")

#rename relevant variables
admin <- admin %>% mutate(fire_centre = MFFRCNTRNM, fire_zone = MFFRZNNM)

#select relevant variables
admin <- admin %>% select(fire_centre, fire_zone)

## Step 3: Reproject to harmonized CRS

#convert to terra object
admin <- vect(admin)

#reproject polygons to EPSG:3979
admin <- admin %>% project("EPSG:3979")

## Step 4: Save as gpkg

#write gpkg
writeVector(admin, "Products/2022_bcvect_firezones.gpkg")
