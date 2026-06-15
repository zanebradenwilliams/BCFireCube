##Freshwater Atlas Watersheds

##Initialization: Download BC Wildfire Fire Zones from https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-watersheds

##Store data in /BCFireCube/Data/Watersheds

## Step 1: Call libraries

library(sf)
library(terra)
library(tidyterra)

## Step 2: Call data and rename variables

#load admin boundaries
water <- st_read("Data/Watersheds/fwa_watershed_groups.geojson")

#rename relevant variables
water <- water %>% mutate(watershed_code = WATERSHED_GROUP_CODE, watershed_name = WATERSHED_GROUP_NAME)

#select relevant variables
water <- water %>% select(watershed_code, watershed_name)

## Step 3: Reproject to harmonized CRS

#convert to terra object
water <- vect(water)

#reproject polygons to EPSG:3979
water <- water %>% project("EPSG:3979")

## Step 4: Save as gpkg

#write gpkg
writeVector(water, "Products/2024_bcvect_watersheds.gpkg")
