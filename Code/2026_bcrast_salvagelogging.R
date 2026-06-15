##Salvage Logging (2026)

##Initialization: download data from https://open.canada.ca/data/en/dataset/2af751e7-79f9-4da8-9b45-14688818dca3

##Store unzipped data in /BCFireCube/Data/SalvageLogging

##Please have also completed running the following R scripts:
#2020_bcvect_extent.R
#2014_bcrast_dem.R

## Step 1: Call libraries

library(terra)
library(tidyverse)
library(sf)

## Step 2: Read data

#pull DEM data
dem <- rast("Products/2014_bcrast_dem.nc")

#pull extent data
ext <- vect("Products/2020_bcvect_extent.gpkg")

#pull raw data
logging <- rast("Data/SalvageLogging/CanLaBS_salvageMask_1985_2024_v20260121.tif")

## Step 3: Crop to terrestrial extent to ease memory usage

#reproject extent to CRS
ext <- ext %>% project(crs(logging))

#crop to extent:
logging <- logging %>% crop(ext)

## Step 4: Reproject data and mask to extent

#reproject to EPSG:3979
logging <- logging %>% project("EPSG:3979")

#reload extent without reprojection
ext <- vect("Products/2020_bcvect_extent.gpkg")

#set background values of raster to 0
logging[is.na(logging)] <- 0L

#mask to extent:
logging <- logging %>% mask(ext, touches = TRUE)

## Step 5: Save as NetCDF
writeCDF(logging, "Products/2026_bcrast_salvagelogging.nc", varname="salvagelog", longname="Salvage Logging", unit = "code")
