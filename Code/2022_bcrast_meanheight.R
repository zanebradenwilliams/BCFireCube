##Mean Forest Height (2022)

##Initialization: download data from https://open.canada.ca/data/en/dataset/7e1b6612-a3b2-400c-8318-63d93ab2f1b7

##Store unzipped data in /BCFireCube/Data/MeanHeight

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
height <- rast("Data/MeanHeight/CA_elev_mean_2022.tif")

## Step 3: Crop to terrestrial extent to ease memory usage

#reproject extent to CRS
ext <- ext %>% project(crs(height))

#crop to extent:
height <- height %>% crop(ext)

## Step 4: Reproject data and mask to extent

#reproject to EPSG:3979
height <- height %>% project("EPSG:3979")

#reload extent without reprojection
ext <- vect("Products/2020_bcvect_extent.gpkg")

#mask to extent:
height <- height %>% mask(ext, touches = TRUE)

## Step 5: Save as NetCDF
writeCDF(height, "Products/2022_bcrast_meanheight.nc", varname="height", longname="Forest Mean Height", unit = "m")
