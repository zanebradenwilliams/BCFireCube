##NBAC year (2026)

##Initialization: download data from https://open.canada.ca/data/en/dataset/2af751e7-79f9-4da8-9b45-14688818dca3

##Store unzipped data in /BCFireCube/Data/NBACYear

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
year <- rast("Data/NBACYear/NBAC_MRB_1972to2024_reproj.tif")

## Step 3: Crop to terrestrial extent to ease memory usage

#reproject extent to CRS
ext <- ext %>% project(crs(year))

#crop to extent:
year <- year %>% crop(ext)

## Step 4: Reproject data and mask to extent

#reproject to EPSG:3979
year <- year %>% project("EPSG:3979")

#reload extent without reprojection
ext <- vect("Products/2020_bcvect_extent.gpkg")

#set background values of raster to 0
year[is.na(year)] <- 0L

#mask to extent:
year <- year %>% mask(ext, touches = TRUE)

## Step 5: Save as NetCDF
writeCDF(year, "Products/2026_bcrast_nbacyear.nc", varname="burnyear", longname="NBAC Burn Year", unit = "year")
