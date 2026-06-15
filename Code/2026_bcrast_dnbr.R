##dNBR Values (2026)

##Initialization: download data from https://open.canada.ca/data/en/dataset/2af751e7-79f9-4da8-9b45-14688818dca3

##Store unzipped data in /BCFireCube/Data/dNBR

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
dnbr <- rast("Data/dNBR/CanLaBS_1985_2024_v20260121.tif")

## Step 3: Crop to terrestrial extent to ease memory usage

#reproject extent to CRS
ext <- ext %>% project(crs(dnbr))

#crop to extent:
dnbr <- dnbr %>% crop(ext)

## Step 4: Reproject data and mask to extent

#reproject to EPSG:3979
dnbr <- dnbr %>% project("EPSG:3979")

#reload extent without reprojection
ext <- vect("Products/2020_bcvect_extent.gpkg")

#set background values of raster to 0
dnbr[is.na(dnbr)] <- -9999L

#mask to extent:
dnbr <- dnbr %>% mask(ext, touches = TRUE)

## Step 5: Save as NetCDF
writeCDF(dnbr, "Products/2026_bcrast_dnbr.nc", varname="dnbr", longname="Differenced Normalized Burn Ratio", unit = "value")
