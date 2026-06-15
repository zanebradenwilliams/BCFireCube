##2022 Forest Age

##Initialization: download data from https://open.canada.ca/data/en/dataset/2dac1be0-411b-421e-bdf5-142a77afdbf0

##Store unzipped data in /BCFireCube/Data/ForestAge

##Please have also completed running the following R scripts:
#2020_bcvect_extent.R
#2014_bcrast_dem.R

## Step 1: Call libraries

library(terra)
library(tidyverse)
library(sf)
library(tidyterra)

## Step 2: Read data

#pull DEM data
dem <- rast("Products/2014_bcrast_dem.nc")

#pull extent data
ext <- vect("Products/2020_bcvect_extent.gpkg")

#pull forestage data
age <- rast("Data/ForestAge/CA_forest_age_2022.tif")

## Step 3: Crop and reproject data

#reproject extent to CRS of raw data
ext <- ext %>% project(crs(age))

#crop land cover to extent
age <- age %>% crop(ext)

#reproject to EPSG:3979
age <- age %>% project("EPSG:3979")

## Step 4: Change values from age to year

#subtract age from 2022
age_year <- 2022 - age

#set NA values to 0
age_year[is.na(age_year)] <- 0

## Step 5: Mask to extent

#reload extent
ext <- vect("Products/2025_bcvect_extent.gpkg")

#mask to extent:
age_year <- age_year %>% mask(ext, touches = TRUE)

## Step 6: Save as NetCDF
writeCDF(age_year, "Products/2022_bcrast_forestage.nc", varname="forestage", longname="Forest Age", unit = "year")
