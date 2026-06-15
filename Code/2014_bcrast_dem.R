##Digital Elevation Model for British Columbia - CDED - 1:250,000

##Initialization: Download all map letterblocks from https://catalogue.data.gov.bc.ca/dataset/digital-elevation-model-for-british-columbia-cded-1-250-000

##Store unzipped data in /BCFireCube/Data/DEM

##Please have also completed running the following R scripts:
#2020_bcvect_extent.R

## Step 1: Call libraries

library(stars)
library(tidyverse)
library(terra)

## Step 2: Mosaic DEM letterblocks

#store data folder
wd <- "Data/DEM"

#list dem letterblocks
dem_names <- list.files(wd, full.names=TRUE)

#clean file names
dem_names_clean <- gsub("\\.dem$", "", basename(dem_names))

#create list of stars objects and rename attributes to elevation
dem_list <- setNames(map(dem_names, read_stars), dem_names_clean) %>%
  map(~{ names(.) <- "elevation"; . })

#mosaic together all objects
dem <- exec(st_mosaic, !!!dem_list)

#convert to raster
dem <- dem %>% as("Raster")

#convert to SpatRaster
dem <- rast(dem)

## Step 3: Reproject to correct CRS and aggregate to correct resolution

#reproject to EPSG:3979
dem <- dem %>% project("EPSG:3979")

#create template raster in EPSG:3979 with 30m resolution
temp <- rast(extent = ext(dem), crs = "EPSG:3979", res = 30)

#resample to template
dem <- dem %>% resample(temp, method = "bilinear")

## Step 4: Crop to terrestrial extent

#load extent data
ext <- vect("Products/2020_bcvect_extent.gpkg")

#mask and crop dem data to extent
dem <- mask(crop(dem, ext), ext, touches = TRUE)

## Step 5: Save as NetCDF
writeCDF(dem, "Products/2014_bcrast_dem.nc", varname = "elevation", longname = "Digital Elevation Model", unit = "m")
