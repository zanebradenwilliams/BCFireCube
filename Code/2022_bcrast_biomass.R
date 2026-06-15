##Forest Total Biomass (2022)

##Initialization: download data from https://open.canada.ca/data/en/dataset/877566da-647c-4e92-9e4e-357698575d59

##Store unzipped data in /BCFireCube/Data/Biomass

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

#pull raw fuel types data
biom <- rast("Data/Biomass/CA_total_biomass_2022.tif")

## Step 3: Crop to terrestrial extent to ease memory usage

#reproject extent to CRS
ext <- ext %>% project(crs(biom))

#crop to extent:
biom <- biom %>% crop(ext)

## Step 4: Reproject data and mask to extent

#reproject to EPSG:3979
biom <- biom %>% project("EPSG:3979")

#reload extent without reprojection
ext <- vect("Products/2020_bcvect_extent.gpkg")

#mask to extent:
biom <- biom %>% mask(ext, touches = TRUE)

## Step 5: Convert units

#change from t/ha to t/km^2
biom_km <- 100*biom

## Step 6: Save as NetCDF
writeCDF(biom_km, "Products/2022_bcrast_biomass.nc", varname="biomass", longname="Forest Total Biomass", unit = "t/km^2")
