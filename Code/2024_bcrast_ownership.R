##Generalized Forest Cover Ownership

##Initialization: Download data from https://catalogue.data.gov.bc.ca/dataset/generalized-forest-cover-ownership

##Store unzipped data in /BCFireCube/Data/Ownership

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

#pull ownership data
ownership <- st_read("Data/Ownership/F_OWN/F_OWN_polygon.shp")

## Step 3: Create documentation

#create documentation dataframe
docs <- st_drop_geometry(ownership) %>% distinct(OWN, .keep_all = TRUE) %>% 
  select(OWN, OWN_DESC) %>% arrange(OWN)

#rename variables
docs <- docs %>% rename("ownership" = "OWN", "Ownership Description" = "OWN_DESC")

#save documentation as CSV
write.csv(docs, "Documentation/2024_bcrast_ownership.csv", row.names = FALSE)

## Step 4: Reproject and rasterize data

#convert to SpatVector
ownership <- ownership %>% vect()

#reproject to EPSG:3979
ownership <- ownership %>% project("EPSG:3979")

#rasterize to mirror DEM format
ownership <- ownership %>% rasterize(dem, field = "OWN", touches = TRUE)

## Step 5: Crop to terrestrial extent

#crop and mask to extent:
ownership <- mask(crop(ownership, ext), ext, touches = TRUE)

## Step 6: Save as NetCDF
writeCDF(ownership, "Products/2024_bcrast_ownership.nc", varname="ownership", longname="Ownership Code", unit = "code")
