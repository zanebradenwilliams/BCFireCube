##Natural Disturbance Type Map

##Initialization: download data from https://catalogue.data.gov.bc.ca/dataset/natural-disturbance-type-map

##Store unzipped data in /BCFireCube/Data/NDT

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
ndt <- st_read("Data/NDT/BEC_NATURAL_DISTURBANCE_SV/BECNATURAL_polygon.shp")

## Step 3: Create documentation

#create combined zone-subzone code
ndt$ndtcode <- as.integer(substr(ndt$NTRL_DSTRD, 4, 4))

#create df of observations
docs <- st_drop_geometry(ndt) %>% distinct(ndtcode, .keep_all = TRUE) %>% 
  select(ndtcode, NTRL_DSTRD, NTRL_DSTRC) %>% arrange(ndtcode)

#save documentation as CSV
write.csv(docs, "Documentation/2021_bcrast_ndt.csv", row.names = FALSE)

## Step 4: Reproject and rasterize data
ndt <- ndt %>% select(ndtcode)

#convert to SpatVector
ndt <- ndt %>% vect()

#reproject to EPSG:3979
ndt <- ndt %>% project("EPSG:3979")

#rasterize to mirror dem format
ndt <- ndt %>% rasterize(dem, field = "ndtcode", touches = TRUE)

## Step 5: Crop to terrestrial extent

#crop and mask to extent:
ndt <- mask(crop(ndt, ext), ext, touches = TRUE)

## Step 6: Save as NetCDF
writeCDF(ndt, "Products/2021_bcrast_ndt.nc", varname="ndtcode", longname="Natural Disturbance Type Code", unit = "code")
