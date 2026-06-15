##2015 Land Cover

##Initialization: download data from https://open.canada.ca/data/en/dataset/4e615eae-b90c-420b-adee-2ca35896caf6

##Store unzipped data in /BCFireCube/Data/2015LandCover

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

#pull land cover data
lco <- rast("Data/2015LandCover/landcover-2015-classification.tif")

## Step 3: Crop, reproject, and mask data

#reproject extent to CRS of raw data
ext <- ext %>% project(crs(lco))

#crop land cover to extent
lco <- lco %>% crop(ext)

#reproject to EPSG:3979
lco <- lco %>% project("EPSG:3979")

#reload extent
ext <- vect("Products/2025_bcvect_extent.gpkg")

#mask to extent:
lco <- lco %>% mask(ext, touches = TRUE)

## Step 4: Create documentation

#create df of observations
docs <- as.data.frame(unique(lco))

#label index data
docs <- docs %>% mutate(landcover = Canada2015, `Land Cover Class` = recode(
  Canada2015, `1` = "Temperate or sub-polar needleleaf forest", `2` = "Sub-polar taiga needleleaf forest",
  `5` = "Temperate or sub-polar broadleaf deciduous forest", `6` = "Mixed forest",
  `8` = "Temperate or sub-polar shrubland", `10` = "Temperate or sub-polar grassland",
  `11` = "Sub-polar or polar shrubland-lichen-moss", `12` = "Sub-polar or polar grassland-lichen-moss",
  `14` = "Wetland", `15` = "Cropland", `16` = "Barren land",
  `17` = "Urban and built-up", `18` = "Water", `19` = "Snow and ice"))

#select relevant columns
docs <- docs %>% select(c("landcover", "Land Cover Class"))

#save documentation as CSV
write.csv(docs, "Documentation/2015_bcrast_landcover.csv", row.names = FALSE)

## Step 6: Save as NetCDF
writeCDF(lco, "Products/2015_bcrast_landcover.nc", varname="landcover", longname="Land Cover Code", unit = "code")
