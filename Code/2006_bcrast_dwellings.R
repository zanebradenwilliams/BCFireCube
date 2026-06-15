##2006 Dwellings Raster

##Initialization: download data from https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
##and https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/attribute-attribs/index2021-eng.cfm

##Store unzipped data in /BCFireCube/Data/2021Census

##Please have also completed running the following R scripts:
#2020_bcvect_extent.R
#2014_bcrast_dem.R
#2006_bcrast_population.R

## Step 1: Call libraries

library(terra)
library(tidyverse)
library(sf)

## Step 2: Read data

#pull DEM data
dem <- rast("Products/2014_bcrast_dem.nc")

#pull extent data
ext <- vect("Products/2020_bcvect_extent.gpkg")

#pull census boundaries data
bnd <- st_read("Data/2006Census/gdb_000b06a_e.shp")

#pull census dwelling data
dwl <- read.csv("Data/2006Census/2006Census.csv")

## Step 3: Filter to BC observations

#the first two characters of the DBUID are the province code, BC = 59
#filter bnd to BC observations
bnd <- bnd %>% filter(substr(DBUID, 1, 2)=="59")

#convert DBUID to character
dwl$DBUID <- as.character(dwl$DBUID)

#left join dwl data to bnd
bnd <- bnd %>% left_join(dwl, by = "DBUID")

#filter out observations with LANDAREA == 0
bnd <- bnd %>% filter(!(LANDAREA == 0))

#calculate average dwelling density per km^2
bnd <- bnd %>% mutate(dwellings = rawdwl/LANDAREA)

#select relevant variable
bnd <- bnd %>% select(dwellings)

## Step 4: Reproject and rasterize data

#convert to SpatVector
bnd <- bnd %>% vect()

#reproject to EPSG:3979
bnd <- bnd %>% project("EPSG:3979")

#rasterize to dem format
bnd <- bnd %>% rasterize(dem, field = "dwellings", touches = TRUE)

## Step 5: Crop to terrestrial extent

#crop and mask to extent:
bnd <- mask(crop(bnd, ext), ext, touches = TRUE)

## Step 6: Save as NetCDF
writeCDF(bnd, "Products/2006_bcrast_dwellings.nc", varname="dwellings", longname="Dwellings per Square Kilometer", unit = "dwellings/km^2")
