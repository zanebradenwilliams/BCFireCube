##2011 Population Raster

##Initialization: download data from https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm
##and https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/att-eng.cfm?year=2011

##Store unzipped data in /BCFireCube/Data/2011Census

##Please have also completed running the following R scripts:
#2020_bcvect_extent.R
#2014_bcrast_dem.R

## Step 1: Call libraries

library(terra)
library(tidyverse)
library(sf)
library(readxl)

## Step 2: Read data

#pull DEM data
dem <- rast("Products/2014_bcrast_dem.nc")

#pull extent data
ext <- vect("Products/2020_bcvect_extent.gpkg")

#pull census boundaries data
bnd <- st_read("Data/2011Census/gdb_000b11a_e.shp")

#pull census population data
pop <- read_excel("Data/2011Census/2011_92-151_XBB_XLSX.xlsx", col_names = FALSE)

## Step 3: Filter to BC observations

#the first two characters of the DBUID are the province code, BC = 59
#filter bnd to BC observations
bnd <- bnd %>% filter(substr(DBUID, 1, 2)=="59")

#select relevant variables from pop
pop <- pop %>% mutate(DBUID = as.character(`...1`), rawpop = as.numeric(`...2`),
                      rawdwl = as.numeric(`...3`),LANDAREA = as.numeric(`...5`)) %>% 
  select(c("DBUID", "rawpop", "rawdwl", "LANDAREA"))

#write this to csv for future use
write.csv(pop, "Data/2011Census/2011Census.csv", row.names = FALSE)

#left join pop data to bnd
bnd <- bnd %>% left_join(pop, by = "DBUID")

#filter out observations with LANDAREA == 0
bnd <- bnd %>% filter(!(LANDAREA == 0))

#calculate average population density per km^2
bnd <- bnd %>% mutate(population = rawpop/LANDAREA)

#select relevant variable
bnd <- bnd %>% select(population)

## Step 4: Reproject and rasterize data

#convert to SpatVector
bnd <- bnd %>% vect()

#reproject to EPSG:3979
bnd <- bnd %>% project("EPSG:3979")

#rasterize to dem format
bnd <- bnd %>% rasterize(dem, field = "population", touches = TRUE)

## Step 5: Crop to terrestrial extent

#crop and mask to extent:
bnd <- mask(crop(bnd, ext), ext, touches = TRUE)

## Step 6: Save as NetCDF
writeCDF(bnd, "Products/2011_bcrast_population.nc", varname="population", longname="Population per Square Kilometer", unit = "population/km^2")
