##BEC Map

##Initialization: download data from https://catalogue.data.gov.bc.ca/dataset/bec-map

##Store unzipped data in /BCFireCube/Data/BEC

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
bec <- st_read("Data/BEC/BEC_BIOGEOCLIMATIC_POLY/BEC_POLY_polygon.shp")

## Step 3: Create documentation

#create combined zone-subzone code
bec$Z_SZ <- paste0(bec$ZONE, "-", bec$SUBZONE)

#create df of observations
docs <- st_drop_geometry(bec) %>% distinct(Z_SZ, .keep_all = TRUE) %>% select(Z_SZ, ZONE, SUBZONE, ZONE_NAME, SBZNNM)

#group and index data
docs <- docs %>% mutate(beccode = recode(
  ZONE, PP = 10L, CMA = 20L, CDF = 30L, IMA = 40L, BG = 50L, SBPS = 60L, SWB = 70L, MH = 80L,
  BWBS = 90L, MS = 100L, CWH = 110L, SBS = 120L, IDF = 140L, ICH = 160L, ESSF = 180L, BAFA = 230L))

#add counter by zone
docs <- docs %>% mutate(i = row_number()-1, .by = ZONE)

#add code and index
docs <- docs %>% mutate(beccode = beccode + i) %>% select(beccode, Z_SZ, ZONE, SUBZONE, ZONE_NAME, SBZNNM) %>% 
  arrange(beccode)

#save documentation as CSV
write.csv(docs, "Documentation/2021_bcrast_bec.csv", row.names = FALSE)

#add beccode to spatial data
bec <- bec %>% left_join(docs, by = "Z_SZ")

## Step 4: Reproject and rasterize data

#select only relevant observations
bec <- bec %>% select(beccode)

#convert to SpatVector
bec <- bec %>% vect()

#reproject to EPSG:3979
bec <- bec %>% project("EPSG:3979")

#rasterize to mirror dem format
bec <- bec %>% rasterize(dem, field = "beccode", touches = TRUE)

## Step 5: Crop to terrestrial extent

#crop and mask to extent:
bec <- mask(crop(bec, ext), ext, touches = TRUE)

## Step 6: Save as NetCDF
writeCDF(bec, "Products/2021_bcrast_bec.nc", varname="beccode", longname="BEC Zone and Subzone Code", unit = "code")
