##BC Wildfire PSTA Head Fire Intensity 

##Initialization: Download data from https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-psta-head-fire-intensity

##Store unzipped data in /BCFireCube/Data/PSTAHead

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

#pull raw PSTA Head Fire Intensity data
pstahead <- st_read("Data/PSTAHead/PROT_PSTA_HEAD_FIRE_INTNSTY_SP/PROTPSTAHE_polygon.shp")

## Step 3: Create documentation

#create documentation dataframe
docs <- st_drop_geometry(pstahead) %>% distinct(HFI_CLASS, .keep_all = TRUE) %>% 
  select(HFI_CLASS, HFI_CL_RNG) %>% arrange(HFI_CLASS)

#rename variables
docs <- docs %>% rename("hf_intensity" = "HFI_CLASS", "Head Fire Intensity Class Range" = "HFI_CL_RNG")

#save documentation as CSV
write.csv(docs, "Documentation/2021_bcrast_pstahead.csv", row.names = FALSE)

## Step 4: Simplify data and rewrite as a gpkg for memory efficiency

#select only the variable we need
pstahead <- pstahead %>% select(HFI_CLASS)

#write as a gpkg
st_write(pstahead, "Data/PSTAHead/pstahead.gpkg")

#remove old version from environment and free memory
rm(pstahead)
gc()

## Step 5: Reproject and rasterize data

#load gpkg as SpatVector
pstahead <- vect("Data/PSTAHead/pstahead.gpkg")

#reproject to EPSG:3979
pstahead <- pstahead %>% project("EPSG:3979")

#rasterize to mirror dem format
pstahead <- pstahead %>% rasterize(dem, field = "HFI_CLASS", touches = TRUE)

## Step 6: Crop to terrestrial extent

#crop and mask to extent:
pstahead <- mask(crop(pstahead, ext), ext, touches = TRUE)

## Step 6: Save as NetCDF
writeCDF(pstahead, "Products/2021_bcrast_pstahead.nc", varname="hf_intensity", longname="Head Fire Intensity", unit = "code")
