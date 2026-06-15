##BC Wildfire PSTA Human Fire Density 

##Initialization: Download data from https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-psta-human-fire-density

##Store unzipped data in /BCFireCube/Data/PSTAHuman

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

#pull raw PSTA Human Fire Density data
pstahuman <- st_read("Data/PSTAHuman/PROT_PSTA_HMN_FIRE_ST_DNSTY_SP/PROTPSTAHM_polygon.shp")

## Step 3: Recode data for increased precision

#recode data
pstahuman <- pstahuman %>% mutate(human_density = recode(
  FR_STRT_DY, `Water` = -1L, `No Fires` = 0L, `1 - 5` = 1L, `5.1 – 10` = 2L, `10.1 – 17` = 3L,
  `17.1 – 24` = 4L, `24.1 – 33` = 5L, `33.1 – 45` = 6L, `45.1 – 60` = 7L, `60.1 – 82` = 8L,
  `82.1 – 116` = 9L, `>116` = 10L))

#create documentation dataframe
docs <- st_drop_geometry(pstahuman) %>% distinct(human_density, .keep_all = TRUE) %>% 
  select(human_density, FR_STRT_DY) %>% arrange(human_density)

#save documentation as CSV
write.csv(docs, "Documentation/2021_bcrast_pstahuman.csv", row.names = FALSE)

## Step 4: Reproject and rasterize data

#convert to SpatVector
pstahuman <- pstahuman %>% vect()

#reproject to EPSG:3979
pstahuman <- pstahuman %>% project("EPSG:3979")

#rasterize to mirror dem format
pstahuman <- pstahuman %>% rasterize(dem, field = "human_density", touches = TRUE)

## Step 5: Crop to terrestrial extent

#crop and mask to extent:
pstahuman <- mask(crop(pstahuman, ext), ext, touches = TRUE)

## Step 6: Save as NetCDF
writeCDF(pstahuman, "Products/2021_bcrast_pstahuman.nc", varname="human_density", longname="Human Fire Start Density", unit = "code")
