##BC Wildfire PSTA Lightning Fire Density 

##Initialization: Download data from https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-psta-lightning-fire-density

##Store unzipped data in /BCFireCube/Data/PSTALightning

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

#pull raw PSTA Lightning Fire Density data
pstalightning <- st_read("Data/PSTALightning/PROT_PSTA_LTG_FIRE_ST_DNSTY_SP/PROTPSTALT_polygon.shp")

## Step 3: Recode data for increased precision

#recode data
pstalightning <- pstalightning %>% mutate(lightning_density = recode(
  FR_STRT_DY, `Water` = -1L, `No Fires` = 0L, `1 - 5` = 1L, `5.1 – 10` = 2L, `10.1 – 17` = 3L,
  `17.1 – 24` = 4L, `24.1 – 33` = 5L, `33.1 – 45` = 6L, `45.1 – 60` = 7L, `60.1 – 82` = 8L,
  `82.1 – 116` = 9L, `>116` = 10L))

#create documentation dataframe
docs <- st_drop_geometry(pstalightning) %>% distinct(lightning_density, .keep_all = TRUE) %>% 
  select(lightning_density, FR_STRT_DY) %>% arrange(lightning_density)

#save documentation as CSV
write.csv(docs, "Documentation/2021_bcrast_pstalightning.csv", row.names = FALSE)

## Step 4: Reproject and rasterize data

#convert to SpatVector
pstalightning <- pstalightning %>% vect()

#reproject to EPSG:3979
pstalightning <- pstalightning %>% project("EPSG:3979")

#rasterize to mirror dem format
pstalightning <- pstalightning %>% rasterize(dem, field = "lightning_density", touches = TRUE)

## Step 5: Crop to terrestrial extent

#crop and mask to extent:
pstalightning <- mask(crop(pstalightning, ext), ext, touches = TRUE)

## Step 6: Save as NetCDF
writeCDF(pstalightning, "Products/2021_bcrast_pstalightning.nc", varname="lightning_density", longname="Lightning Fire Start Density", unit = "code")
