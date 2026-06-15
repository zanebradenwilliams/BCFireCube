##Canadian Forest Fire Danger Rating System (CFFDRS) Fire Behaviour Prediction (FBP) Fuel Types 2024, 30 M

##Initialization: download data from https://open.canada.ca/data/en/dataset/4e66dd2f-5cd0-42fd-b82c-a430044b31de
##check documentation at https://cwfis.cfs.nrcan.gc.ca/downloads/fuels/development/Canadian_Forest_FBP_Fuel_Types/Canadian_Forest_FBP_Fuel_Types_Metadata_v20191114.pdf

##Store unzipped data in /BCFireCube/Data/Fueltypes

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
fuels <- rast("Data/Fueltypes/FBP_Canada_30m_3978_22052024_forRelease.tif")

## Step 3: Crop to terrestrial extent to ease memory usage

#reproject extent to CRS
ext <- ext %>% project(crs(fuels))

#crop to extent:
fuels <- fuels %>% crop(ext)

## Step 4: Create documentation

#create documentation dataframe
docs <- data.frame(unique(fuels)) %>% rename(fueltype = `Canadian.FBP.Fuel.Types..30m..May.22nd..2024`)

#add names from PDF documentation
docs$fuelname <- docs$fueltype %>% recode(`1` = "C1", `2` = "C2", `3` = "C3", `4` = "C4",
                                          `5` = "C5", `7` = "C7", `11` = "D1", `13` = "D1/2",
                                          `31` = "O1", `101` = "Non-fuel", `102` = "Water",
                                          `105` = "Vegetated Non-fuel", `415` = "M1 C15", `625` = "M1/2 C25",
                                          `650` = "M1/2 C50", `675` = "M1/2 C75")

docs$fuellongname <- docs$fueltype %>% recode(`1` = "C-1 Spruce-Lichen Woodland", `2` = "C-2 Boreal Spruce",
                                              `3` = "C-3 Mature Jack or Lodgepole Pine", `4` = "C-4 Immature Jack or Lodgepole Pine",
                                              `5` = "C-5 Red and White Pine", `7` = "C-7 Ponderosa Pine / Douglas Fir",
                                              `11` = "D-1 Leafless Aspen", `13` = "D-1/D-2 Aspen",
                                              `31` = "O-1a Matted Grass", `101` = "Non-fuel", `102` = "Water",
                                              `105` = "Vegetated Non-fuel", `415` = "M-1 Boreal Mixedwood - Leafless (15% Conifer)",
                                              `625` = "M-1/M-2 Boreal Mixedwood (25% Conifer)",
                                              `650` = "M-1/M-2 Boreal Mixedwood (50% Conifer)",
                                              `675` = "M-1/M-2 Boreal Mixedwood (75% Conifer)")

#save documentation as CSV
write.csv(docs, "Documentation/2024_bcrast_fueltypes.csv", row.names = FALSE)

## Step 5: Reproject data and mask to extent

#reproject to EPSG:3979
fuels <- fuels %>% project("EPSG:3979")

#reload extent without reprojection
ext <- vect("Products/2020_bcvect_extent.gpkg")

#mask to extent:
fuels <- fuels %>% mask(ext, touches = TRUE)

## Step 6: Save as NetCDF
writeCDF(fuels, "Products/2024_bcrast_fueltypes.nc", varname="fueltype", longname="Fuel Type Code", unit = "code")
