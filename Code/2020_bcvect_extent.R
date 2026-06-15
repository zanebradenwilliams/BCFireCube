##BC Terrestrial Extent

##Initialization: Download Province of British Columbia - Boundary Terrestrial from https://catalogue.data.gov.bc.ca/dataset/province-of-british-columbia-boundary-terrestrial
##Note: A cURL download is needed

##Store data in /BCFireCube/Data/BC_Boundary_Terrestrial.gpkg

## Step 1: Call libraries

library(tidyverse)
library(terra)
library(sf)
library(lwgeom)

## Step 2: Join and polygonize boundaries

#read gpkg
ext <- st_read("Data/BC_Boundary_Terrestrial.gpkg", layer = "BC_Boundary_Terrestrial_Line", quiet = TRUE)

#write to polygon from lines
ext <- ext %>% st_cast("MULTILINESTRING", warn = FALSE) %>% 
  st_union() %>% 
  st_polygonize() %>% 
  st_collection_extract("POLYGON") %>% 
  st_make_valid()

## Step 3: Reproject to harmonized CRS

#convert to terra object
ext <- vect(ext)

#project to EPSG:3979
ext <- ext %>% project("EPSG:3979")

## Step 4: Save as gpkg
writeVector(ext, "Products/2020_bcvect_extent.gpkg")
