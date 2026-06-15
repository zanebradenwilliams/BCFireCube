##Slope - Digital Elevation Model for British Columbia - CDED - 1:250,000

##Please have also completed running the following R scripts:
#2014_bcrast_dem.R

## Step 1: Call libraries

library(terra)

## Step 2: Load data

#load dem data
dem <- rast("Products/2014_bcrast_dem.nc")

## Step 3: Generate slope raster

#use slope() to generate output
slope <- dem %>% terrain(v = "slope", neighbors = 8)

## Step 4: Save as NetCDF
writeCDF(slope, "Products/2014_bcrast_slope.nc", varname="slope", longname="Slope - Digital Elevation Model", unit = "degrees")
