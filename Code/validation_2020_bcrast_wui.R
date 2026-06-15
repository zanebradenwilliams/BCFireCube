##Technical Validation
##BC Wildfire WUI Human Interface Buffer

## Step 1: Call libraries

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(viridis)
library(terra)
library(sf)
library(tidyterra)

## Step 2: Load data

#harmonized pstawui data
pstawui <- rast("Products/2020_bcrast_wui.nc")

#raw pstawui data
pstawui_raw <- vect("Data/WUI/PROT_WUI_HMN_INTRFCE_BUFFR_SP/PROTWUIHMN_polygon.shp")

#terrestrial extent data
ext <- st_read("Products/2025_bcvect_extent.gpkg")

## Step 3: Iterate simple random sampling of datasets

#create results dataframe
results <- data.frame(PCC = vector(mode = "numeric", length = 30))

#iterate sampling process 30 times
for(i in 1:30){
  #generate sample within extent of dem
  sample <- ext %>% st_sample(size = 8100, type = "random") %>% vect()
  
  #annotate sampling points to rejoin data
  sample$ID = 1:nrow(sample)
  
  #extract dem values
  vals <- terra::extract(pstawui, sample)
  
  #reproject sample locations to crs of raw data
  sample_raw <- sample %>% project(crs(pstawui_raw))
  
  #extract raw dem values
  vals_raw <- as.data.frame(terra::intersect(sample_raw, pstawui_raw))
  
  #add new binary variable for wui coverage
  vals_raw <- vals_raw %>% mutate(wui_raw = 1) %>% select(ID, wui_raw)
  
  #join dataframes
  vals <- vals %>% left_join(vals_raw)
  
  #recode NA values to 0 for PCC calculation
  vals[is.na(vals)] <- 0
  
  #calculate accuracy
  vals <- vals %>% mutate(correct = as.numeric(wui == wui_raw))
  
  #store PCC
  results$PCC[i] <- sum(vals$correct)/nrow(vals)
}

## Step 3: Process and analyze extracted data

#transform probabilities using logit
results$logitPCC <- log(results$PCC/(1-results$PCC))

#save data
write.csv(results, "Data/Validation/validation_2020_bcrast_wui.csv", row.names = FALSE)

#calculate confidence interval
logitmean <- mean(results$logitPCC)
logitsd <- sd(results$logitPCC)
logitci <- c(logitmean - 1.96*(logitsd/sqrt(30)), logitmean + 1.96*(logitsd/sqrt(30)))

#transform back to percentages
mean <- 1/(1+exp(-logitmean))
ci <- 1/(1+exp(-logitci))
