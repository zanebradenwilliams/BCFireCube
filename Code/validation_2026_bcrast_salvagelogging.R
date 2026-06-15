##Technical Validation
##Salvage Logging (2026)

## Step 1: Call libraries

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(viridis)
library(terra)
library(sf)
library(tidyterra)

## Step 2: Load data

#harmonized data
logging <- rast("Products/2026_bcrast_salvagelogging.nc")

#raw data
logging_raw <- rast("Data/SalvageLogging/CanLaBS_salvageMask_1985_2024_v20260121.tif")

#terrestrial extent data
ext <- st_read("Products/2020_bcvect_extent.gpkg")

## Step 3: Iterate simple random sampling of datasets

#create results dataframe
results <- data.frame(PCC = vector(mode = "numeric", length = 30))

#iterate sampling process 30 times
for(i in 1:30){
  #generate sample within extent
  sample <- ext %>% st_sample(size = 8100, type = "random") %>% vect()
  
  #annotate sampling points to rejoin data
  sample$ID = 1:nrow(sample)
  
  #extract values
  vals <- terra::extract(logging, sample)
  
  #reproject sample locations to crs of raw data
  sample_raw <- sample %>% project(crs(logging_raw))
  
  #extract raw values
  vals_raw <- terra::extract(logging_raw, sample_raw)
  
  #rename variables in raw extract
  vals_raw <- vals_raw %>% rename(salvagelog_raw = `CanLaBS_salvageMask_1985_2024_v20260121`) %>% 
    select(ID, salvagelog_raw)
  
  #set NA values to 0
  vals_raw$salvagelog_raw[is.na(vals_raw$salvagelog_raw)] <- 0
  
  #join dataframes
  vals <- vals %>% left_join(vals_raw)
  
  #calculate accuracy
  vals <- vals %>% mutate(correct = as.numeric(salvagelog == salvagelog_raw))
  
  #store PCC
  results$PCC[i] <- sum(vals$correct)/nrow(vals)
}

## Step 3: Process and analyze extracted data

#transform probabilities using logit
results$logitPCC <- log(results$PCC/(1-results$PCC))

#save data
write.csv(results, "Data/Validation/validation_2026_bcrast_salvagelogging.csv", row.names = FALSE)

#calculate confidence interval
logitmean <- mean(results$logitPCC)
logitsd <- sd(results$logitPCC)
logitci <- c(logitmean - 1.96*(logitsd/sqrt(30)), logitmean + 1.96*(logitsd/sqrt(30)))

#transform back to percentages
mean <- 1/(1+exp(-logitmean))
ci <- 1/(1+exp(-logitci))
