##Technical Validation
##Wildfire polygon and point data

## Step 1: Call libraries

library(terra)
library(sf)
library(tidyterra)

## Step 2: Load data

#load point data
point <- st_read("Products/2026_bcfire_firepoints.gpkg")

#load polygon data
poly <- st_read("Products/2026_bcfire_firepolygons.gpkg")

#load both data
dboth <- read.csv("Products/2026_bcfire_bothdata.csv")

## Step 3: Filter both points and polygons

#filter to fires that also have polygons
point <- point %>% filter(label %in% dboth$label)

#filter to fires that also have points
poly <- poly %>% filter(label %in% dboth$label)

## Step 4: Calculate centroids of fire polygons

#generate centroids
centroids <- st_centroid(poly)

## Step 5: Calculate distance between centroids and points

#arrange centroids by label
centroids <- centroids %>% arrange(label)

#arrange points by label
point <- point %>% arrange(label)

#arrange polygons by label
poly <- poly %>% arrange(label)

#distance extract
point$dist <- st_distance(point, centroids, by_element = TRUE)

## Step 6: Calculate polygon area

#generate area
poly$area <- st_area(poly)

#join data together
df <- st_drop_geometry(poly) %>% left_join(st_drop_geometry(point))

#calculate error metric
df <- df %>% mutate(error = dist/sqrt(area))

#calculate whether error is less than 1
df <- df %>% mutate(smallerror = as.numeric(error) <= 1)

#calculate whether error is less than 5
df <- df %>% mutate(mediumerror = as.numeric(error) <= 5)

#calculate whether error is less than 10
df <- df %>% mutate(largeerror = as.numeric(error) <= 10)

## Step 7: Check if points are inside the polygon

#generate list of distances
df$polydist <- st_distance(point, poly, by_element = TRUE)

#generate identifier for whether the point is inside the polygon
df <- df %>% mutate(within = as.numeric(polydist) == 0)

## Step 8: Organize results and export data

#organize variables
df <- df %>% select(c("firelabel", "label", "area", "dist", "polydist",
                      "error", "smallerror", "mediumerror", "largeerror", "within"))

#generate summary
summary(df)

#calculate small error percentage
meansmallerror <- mean(df$smallerror)

#calculate medium error percentage
meanmediumerror <- mean(df$mediumerror)

#calculate large error percentage
meanlargeerror <- mean(df$largeerror)

#calculate within percentage
meanwithin <- mean(df$within)

#save data
write.csv(df, "Data/Validation/validation_2026_bcfire.csv", row.names = FALSE)
