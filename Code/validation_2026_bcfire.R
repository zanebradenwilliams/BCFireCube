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

#generate identifier for whether the point is within 100m of the polygon
df <- df %>% mutate(within.100 = as.numeric(polydist) <= 100)

#generate identifier for whether the point is within 1000m of the polygon
df <- df %>% mutate(within.1000 = as.numeric(polydist) <= 1000)

## Step 8: Organize results and export data

#organize variables
df <- df %>% select(c("firelabel", "label", "area", "dist", "polydist",
                      "error", "smallerror", "mediumerror", "largeerror",
                      "within", "within.100", "within.1000"))

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

#calculate 100m within percentage
meanwithin.100 <- mean(df$within.100)

#calculate 1000m within percentage
meanwithin.1000 <- mean(df$within.1000)

#filter dataframe to fires larger than 1000ha
dlarge <- df %>% filter(area >= 10000000)

#calculate small error percentage
fmeansmallerror <- mean(dlarge$smallerror)

#calculate medium error percentage
fmeanmediumerror <- mean(dlarge$mediumerror)

#calculate large error percentage
fmeanlargeerror <- mean(dlarge$largeerror)

#calculate within percentage of filtered fires
fmeanwithin <- mean(dlarge$within)

#calculate 100m within percentage
fmeanwithin.100 <- mean(dlarge$within.100)

#calculate 1000m within percentage
fmeanwithin.1000 <- mean(dlarge$within.1000)

#save data
write.csv(df, "Data/Validation/validation_2026_bcfire.csv", row.names = FALSE)
