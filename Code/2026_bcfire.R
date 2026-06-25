##Wildfire Data Processing

##Initialization: Download BC Wildfire Perimeters -- Historical from https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-historical
##and BC Wildfire Fire Incident Locations -- Historical from https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-incident-locations-historical

##Unzip data and store in /BCFireCube/Data/HistoricalFirePoints and /BCFireCube/Data/HistoricalFirePolygons

##Please have also completed running the following R scripts:
#2020_bcvect_extent.R

## Step 1: Call libraries

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(exactextractr)

## Step 2: Call spatial data, filter, and address data duplicates

#read polygon shapefile
d <- st_read("Data/HistoricalFirePolygons/PROT_HISTORICAL_FIRE_POLYS_SP/H_FIRE_PLY_polygon.shp")

#filter out fires without a listed date
d <- d %>% filter(!is.na(FIRE_DATE))

#654 observations uploaded on 2007-05-17 have incorrect fire dates listed as 2007-05-16:
d <- d %>% filter(!(LOAD_DATE == "20070517" & FIRE_DATE == "20070516"))

#duplicate observation of fire 1958-R00207
#NOTE: THIS OBJECTID REQUIRES AN UPDATE WHENEVER SOURCE DATASET IS UPDATED
d <- d %>% filter(!(OBJECTID == "3297362"))

#duplicate observation of fire 2006-G70278
#NOTE: THIS OBJECTID REQUIRES AN UPDATE WHENEVER SOURCE DATASET IS UPDATED
d <- d %>% filter(!(OBJECTID == "3303869"))

#view fires with identical fire labels
dups <- d %>% add_count(FIRELABEL) %>% filter(n > 1) %>%
  distinct() %>% arrange(FIRELABEL)

#add index to fires with multiple observations
dups <- dups %>% group_by(FIRELABEL) %>% mutate(index = row_number()) %>% ungroup()

#drop dups geometry and simplify for join
dups <- dups %>% st_drop_geometry() %>% select(c("FIRELABEL", "AREA_SQM", "index"))

#left join index to dataframe
d <- d %>% left_join(dups, by = c("FIRELABEL", "AREA_SQM"))

#add date identifier
d$date <- as_date(paste0(substr(d$FIRE_DATE, 1, 4), "-", substr(d$FIRE_DATE, 5, 6), "-", substr(d$FIRE_DATE, 7, 8)))

#add new label identifier
d <- d %>% mutate(label = paste0(FIRE_YEAR, "-", FIRE_NO,"-",index))

#remove NA terms from label where present
d$label <- gsub("-NA", "", d$label)

#collect observations where the FIRELABEL contains "-NA"
dNA <- d %>% filter(str_detect(FIRELABEL, "NA"))

#add -NA back to those observations
d$label[d$FIRELABEL %in% dNA$FIRELABEL] <- paste0(substr(dNA$label, 1, 4), "-NA", paste0(substr(dNA$label, 5, 10)))

#select relevant variables
d <- d %>% select(c("FIRELABEL", "label", "date", "FIRE_CAUSE", "AREA_SQM", "SOURCE", "METHOD"))

#rename variables
d <- d %>% mutate("firelabel"=FIRELABEL, "cause" = FIRE_CAUSE,
                  "area_sqm" = AREA_SQM, "datasource" = SOURCE,
                  "creationnotes" = METHOD) %>% 
  select(c("firelabel", "label", "date", "cause", "area_sqm", "datasource", "creationnotes"))

## Step 3: Save intermediate step as CSV

#drop geometry
d_nogeo <- d %>% st_drop_geometry()

#write csv to products folder
write.csv(d_nogeo, "Products/2026_bcfire_polygondata.csv", row.names = FALSE)

## Step 4: Save polygons as GPKG

#select identifying variables
d_poly <- d %>% select("firelabel", "label")

#convert to spatVector
d_poly <- vect(d_poly)

#reproject polygons to EPSG:3979
d_poly <- d_poly %>% project("EPSG:3979")

#write gpkg
writeVector(d_poly, "Products/2026_bcfire_firepolygons.gpkg")

## Step 5: Repeat data processing for wildfire incident locations

#read historical incident data
d2 <- st_read("Data/HistoricalFirePoints/PROT_HISTORICAL_INCIDENTS_SP/H_FIRE_PNT_point.shp")

#filter out observations missing both ignition and fire out dates
d2 <- d2 %>% filter(!(is.na(IGN_DATE) & is.na(FR_T_DTE)))

#view fires with ignition date data and prepare for left join
idate <- d2 %>% filter(!is.na(IGN_DATE)) %>%
  mutate(igndate = as_date(paste0(substr(IGN_DATE, 1, 4), "-", 
                                  substr(IGN_DATE, 5, 6), "-",
                                  substr(IGN_DATE, 7, 8)))) %>% 
  st_drop_geometry() %>% select(c("FIRELABEL", "IGN_DATE",
                                  "LATITUDE", "LONGITUDE", "igndate"))

#left join igndate to dataframe
d2 <- d2 %>% left_join(idate, by = c("FIRELABEL", "IGN_DATE", "LATITUDE", "LONGITUDE"))

#view fires with fire out date data and prepare for left join
fodate <- d2 %>% filter(!is.na(FR_T_DTE)) %>%
  mutate(outdate = as_date(paste0(substr(FR_T_DTE, 1, 4), "-", 
                                  substr(FR_T_DTE, 5, 6), "-",
                                  substr(FR_T_DTE, 7, 8)))) %>% 
  st_drop_geometry() %>% select(c("FIRELABEL", "FR_T_DTE",
                                  "LATITUDE", "LONGITUDE", "outdate"))

#left join igndate to dataframe
d2 <- d2 %>% left_join(fodate, by = c("FIRELABEL", "FR_T_DTE", "LATITUDE", "LONGITUDE"))

#NOTE: THESE ROW NUMBERS REQUIRE AN UPDATE WHENEVER SOURCE DATASET IS UPDATED
#Correct typo in igndate for fire 2023-R11236
d2$igndate[6190] <- as_date("2023-07-07")

#Correct typo in outdate for fire 2020-K50952
d2$outdate[6233] <- as_date("2020-08-05")

#Correct typo in outdate for fire 2016-N70347
d2$outdate[13340] <- as_date("2016-08-27")

#Correct typo in igndate for fire 2022-N71401
d2$igndate[13949] <- as_date("2022-08-13")

#Correct typo in igndate for fire 2021-N20991
d2$igndate[14082] <- as_date("2021-06-28")

#Correct typo in igndate for fire 2004-G40532
d2$igndate[17317] <- as_date("2004-10-06")

#Correct typo in outdate for fire 2011-N60045
d2$outdate[17639] <- as_date("2011-07-02")

#Correct typo in igndate for fire 2003-N11055
d2$igndate[18519] <- as_date("2003-01-05")

#Correct typo in igndate for fire 2004-N50192
d2$igndate[20809] <- as_date("2004-07-19")

#Correct typo in outdate for fire 2004-N20231
d2$outdate[21456] <- as_date("2004-07-29")

#Correct typo in outdate for fire 2011-N10307
d2$outdate[24601] <- as_date("2011-08-15")

#Correct typo in igndate for fire 2006-C40299
d2$igndate[24777] <- as_date("2006-07-23")

#Correct typo in outdate for fire 2007-N10073
d2$outdate[25080] <- as_date("2007-07-07")

#Correct typo in igndate for fire 2008-K30232
d2$igndate[26572] <- as_date("2008-05-30")

#Correct typo in outdate for fire 2003-N10739
d2$outdate[33610] <- as_date("2003-08-19")

#Correct typo in igndate for fire 2009-V60035
d2$igndate[36459] <- as_date("2009-04-24")

#Correct typo in igndate for fire 2009-K20139
d2$igndate[37824] <- as_date("2009-05-15")

#Correct typo in outdate for fire 2012-K61009
d2$outdate[39841] <- as_date("2012-03-01")

#Correct typo in outdate for fire 2016-K40167
d2$outdate[44849] <- as_date("2016-07-02")

#Correct typo in igndate for fire 2007-V10026
d2$igndate[45156] <- as_date("2007-05-11")

#Correct typo in outdate for fire 2009-N20727
d2$outdate[46384] <- as_date("2009-08-23")

#Correct typo in outdate for fire 2009-N10101
d2$outdate[48433] <- as_date("2009-06-13")

#Correct typo in outdate for fire 2016-C10021
d2$outdate[55357] <- as_date("2016-04-09")

#Correct typo in igndate for fire 2012-N50510
d2$igndate[62126] <- as_date("2012-09-03")

#Correct typo in igndate for fire 2006-C30093
d2$igndate[67343] <- as_date("2006-05-23")

#Correct typo in outdate for fire 1998-G10275
d2$outdate[175847] <- as_date("1998-07-27")

#Correct typo in outdate for fire 1998-G50259
d2$outdate[176027] <- as_date("1998-07-22")

#Correct typo in outdate for fire 1998-G10298
d2$outdate[176662] <- as_date("1998-07-30")

#Correct typo in igndate for fire 1998-G70383
d2$igndate[177035] <- as_date("1998-08-03")

#Correct typo in outdate for fire 1998-G80290
d2$outdate[177673] <- as_date("1998-07-29")

#Correct typo in outdate for fire 1998-G10262
d2$outdate[178422] <- as_date("1998-07-24")

#Correct typo in outdate for fire 1998-G90222
d2$outdate[178621] <- as_date("1998-07-11")

#Correct typo in outdate for fire 1998-G10273
d2$outdate[178629] <- as_date("1998-07-27")

#Correct typo in outdate for fire 1998-G40278
d2$outdate[178793] <- as_date("1998-07-28")

#Correct typo in igndate for fire 1999-V10304
d2$igndate[183217] <- as_date("1999-08-06")

#Correct typo in outdate for fire 1999-V80013
d2$outdate[183276] <- as_date("1999-04-24")

#Correct typo in outdate for fire 1999-C50096
d2$outdate[184159] <- as_date("1999-06-06")

#Correct typo in igndate for fire 1999-V10127
d2$igndate[184248] <- as_date("1999-07-28")

#swap igndate and outdate for remaining 100 observations with an igndate later than outdate
d2 <- d2 %>% mutate(swapign = if_else(igndate > outdate, outdate, igndate),
                        swapout = if_else(igndate > outdate, igndate, outdate),
                        igndate = swapign,
                        outdate = swapout) %>% 
  select(-c("swapign", "swapout"))

#add burn length in days
d2 <- d2 %>% mutate(burnlength = as.numeric(outdate - igndate))

#remove fire observations with burn length >5 years
d2 <- d2 %>% filter(burnlength <= 1826 | is.na(burnlength))

#filter out incidents with FIRE_TYPE == "Field Activity" (772 observations)
d2 <- d2 %>% filter(!(FIRE_TYPE == "Field Activity"))

#view fires with identical fire labels
dups2 <- d2 %>% add_count(FIRELABEL) %>% filter(n > 1) %>%
  distinct() %>% arrange(FIRELABEL)

#add index to fires with multiple observations
dups2 <- dups2 %>% group_by(FIRELABEL) %>% mutate(index = row_number()) %>% ungroup()

#drop dups2 geometry and simplify for join
dups2 <- dups2 %>% st_drop_geometry() %>% select(c("FIRELABEL", "FR_T_DTE",
                                                   "LATITUDE", "LONGITUDE", "index"))

#left join index to dataframe
d2 <- d2 %>% left_join(dups2, by = c("FIRELABEL", "FR_T_DTE", "LATITUDE", "LONGITUDE"))

#add new label identifier from ignition date
d2 <- d2 %>% mutate(label = paste0(FIRE_YEAR,"-", FIRE_NO,"-",index))

#manually adjust label for 3 observations with shared FIRE_NO
#for FIRELABEL == "2021-71W22AA":
d2$label[72222] <- "2021-71W22A-1"

#for FIRELABEL == "2021-71W22AC":
d2$label[43850] <- "2021-71W22A-2"

#for FIRELABEL == "2021-71W22AE":
d2$label[41457] <- "2021-71W22A-3"

#remove NA terms from label where present
d2$label <- gsub("-NA", "", d2$label)

#collect observations where the FIRELABEL contains "NA"
d2NA <- d2 %>% filter(str_detect(FIRELABEL, "NA"))

#add -NA back to those observations
d2$label[d2$FIRELABEL %in% d2NA$FIRELABEL] <- paste0(substr(d2NA$label, 1, 4), "-NA", paste0(substr(d2NA$label, 5, 10)))

#select fires with FIRE_YEAR >= 1989 (97275 observations)
d2_recent <- d2 %>% filter(FIRE_YEAR >= 1989)

#convert fire centre digits into codes
d2_recent <- d2_recent %>% mutate(fire_centre = recode(FRCNTR, `2` = "Coastal Fire Centre",
                                              `3` = "Northwest Fire Centre", `4` = "Prince George Fire Centre",
                                              `5` = "Kamloops Fire Centre", `6` = "Southeast Fire Centre",
                                              `7` = "Cariboo Fire Centre"))

#convert fire zone digits by fire centre
d2_coastal <- d2_recent %>% filter(fire_centre == "Coastal Fire Centre") %>%
  mutate(fire_zone = recode(ZONE, `1` = "Fraser Fire Zone", `3` = "Pemberton Fire Zone",
                            `5` = "Sunshine Coast Fire Zone", `6` = "South Island Fire Zone",
                            `7` = "Mid Island Fire Zone", `8` = "North Island Mid Coast Fire Zone",
                            `9` = "North Island Mid Coast Fire Zone", `10` = "North Island Mid Coast Fire Zone",
                            `11` = "Fraser Fire Zone")) %>% 
  select(c("OBJECTID", "fire_centre", "fire_zone"))

d2_northwest <- d2_recent %>% filter(fire_centre == "Northwest Fire Centre") %>% 
  mutate(fire_zone = recode(ZONE, `1` = "Nadina Fire Zone", `2` = "Nadina Fire Zone",
                            `3` = "Bulkley Fire Zone", `4` = "Bulkley Fire Zone",
                            `5` = "Skeena Fire Zone", `8` = "Skeena Fire Zone",
                            `9` = "Cassiar Fire Zone")) %>% 
  select(c("OBJECTID", "fire_centre", "fire_zone"))

d2_prince <- d2_recent %>% filter(fire_centre == "Prince George Fire Centre") %>% 
  mutate(fire_zone = recode(ZONE, `1` = "Prince George Fire Zone", `3` = "Robson Valley Fire Zone",
                            `4` = "VanJam Fire Zone", `5` = "VanJam Fire Zone",
                            `6` = "Mackenzie Fire Zone", `7` = "Dawson Creek Fire Zone",
                            `8` = "Fort St. John Fire Zone", `9` = "Fort Nelson Fire Zone")) %>% 
  select(c("OBJECTID", "fire_centre", "fire_zone"))

d2_kamloops <- d2_recent %>% filter(fire_centre == "Kamloops Fire Centre") %>% 
  mutate(fire_zone = recode(ZONE, `0` = "Kamloops Fire Zone", `1` = "Kamloops Fire Zone",
                            `2` = "Kamloops Fire Zone", `3` = "Vernon Fire Zone",
                            `4` = "Vernon Fire Zone", `5` = "Penticton Fire Zone",
                            `6` = "Merritt Fire Zone", `7` = "Lillooet Fire Zone")) %>% 
  select(c("OBJECTID", "fire_centre", "fire_zone"))

d2_southeast <- d2_recent %>% filter(fire_centre == "Southeast Fire Centre") %>% 
  mutate(fire_zone = recode(ZONE, `1` = "Cranbrook Fire Zone", `2` = "Invermere Fire Zone",
                            `3` = "Columbia Fire Zone", `4` = "Columbia Fire Zone",
                            `5` = "Arrow Fire Zone", `6` = "Boundary Fire Zone",
                            `7` = "Kootenay Lake Fire Zone")) %>% 
  select(c("OBJECTID", "fire_centre", "fire_zone"))

d2_cariboo <- d2_recent %>% filter(fire_centre == "Cariboo Fire Centre") %>% 
  mutate(fire_zone = recode(ZONE, `1` = "Quesnel Fire Zone", `2` = "Central Cariboo Fire Zone",
                            `3` = "Central Cariboo Fire Zone", `4` = "100 Mile House Fire Zone",
                            `5` = "Chilcotin Fire Zone")) %>% 
  select(c("OBJECTID", "fire_centre", "fire_zone"))

#rejoin values
d2_recent <- rbind(d2_coastal, d2_northwest, d2_prince, d2_kamloops, d2_southeast, d2_cariboo) %>% 
  st_drop_geometry

#rejoin fire centre and zone values to main dataframe
d2 <- d2 %>% left_join(d2_recent, by = "OBJECTID")

#select relevant variables
d2 <- d2 %>% select(c("FIRELABEL", "label", "igndate", "outdate", "burnlength",
                        "FIRE_CAUSE", "fire_centre", "fire_zone", "FIRE_TYPE", "RSPNS_TYPC"))

#rename variables
d2 <- d2 %>% mutate("firelabel"=FIRELABEL, "cause" = FIRE_CAUSE,
                  "firetype" = FIRE_TYPE, "responsetype" = RSPNS_TYPC) %>% 
  select(c("firelabel", "label", "igndate", "outdate", "burnlength",
           "cause", "fire_centre", "fire_zone", "firetype", "responsetype"))

#load BC extent to crop observations
ext <- st_read("Products/2020_bcvect_extent.gpkg")

#reproject extent to points CRS
ext <- ext %>% st_transform(crs = st_crs(d2))

#crop observations to extent
d2 <- d2 %>% st_filter(ext)

## Step 6: Save intermediate step as CSV

#drop geometry
d2_nogeo <- d2 %>% st_drop_geometry

#write csv to products folder
write.csv(d2_nogeo, "Products/2026_bcfire_pointdata.csv", row.names = FALSE)

## Step 7: Save polygons as GPKG

#select identifying variables
d2_point <- d2 %>% select("firelabel", "label")

#convert to spatVector
d2_point <- vect(d2_point)

#reproject points to EPSG:3979
d2_point <- d2_point %>% project("EPSG:3979")

#write gpkg
writeVector(d2_point, "Products/2026_bcfire_firepoints.gpkg")

## Step 8: Clear environment and load data

#clear environment
rm(list=ls())

#clear garbage
gc()

#load points
point <- st_read("Products/2026_bcfire_firepoints.gpkg")

#load polygons
poly <- st_read("Products/2026_bcfire_firepolygons.gpkg")

#load points data
dpoint <- read.csv("Products/2026_bcfire_pointdata.csv")

#load polygons data
dpoly <- read.csv("Products/2026_bcfire_polygondata.csv")

## Step 9: Extract polygon values

## Step 9.1: Average elevation

#load data
dem <- rast("Products/2014_bcrast_dem.nc")

#extract mean
poly$avgelevation <- exact_extract(dem, poly, "mean")

#join to dataframe
dpoly <- dpoly %>% left_join(st_drop_geometry(poly))

#select rows
poly <- poly %>% select(c("firelabel", "label"))

#remove from environment
rm(dem)

#clear garbage
gc()

## Step 9.2: Average slope

#load data
slope <- rast("Products/2014_bcrast_slope.nc")

#extract mean
poly$avgslope <- exact_extract(slope, poly, "mean")

#join to dataframe
dpoly <- dpoly %>% left_join(st_drop_geometry(poly))

#select rows
poly <- poly %>% select(c("firelabel", "label"))

#remove from environment
rm(slope)

#clear garbage
gc()

## Step 9.3: Ownership

#generate list of fires from 2023-2025
dpoly2023.2025 <- dpoly %>% filter(year(date) %in% c(2023, 2025))

#generate filtered polygons
poly2023.2025 <- poly %>% filter(label %in% dpoly2023.2025$label)

#load data
own <- rast("Products/2024_bcrast_ownership.nc")

#extract mode
poly2023.2025$avgownership <- exact_extract(own, poly2023.2025, "mode")

#join to dataframe
dpoly <- dpoly %>% left_join(st_drop_geometry(poly2023.2025))

#remove from environment
rm(own, dpoly2023.2025, poly2023.2025)

#clear garbage
gc()

## Step 9.4: Land cover

#generate list of fires from 2019-2024
dpoly2019.2024 <- dpoly %>% filter(year(date) %in% 2019:2024)

#generate filtered polygons
poly2019.2024 <- poly %>% filter(label %in% dpoly2019.2024$label)

#load data
lco <- rast("Products/2020_bcrast_landcover.nc")

#extract mode
poly2019.2024$avglandcover <- exact_extract(lco, poly2019.2024, "mode")

#remove from environment
rm(lco)

#clear garbage
gc()

#generate list of fires from 2014-2018
dpoly2014.2018 <- dpoly %>% filter(year(date) %in% 2014:2018)

#generate filtered polygons
poly2014.2018 <- poly %>% filter(label %in% dpoly2014.2018$label)

#load data
lco <- rast("Products/2015_bcrast_landcover.nc")

#extract mode
poly2014.2018$avglandcover <- exact_extract(lco, poly2014.2018, "mode")

#remove from environment
rm(lco)

#clear garbage
gc()

#generate list of fires from 2006-2013
dpoly2006.2013 <- dpoly %>% filter(year(date) %in% 2006:2013)

#generate filtered polygons
poly2006.2013 <- poly %>% filter(label %in% dpoly2006.2013$label)

#load data
lco <- rast("Products/2010_bcrast_landcover.nc")

#extract mode
poly2006.2013$avglandcover <- exact_extract(lco, poly2006.2013, "mode")

#combine extracted results
dpoly2006.2024 <- bind_rows(st_drop_geometry(poly2006.2013), st_drop_geometry(poly2014.2018),
                            st_drop_geometry(poly2019.2024))

#join to dataframe
dpoly <- dpoly %>% left_join(dpoly2006.2024)

#remove from environment
rm(lco, dpoly2006.2024, dpoly2006.2013, dpoly2014.2018, dpoly2019.2024,
   poly2006.2013, poly2014.2018, poly2019.2024)

#clear garbage
gc()

## Step 9.5: Wildland-urban interface: REMOVED FROM FINAL VERSION

#generate list of fires from 2016-2024
dpoly2016.2024 <- dpoly %>% filter(year(date) %in% 2016:2024)

#generate filtered polygons
poly2016.2024 <- poly %>% filter(label %in% dpoly2016.2024$label)

#load data
wui <- rast("Products/2020_bcrast_wui.nc")

#extract mean
poly2016.2024$avgwui <- exact_extract(wui, poly2016.2024, "mean")

#join to dataframe
dpoly <- dpoly %>% left_join(st_drop_geometry(poly2016.2024))

#remove from environment
rm(wui, dpoly2016.2024, poly2016.2024)

#clear garbage
gc()

## Step 9.6: Biogeoclimatic code

#generate list of fires from 2017-2025
dpoly2017.2025 <- dpoly %>% filter(year(date) %in% 2017:2025)

#generate filtered polygons
poly2017.2025 <- poly %>% filter(label %in% dpoly2017.2025$label)

#load data
bec <- rast("Products/2021_bcrast_bec.nc")

#extract mode
poly2017.2025$avgbeccode <- exact_extract(bec, poly2017.2025, "mode")

#join to dataframe
dpoly <- dpoly %>% left_join(st_drop_geometry(poly2017.2025))

#remove from environment
rm(bec, dpoly2017.2025, poly2017.2025)

#clear garbage
gc()

## Step 9.7: Natural disturbance type

#generate list of fires from 2017-2025
dpoly2017.2025 <- dpoly %>% filter(year(date) %in% 2017:2025)

#generate filtered polygons
poly2017.2025 <- poly %>% filter(label %in% dpoly2017.2025$label)

#load data
ndt <- rast("Products/2021_bcrast_ndt.nc")

#extract mode
poly2017.2025$avgndtcode <- exact_extract(ndt, poly2017.2025, "mode")

#join to dataframe
dpoly <- dpoly %>% left_join(st_drop_geometry(poly2017.2025))

#remove from environment
rm(ndt, dpoly2017.2025, poly2017.2025)

#clear garbage
gc()

## Step 9.8: Fuel type

#generate list of fires from 2020-2025
dpoly2020.2025 <- dpoly %>% filter(year(date) %in% 2020:2025)

#generate filtered polygons
poly2020.2025 <- poly %>% filter(label %in% dpoly2020.2025$label)

#load data
fuel <- rast("Products/2024_bcrast_fueltypes.nc")

#extract mode
poly2020.2025$avgfueltype <- exact_extract(fuel, poly2020.2025, "mode")

#join to dataframe
dpoly <- dpoly %>% left_join(st_drop_geometry(poly2020.2025))

#remove from environment
rm(fuel, dpoly2020.2025, poly2020.2025)

#clear garbage
gc()

## Step 9.9: Population

#generate list of fires from 2020-2025
dpoly2020.2025 <- dpoly %>% filter(year(date) %in% 2020:2025)

#generate filtered polygons
poly2020.2025 <- poly %>% filter(label %in% dpoly2020.2025$label)

#load data
pop <- rast("Products/2021_bcrast_population.nc")

#extract mean
poly2020.2025$avgpopulation <- exact_extract(pop, poly2020.2025, "mean")

#remove from environment
rm(pop)

#clear garbage
gc()

#generate list of fires from 2015-2019
dpoly2015.2019 <- dpoly %>% filter(year(date) %in% 2015:2019)

#generate filtered polygons
poly2015.2019 <- poly %>% filter(label %in% dpoly2015.2019$label)

#load data
pop <- rast("Products/2016_bcrast_population.nc")

#extract mean
poly2015.2019$avgpopulation <- exact_extract(pop, poly2015.2019, "mean")

#remove from environment
rm(pop)

#clear garbage
gc()

#generate list of fires from 2010-2014
dpoly2010.2014 <- dpoly %>% filter(year(date) %in% 2010:2014)

#generate filtered polygons
poly2010.2014 <- poly %>% filter(label %in% dpoly2010.2014$label)

#load data
pop <- rast("Products/2011_bcrast_population.nc")

#extract mean
poly2010.2014$avgpopulation <- exact_extract(pop, poly2010.2014, "mean")

#remove from environment
rm(pop)

#clear garbage
gc()

#generate list of fires from 2002-2009
dpoly2002.2009 <- dpoly %>% filter(year(date) %in% 2002:2009)

#generate filtered polygons
poly2002.2009 <- poly %>% filter(label %in% dpoly2002.2009$label)

#load data
pop <- rast("Products/2006_bcrast_population.nc")

#extract mean
poly2002.2009$avgpopulation <- exact_extract(pop, poly2002.2009, "mean")

#combine extracted results
dpoly2002.2025 <- bind_rows(st_drop_geometry(poly2002.2009), st_drop_geometry(poly2010.2014),
                            st_drop_geometry(poly2015.2019), st_drop_geometry(poly2020.2025))

#join to dataframe
dpoly <- dpoly %>% left_join(dpoly2002.2025)

#remove from environment
rm(pop, dpoly2002.2025, dpoly2002.2009, dpoly2010.2014, dpoly2015.2019, dpoly2020.2025,
   poly2002.2009, poly2010.2014, poly2015.2019, poly2020.2025)

#clear garbage
gc()

## Step 9.10: Dwellings

#generate list of fires from 2020-2025
dpoly2020.2025 <- dpoly %>% filter(year(date) %in% 2020:2025)

#generate filtered polygons
poly2020.2025 <- poly %>% filter(label %in% dpoly2020.2025$label)

#load data
dwl <- rast("Products/2021_bcrast_dwellings.nc")

#extract mean
poly2020.2025$avgdwellings <- exact_extract(dwl, poly2020.2025, "mean")

#remove from environment
rm(dwl)

#clear garbage
gc()

#generate list of fires from 2015-2019
dpoly2015.2019 <- dpoly %>% filter(year(date) %in% 2015:2019)

#generate filtered polygons
poly2015.2019 <- poly %>% filter(label %in% dpoly2015.2019$label)

#load data
dwl <- rast("Products/2016_bcrast_dwellings.nc")

#extract mean
poly2015.2019$avgdwellings <- exact_extract(dwl, poly2015.2019, "mean")

#remove from environment
rm(dwl)

#clear garbage
gc()

#generate list of fires from 2010-2014
dpoly2010.2014 <- dpoly %>% filter(year(date) %in% 2010:2014)

#generate filtered polygons
poly2010.2014 <- poly %>% filter(label %in% dpoly2010.2014$label)

#load data
dwl <- rast("Products/2011_bcrast_dwellings.nc")

#extract mean
poly2010.2014$avgdwellings <- exact_extract(dwl, poly2010.2014, "mean")

#remove from environment
rm(dwl)

#clear garbage
gc()

#generate list of fires from 2002-2009
dpoly2002.2009 <- dpoly %>% filter(year(date) %in% 2002:2009)

#generate filtered polygons
poly2002.2009 <- poly %>% filter(label %in% dpoly2002.2009$label)

#load data
dwl <- rast("Products/2006_bcrast_dwellings.nc")

#extract mean
poly2002.2009$avgdwellings <- exact_extract(dwl, poly2002.2009, "mean")

#combine extracted results
dpoly2002.2025 <- bind_rows(st_drop_geometry(poly2002.2009), st_drop_geometry(poly2010.2014),
                            st_drop_geometry(poly2015.2019), st_drop_geometry(poly2020.2025))

#join to dataframe
dpoly <- dpoly %>% left_join(dpoly2002.2025)

#remove from environment
rm(dwl, dpoly2002.2025, dpoly2002.2009, dpoly2010.2014, dpoly2015.2019, dpoly2020.2025,
   poly2002.2009, poly2010.2014, poly2015.2019, poly2020.2025)

#clear garbage
gc()

## Step 9.11: Forest Age

#generate list of fires from 2021-2023
dpoly2021.2023 <- dpoly %>% filter(year(date) %in% 2021:2023)

#generate filtered polygons
poly2021.2023 <- poly %>% filter(label %in% dpoly2021.2023$label)

#load data
age <- rast("Products/2022_bcrast_forestage.nc")

#extract mode
poly2021.2023$avgforestage <- exact_extract(age, poly2021.2023, "mode")

#join to dataframe
dpoly <- dpoly %>% left_join(st_drop_geometry(poly2021.2023))

#remove from environment
rm(age, dpoly2021.2023, poly2021.2023)

#clear garbage
gc()

## Step 9.12: Biomass

#generate list of fires from 2021-2023
dpoly2021.2023 <- dpoly %>% filter(year(date) %in% 2021:2023)

#generate filtered polygons
poly2021.2023 <- poly %>% filter(label %in% dpoly2021.2023$label)

#load data
biom <- rast("Products/2022_bcrast_biomass.nc")

#extract mean
poly2021.2023$avgbiomass <- exact_extract(biom, poly2021.2023, "mean")

#join to dataframe
dpoly <- dpoly %>% left_join(st_drop_geometry(poly2021.2023))

#remove from environment
rm(biom, dpoly2021.2023, poly2021.2023)

#clear garbage
gc()

## Step 9.13: Remove WUI and ownership, save dataframe

#remove wui and ownership
dpoly <- dpoly %>% select(-c(avgwui, avgownership))

#write csv
write.csv(dpoly, "Products/2026_bcfire_polygondata.csv", row.names = FALSE)

## Step 10: Extract point values

#add ID to point values
point$ID <- 1:nrow(point)

#add ID to dataframe
dpoint <- dpoint %>% left_join(point)

## Step 10.1: Elevation

#load data
dem <- rast("Products/2014_bcrast_dem.nc")

#extract
extpoint <- terra::extract(dem, point)

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint)

#remove from environment
rm(dem, extpoint)

#clear garbage
gc()

## Step 10.2: Slope

#load data
slope <- rast("Products/2014_bcrast_slope.nc")

#extract
extpoint <- terra::extract(slope, point)

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint)

#remove from environment
rm(slope, extpoint)

#clear garbage
gc()

## Step 10.3: Ownership

#generate list of fires from 2023-2025
dpoint2023.2025 <- dpoint %>% filter(year(igndate) %in% c(2023, 2025))

#generate filtered points
point2023.2025 <- point %>% filter(label %in% dpoint2023.2025$label)

#load data
own <- rast("Products/2024_bcrast_ownership.nc")

#extract
extpoint <- as.data.frame(terra::extract(own, point2023.2025, bind = TRUE))

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint)

#remove from environment
rm(own, extpoint, dpoint2023.2025, point2023.2025)

#clear garbage
gc()

## Step 10.4: Land cover

#generate list of fires from 2019-2024
dpoint2019.2024 <- dpoint %>% filter(year(igndate) %in% 2019:2024)

#generate filtered points
point2019.2024 <- point %>% filter(label %in% dpoint2019.2024$label)

#load data
lco <- rast("Products/2020_bcrast_landcover.nc")

#extract
extpoint2019.2024 <- as.data.frame(terra::extract(lco, point2019.2024, bind = TRUE))

#remove from environment
rm(lco)

#clear garbage
gc()

#generate list of fires from 2014-2018
dpoint2014.2018 <- dpoint %>% filter(year(igndate) %in% 2014:2018)

#generate filtered points
point2014.2018 <- point %>% filter(label %in% dpoint2014.2018$label)

#load data
lco <- rast("Products/2015_bcrast_landcover.nc")

#extract 
extpoint2014.2018 <- as.data.frame(terra::extract(lco, point2014.2018, bind = TRUE))

#remove from environment
rm(lco)

#clear garbage
gc()

#generate list of fires from 2006-2013
dpoint2006.2013 <- dpoint %>% filter(year(igndate) %in% 2006:2013)

#generate filtered points
point2006.2013 <- point %>% filter(label %in% dpoint2006.2013$label)

#load data
lco <- rast("Products/2010_bcrast_landcover.nc")

#extract 
extpoint2006.2013 <- as.data.frame(terra::extract(lco, point2006.2013, bind = TRUE))

#combine extracted results
extpoint2006.2024 <- bind_rows(extpoint2006.2013, extpoint2014.2018, extpoint2019.2024)

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint2006.2024)

#remove from environment
rm(lco, dpoint2006.2013, dpoint2014.2018, dpoint2019.2024,
   point2006.2013, point2014.2018, point2019.2024,
   extpoint2006.2013, extpoint2006.2024, extpoint2014.2018, extpoint2019.2024)

#clear garbage
gc()

## Step 10.5: Wildland-urban interface: REMOVED FROM FINAL VERSION

#generate list of fires from 2016-2024
dpoint2016.2024 <- dpoint %>% filter(year(igndate) %in% 2016:2024)

#generate filtered points
point2016.2024 <- point %>% filter(label %in% dpoint2016.2024$label)

#load data
wui <- rast("Products/2020_bcrast_wui.nc")

#extract 
extpoint2016.2024 <- as.data.frame(terra::extract(wui, point2016.2024, bind = TRUE))

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint2016.2024)

#remove from environment
rm(wui, dpoint2016.2024, point2016.2024, extpoint2016.2024)

#clear garbage
gc()

## Step 10.6: Biogeoclimatic code

#generate list of fires from 2017-2025
dpoint2017.2025 <- dpoint %>% filter(year(igndate) %in% 2017:2025)

#generate filtered points
point2017.2025 <- point %>% filter(label %in% dpoint2017.2025$label)

#load data
bec <- rast("Products/2021_bcrast_bec.nc")

#extract
extpoint2017.2025 <- as.data.frame(terra::extract(bec, point2017.2025, bind = TRUE))

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint2017.2025)

#remove from environment
rm(bec, dpoint2017.2025, point2017.2025, extpoint2017.2025)

#clear garbage
gc()

## Step 10.7: Natural disturbance type

#generate list of fires from 2017-2025
dpoint2017.2025 <- dpoint %>% filter(year(igndate) %in% 2017:2025)

#generate filtered points
point2017.2025 <- point %>% filter(label %in% dpoint2017.2025$label)

#load data
ndt <- rast("Products/2021_bcrast_ndt.nc")

#extract
extpoint2017.2025 <- as.data.frame(terra::extract(ndt, point2017.2025, bind = TRUE))

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint2017.2025)

#remove from environment
rm(ndt, dpoint2017.2025, point2017.2025, extpoint2017.2025)

#clear garbage
gc()

## Step 10.8: Fuel type

#generate list of fires from 2020-2025
dpoint2020.2025 <- dpoint %>% filter(year(igndate) %in% 2020:2025)

#generate filtered points
point2020.2025 <- point %>% filter(label %in% dpoint2020.2025$label)

#load data
fuel <- rast("Products/2024_bcrast_fueltypes.nc")

#extract
extpoint2020.2025 <- as.data.frame(terra::extract(fuel, point2020.2025, bind = TRUE))

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint2020.2025)

#remove from environment
rm(fuel, dpoint2020.2025, point2020.2025, extpoint2020.2025)

#clear garbage
gc()

## Step 10.9: Population

#generate list of fires from 2020-2025
dpoint2020.2025 <- dpoint %>% filter(year(igndate) %in% 2020:2025)

#generate filtered points
point2020.2025 <- point %>% filter(label %in% dpoint2020.2025$label)

#load data
pop <- rast("Products/2021_bcrast_population.nc")

#extract
extpoint2020.2025 <- as.data.frame(terra::extract(pop, point2020.2025, bind = TRUE))

#remove from environment
rm(pop)

#clear garbage
gc()

#generate list of fires from 2015-2019
dpoint2015.2019 <- dpoint %>% filter(year(igndate) %in% 2015:2019)

#generate filtered points
point2015.2019 <- point %>% filter(label %in% dpoint2015.2019$label)

#load data
pop <- rast("Products/2016_bcrast_population.nc")

#extract
extpoint2015.2019 <- as.data.frame(terra::extract(pop, point2015.2019, bind = TRUE))

#remove from environment
rm(pop)

#clear garbage
gc()

#generate list of fires from 2010-2014
dpoint2010.2014 <- dpoint %>% filter(year(igndate) %in% 2010:2014)

#generate filtered points
point2010.2014 <- point %>% filter(label %in% dpoint2010.2014$label)

#load data
pop <- rast("Products/2011_bcrast_population.nc")

#extract
extpoint2010.2014 <- as.data.frame(terra::extract(pop, point2010.2014, bind = TRUE))

#remove from environment
rm(pop)

#clear garbage
gc()

#generate list of fires from 2002-2009
dpoint2002.2009 <- dpoint %>% filter(year(igndate) %in% 2002:2009)

#generate filtered points
point2002.2009 <- point %>% filter(label %in% dpoint2002.2009$label)

#load data
pop <- rast("Products/2006_bcrast_population.nc")

#extract
extpoint2002.2009 <- as.data.frame(terra::extract(pop, point2002.2009, bind = TRUE))

#combine extracted results
extpoint2002.2025 <- bind_rows(extpoint2002.2009, extpoint2010.2014,
                            extpoint2015.2019, extpoint2020.2025)

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint2002.2025)

#remove from environment
rm(pop, dpoint2002.2009, dpoint2010.2014, dpoint2015.2019, dpoint2020.2025,
   point2002.2009, point2010.2014, point2015.2019, point2020.2025,
   extpoint2002.2025, extpoint2002.2009, extpoint2010.2014, extpoint2015.2019, extpoint2020.2025)

#clear garbage
gc()

## Step 10.10: Dwellings

#generate list of fires from 2020-2025
dpoint2020.2025 <- dpoint %>% filter(year(igndate) %in% 2020:2025)

#generate filtered points
point2020.2025 <- point %>% filter(label %in% dpoint2020.2025$label)

#load data
dwl <- rast("Products/2021_bcrast_dwellings.nc")

#extract
extpoint2020.2025 <- as.data.frame(terra::extract(dwl, point2020.2025, bind = TRUE))

#remove from environment
rm(dwl)

#clear garbage
gc()

#generate list of fires from 2015-2019
dpoint2015.2019 <- dpoint %>% filter(year(igndate) %in% 2015:2019)

#generate filtered points
point2015.2019 <- point %>% filter(label %in% dpoint2015.2019$label)

#load data
dwl <- rast("Products/2016_bcrast_dwellings.nc")

#extract
extpoint2015.2019 <- as.data.frame(terra::extract(dwl, point2015.2019, bind = TRUE))

#remove from environment
rm(dwl)

#clear garbage
gc()

#generate list of fires from 2010-2014
dpoint2010.2014 <- dpoint %>% filter(year(igndate) %in% 2010:2014)

#generate filtered points
point2010.2014 <- point %>% filter(label %in% dpoint2010.2014$label)

#load data
dwl <- rast("Products/2011_bcrast_dwellings.nc")

#extract
extpoint2010.2014 <- as.data.frame(terra::extract(dwl, point2010.2014, bind = TRUE))

#remove from environment
rm(dwl)

#clear garbage
gc()

#generate list of fires from 2002-2009
dpoint2002.2009 <- dpoint %>% filter(year(igndate) %in% 2002:2009)

#generate filtered points
point2002.2009 <- point %>% filter(label %in% dpoint2002.2009$label)

#load data
dwl <- rast("Products/2006_bcrast_dwellings.nc")

#extract
extpoint2002.2009 <- as.data.frame(terra::extract(dwl, point2002.2009, bind = TRUE))

#combine extracted results
extpoint2002.2025 <- bind_rows(extpoint2002.2009, extpoint2010.2014,
                            extpoint2015.2019, extpoint2020.2025)

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint2002.2025)

#remove from environment
rm(dwl, dpoint2002.2009, dpoint2010.2014, dpoint2015.2019, dpoint2020.2025,
   point2002.2009, point2010.2014, point2015.2019, point2020.2025,
   extpoint2002.2025, extpoint2002.2009, extpoint2010.2014, extpoint2015.2019, extpoint2020.2025)

#clear garbage
gc()

## Step 10.11: Forest Age

#generate list of fires from 2021-2023
dpoint2021.2023 <- dpoint %>% filter(year(igndate) %in% 2021:2023)

#generate filtered pointgons
point2021.2023 <- point %>% filter(label %in% dpoint2021.2023$label)

#load data
age <- rast("Products/2022_bcrast_forestage.nc")

#extract
extpoint2021.2023 <- as.data.frame(terra::extract(age, point2021.2023, bind = TRUE))

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint2021.2023)

#remove from environment
rm(age, dpoint2021.2023, point2021.2023, extpoint2021.2023)

#clear garbage
gc()

## Step 10.12: Biomass

#generate list of fires from 2021-2023
dpoint2021.2023 <- dpoint %>% filter(year(igndate) %in% 2021:2023)

#generate filtered points
point2021.2023 <- point %>% filter(label %in% dpoint2021.2023$label)

#load data
biom <- rast("Products/2022_bcrast_biomass.nc")

#extract
extpoint2021.2023 <- as.data.frame(terra::extract(biom, point2021.2023, bind = TRUE))

#join to dataframe
dpoint <- dpoint %>% left_join(extpoint2021.2023)

#remove from environment
rm(biom, dpoint2021.2023, point2021.2023, extpoint2021.2023)

#clear garbage
gc()

## Step 10.13: Remove irrelevant variables

#remove geom, ID, WUI
dpoint <- dpoint %>% select(-c(geom, ID, wui, ownership))

## Step 10.14: Save dataframe

#write csv
write.csv(dpoint, "Products/2026_bcfire_pointdata.csv", row.names = FALSE)

## Step 11: Combined dataframe

#create combined dataframe of fires with observations in both points and polygons
dboth <- dpoint %>% filter(label %in% dpoly$label) %>% 
  left_join(dpoly, by = c("firelabel", "label", "cause"))

#reorder variables; NOTE: WUI AND OWNERSHIP DATA EXCLUDED
dboth <- dboth %>% select(c("firelabel", "label", "date", "igndate", "outdate", "burnlength",
                            "area_sqm", "datasource", "creationnotes", "cause", "fire_centre", "fire_zone",
                            "firetype", "responsetype", "elevation", "avgelevation", "slope", "avgslope",
                            "landcover", "avglandcover", "beccode", "avgbeccode", 
                            "ndtcode", "avgndtcode", "fueltype", "avgfueltype",
                            "population", "avgpopulation", "dwellings", "avgdwellings", "forestage",
                            "avgforestage", "biomass", "avgbiomass"))

#write csv to products folder
write.csv(dboth, "Products/2026_bcfire_bothdata.csv", row.names = FALSE)
