## Technical Validation
## Visual Inspection

## Step 1: Call libraries

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(viridis)
library(terra)
library(sf)
library(tidyterra)
library(patchwork)
library(cowplot)

## Step 1: DEM

#load data
dem <- rast("Products/2014_bcrast_dem.nc")

#create dem nap
map_dem <- ggplot() +
  geom_spatraster(data = dem) +
  scale_fill_gradientn(
    colours = c("#00441B", "#238B45", "#FFFF00", "#AA000D", "#FFFFFF"),
    values = c(0, 0.25, 0.35, 0.5, 1),
    breaks = c(0, 1000, 2000, 3000, 4657),
    limits = c(0, 4657),
    name = "Elevation (m)", oob = scales::squish, guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
    na.value = "transparent") +
  theme_map() +
  theme(text = element_text(family = "sans", size = 12))

#plot dem
plot(map_dem)

#save dem map
ggsave("Data/Validation/2014_bcrast_dem.pdf", map_dem, width = 8, height = 8, units = "in", dpi = 320)

## Step 2: Slope

#load data
slope <- rast("Products/2014_bcrast_slope.nc")

#create slope map
map_slope <- ggplot() +
  geom_spatraster(data = slope) +
  scale_fill_gradientn(
    colours = c("black", "yellow"),
    limits = c(0, 83),
    breaks = c(0, 20, 40, 60, 83),
    name = "Slope (Degrees)", oob = scales::squish, guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
    na.value = "transparent") +
  theme_map() +
  theme(text = element_text(family = "sans", size = 12))

#plot slope
plot(map_slope)

#save slope map
ggsave("Data/Validation/2014_bcrast_slope.pdf", map_slope, width = 8, height = 8, units = "in", dpi = 320)

## Step 3: Fuel types

#load data
fuels <- rast("Products/2024_bcrast_fueltypes.nc")

#load docs
fuels_docs <- read.csv("Documentation/2024_bcrast_fueltypes.csv")

#factor fuels raster
fuels_fac <- as.factor(fuels)

#create fuels map
map_fuels <- ggplot() +
  geom_spatraster(data = fuels_fac) +
  scale_fill_manual(
    values = c("#125d0d", "#3a7e2f", "#5da14f", "#81c670",
               "#a5eb93", "#c9f5bd", "#6e4f31", "#906e4e",
               "#b38e6d", "#d7b08e", "#cb4154", "#ff7b87",
               "#f0833a", "#999999", "#88aa88", "#0000bb"),
    name = "Fuel Type",
    breaks = fuels_docs$fueltype[c(1, 2, 3, 4, 5, 6, 14, 15,
                                   16, 13, 7, 8, 9, 10, 12, 11)],
    labels = setNames(fuels_docs$fuellongname, fuels_docs$fueltype),
    na.value = "transparent") +
  theme_map() +
  theme(text = element_text(family = "sans", size = 12))

#plot fuels
plot(map_fuels)

#save fuels map
ggsave("Data/Validation/2024_bcrast_fueltypes.pdf", map_fuels, width = 8, height = 8, units = "in", dpi = 320)

## Step 4: Forest Cover Ownership

#load data
ownership <- rast("Products/2024_bcrast_ownership.nc")

#load documentation
ownership_docs <- read.csv("Documentation/2024_bcrast_ownership.csv")

#factor ownership raster
ownership_fac <- as.factor(ownership)

#adjust ownership docs to add type tag
ownership_docs$owntype <- case_match(ownership_docs$ownership, c(40, 41)~"Private",
                                        c(50:54)~"Federal", c(60:69, 74, 80, 81)~"Crown",
                                        c(70, 72, 75, 77:79)~"Crown Tenure", 99~"Crown Lease",
                                        91~"Unknown & Other")

#adjust ownership docs to add dummy spatial data
ownership_docs$x <- xmin(ownership_fac)
ownership_docs$y <- ymin(ownership_fac)

#convert ownership docs to sf
ownership_docs_sf <- st_as_sf(ownership_docs, coords = c("x","y"))

#add crs
ownership_docs_sf <- ownership_docs_sf %>% st_set_crs(crs(ownership_fac))

#create ownership map
map_ownership <- ggplot() +
  geom_spatraster(data = ownership_fac, show.legend = FALSE) +
  scale_fill_manual(
    values = c("#cb4154", "#ff7b87", "#6e4f31",
               "#906e4e", "#b38e6d", "#d7b08e", "#440154FF",
               "#460B5EFF", "#481568FF", "#481D6FFF", "#482677FF",
               "#472F7DFF", "#453781FF", "#423F85FF", "#3F4788FF",
               "#3D4E8AFF", "#74D055FF", "#84D44BFF", "#32648EFF",
               "#94D840FF", "#A6DB35FF", "#B8DE29FF", "#CAE11FFF",
               "#39558CFF", "#365D8DFF", "#999999", "#f0833a"),
    na.value = "transparent") +
  geom_sf(data = ownership_docs_sf, aes(color = owntype), size = 0, key_glyph = draw_key_rect) +
  scale_color_manual(
    name = "Forest Cover Ownership",
    values = c("#440154FF", "#f0833a", "#74D055FF",
               "#6e4f31", "#cb4154", "#999999"),
    guide = guide_legend(byrow = TRUE, keyheight = unit(14, "pt"),
                         keywidth = unit(14, "pt"), override.aes = list(shape = 15, size = 7))) +
  theme_map() +
  theme(text = element_text(family = "sans", size = 12), legend.key.spacing.y = unit(0, "pt"),
        legend.spacing.y = unit(0, "pt"), legend.text = element_text(margin = margin(0,0,5,5)),
        legend.margin = margin(0,0,0,0), legend.box.margin = margin(0,0,0,0))

#plot ownership map
plot(map_ownership)

#save ownership map
ggsave("Data/Validation/2024_bcrast_ownership.pdf", map_ownership, width = 8, height = 8, units = "in", dpi = 320)

## Step 5: PSTA Fire Density

#load data
pstafire <- rast("Products/2021_bcrast_pstafire.nc")

#load documentation
pstafire_docs <- read.csv("Documentation/2021_bcrast_pstafire.csv")

#factor PSTA fire raster
pstafire_fac <- as.factor(pstafire)

#create fire density map
map_pstafire <- ggplot() +
  geom_spatraster(data = pstafire_fac) +
  scale_fill_manual(
    values = c("#F3E55CFF", "#FAC127FF", "#FB9E07FF", "#F57D15FF",
               "#E8602DFF", "#D44842FF", "#BB3754FF", "#9F2A63FF",
               "#82206CFF", "#65156EFF", "#480B6AFF", "#0000bb"),
    name = "Fire Density",
    breaks = pstafire_docs$fire_density[c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1)],
    labels = setNames(pstafire_docs$FR_STRT_DY, pstafire_docs$fire_density),
    na.value = "transparent") +
  theme_map() +
  theme(text = element_text(family = "sans", size = 12))

#plot fire density map
plot(map_pstafire)

#save fire density map
ggsave("Data/Validation/2021_bcrast_pstafire.pdf", map_pstafire, width = 8, height = 8, units = "in", dpi = 320)

## Step 6: PSTA Lightning Fire Density

#load data
pstalightning <- rast("Products/2021_bcrast_pstalightning.nc")

#load documentation
pstalightning_docs <- read.csv("Documentation/2021_bcrast_pstalightning.csv")

#factor PSTA fire raster
pstalightning_fac <- as.factor(pstalightning)

#create fire density map
map_pstalightning <- ggplot() +
  geom_spatraster(data = pstalightning_fac) +
  scale_fill_manual(
    values = c("#F3E55CFF", "#FAC127FF", "#FB9E07FF", "#F57D15FF",
               "#E8602DFF", "#D44842FF", "#BB3754FF", "#9F2A63FF",
               "#82206CFF", "#65156EFF", "#480B6AFF", "#0000bb"),
    name = "Lightning Fire Density",
    breaks = pstalightning_docs$lightning_density[c(2, 3, 4, 5, 6, 7, 8, 9, 10, 1)],
    labels = setNames(pstalightning_docs$FR_STRT_DY, pstalightning_docs$lightning_density),
    na.value = "transparent") +
  theme_map() +
  theme(text = element_text(family = "sans", size = 12))

#plot lightning fire density map
plot(map_pstalightning)

#save lightning fire head map
ggsave("Data/Validation/2021_bcrast_pstalightning.pdf", map_pstalightning, width = 8, height = 8, units = "in", dpi = 320)

## Step 7: PSTA Human Fire Density

#load data
pstahuman <- rast("Products/2021_bcrast_pstahuman.nc")

#load documentation
pstahuman_docs <- read.csv("Documentation/2021_bcrast_pstahuman.csv")

#factor PSTA fire raster
pstahuman_fac <- as.factor(pstahuman)

#create human fire density map
map_pstahuman <- ggplot() +
  geom_spatraster(data = pstahuman_fac) +
  scale_fill_manual(
    values = c("#F3E55CFF", "#FAC127FF", "#FB9E07FF", "#F57D15FF",
               "#E8602DFF", "#D44842FF", "#BB3754FF", "#9F2A63FF",
               "#82206CFF", "#65156EFF", "#480B6AFF", "#0000bb"),
    name = "Human Fire Density",
    breaks = pstahuman_docs$human_density[c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1)],
    labels = setNames(pstahuman_docs$FR_STRT_DY, pstahuman_docs$human_density),
    na.value = "transparent") +
  theme_map() +
  theme(text = element_text(family = "sans", size = 12))

#plot human fire density map
plot(map_pstahuman)

#save human fire density map
ggsave("Data/Validation/2021_bcrast_pstahuman.pdf", map_pstahuman, width = 8, height = 8, units = "in", dpi = 320)

## Step 8: PSTA Head Fire Intensity

#load data
pstahead <- rast("Products/2021_bcrast_pstahead.nc")

#load documentation
pstahead_docs <- read.csv("Documentation/2021_bcrast_pstahead.csv")

#factor PSTA fire raster
pstahead_fac <- as.factor(pstahead)

#adjust documentation for visual consistency
pstahead_docs$hf_level <- gsub(" kW/m", "", pstahead_docs$Head.Fire.Intensity.Class.Range)

#create fire intensity map
map_pstahead <- ggplot() +
  geom_spatraster(data = pstahead_fac) +
  scale_fill_manual(
    values = c("#F3E55CFF", "#FAC127FF", "#FB9E07FF", "#F57D15FF",
               "#E8602DFF", "#D44842FF", "#BB3754FF", "#9F2A63FF",
               "#82206CFF", "#65156EFF", "#0000bb", "#6e4f31",
               "#999999", "#88aa88"),
    name = "Head Fire Intensity (kW/m)",
    breaks = pstahead_docs$hf_intensity[c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 3, 4, 1, 2)],
    labels = setNames(pstahead_docs$hf_level, pstahead_docs$hf_intensity),
    na.value = "transparent") +
  theme_map() +
  theme(text = element_text(family = "sans", size = 12))

#plot fire intensity map
plot(map_pstahead)

#save fire intensity map
ggsave("Data/Validation/2021_bcrast_pstahead.pdf", map_pstahead, width = 8, height = 8, units = "in", dpi = 320)

## Step 9: BEC

#load data
bec <- rast("Products/2021_bcrast_bec.nc")

#load documentation
bec_docs <- read.csv("Documentation/2021_bcrast_bec.csv")

#factor BEC raster
bec_fac <- as.factor(bec)

#adjust BEC docs for visual consistency
bec_docs$bec_zone <- gsub("--", "—", bec_docs$ZONE_NAME)

#adjust BEC docs to add dummy spatial data
bec_docs$x <- xmin(bec_fac)
bec_docs$y <- ymin(bec_fac)

#convert BEC docs to sf
bec_docs_sf <- st_as_sf(bec_docs, coords = c("x","y"))

#add crs
bec_docs_sf <- bec_docs_sf %>% st_set_crs(crs(bec_fac))

#create BEC map
map_bec <- ggplot() +
  geom_spatraster(data = bec_fac, show.legend = FALSE) +
  scale_fill_manual(
    values = c("#72FF28", colorRampPalette(c("#0000b1", "#1a4ded"))(3),
               "#1FC8F3", "#cb4154", "#ff7b87", "#f0833a", "#fea35a",
               "#6e4f31", "#906e4e", "#b38e6d", "#d7b08e", "#440154FF",
               "#460B5EFF", "#481568FF", "#481D6FFF", "#482677FF",
               "#472F7DFF", "#74D055FF", "#84D44BFF", "#94D840FF",
               "#A6DB35FF", "#B8DE29FF", "#CAE11FFF",
               colorRampPalette(c("#999999", "#bbbbbb"))(5),
               colorRampPalette(c("#6C1417", "#8C3437"))(7),
               colorRampPalette(c("#FA48D8", "#FA98f8"))(10),
               colorRampPalette(c("#EED008", "#FEE078"))(11),
               colorRampPalette(c("#0A4646", "#2A6666"))(13),
               colorRampPalette(c("#F21522", "#F24552"))(13),
               colorRampPalette(c("#543D87", "#6D52a4"))(47),
               "#aadddd", "#baeded"),
    na.value = "transparent") +
  theme_map() +
  geom_sf(data = bec_docs_sf, aes(color = bec_zone), size = 0, key_glyph = draw_key_rect) +
  scale_color_manual(
    name = "Biogeoclimatic Zone",
    values = c("#aadddd", "#f0833a", "#999999", "#1FC8F3",
               "#0000b1", "#FA48D8", "#543D87", "#F21522",
               "#0A4646", "#cb4154", "#74D055FF", "#6C1417",
               "#72FF28", "#6e4f31", "#EED008", "#440154FF"),
    guide = guide_legend(byrow = TRUE, keyheight = unit(14, "pt"),
                         keywidth = unit(14, "pt"), override.aes = list(shape = 15, size = 7))) +
  theme(text = element_text(family = "sans", size = 12), legend.key.spacing.y = unit(0, "pt"),
        legend.spacing.y = unit(0, "pt"), legend.text = element_text(margin = margin(0,0,5,5)),
        legend.margin = margin(0,0,0,0), legend.box.margin = margin(0,0,0,0))

#plot BEC map
plot(map_bec)

#save BEC map
ggsave("Data/Validation/2021_bcrast_bec.pdf", map_bec, width = 8, height = 8, units = "in", dpi = 320)

## Step 10: Wildland-Urban Interface Buffer

#load data
wui <- rast("Products/2020_bcrast_wui.nc")

#load extent data
ext <- vect("Products/2025_bcvect_extent.gpkg")

#factor wui raster
wui_fac <- as.factor(wui)

#create wui  map
map_wui <- ggplot() +
  geom_spatvector(data = ext, fill = "#bbbbbb", colour = "#000000",
                  linewidth = 0.5, show.legend = FALSE) +
  geom_spatraster(data = wui_fac) +
  scale_fill_manual(
    values = c("#6C1417"),
    name = "Wildland-Urban Interface",
    labels = c("1" = "2km Buffer"),
    breaks = c("1"),
    na.value = "transparent") +
  theme_map() +
  theme(text = element_text(family = "sans", size = 12))

#plot wui map
plot(map_wui)

#save wui map
ggsave("Data/Validation/2020_bcrast_wui.pdf", map_wui, width = 8, height = 8, units = "in", dpi = 320)

## Step 11: Natural Disturbance Type

#load data
ndt <- rast("Products/2021_bcrast_ndt.nc")

#load documentation
ndt_docs <- read.csv("Documentation/2021_bcrast_ndt.csv")

#factor ndt raster
ndt_fac <- as.factor(ndt)

#create ndt map
map_ndt <- ggplot() +
  geom_spatraster(data = ndt_fac) +
  scale_fill_manual(
    values = c("#F3E55CFF", "#65156EFF", "#0A4646", "#6C1417", "#6e4f31"),
    name = "Natural Disturbance Type",
    breaks = ndt_docs$ndtcode,
    labels = setNames(ndt_docs$NTRL_DSTRC, ndt_docs$ndtcode),
    na.value = "transparent") +
  theme_map() +
  theme(text = element_text(family = "sans", size = 12))

#plot ndt map
plot(map_ndt)

#save ndt map
ggsave("Data/Validation/2021_bcrast_ndt.pdf", map_ndt, width = 8, height = 8, units = "in", dpi = 320)

## Step 12: Join maps for figure

#join maps together
map_combined <- wrap_plots(map_bec, map_fuels, map_ownership, map_pstahead, ncol = 2, nrow = 2)

#save combined maps
ggsave("Data/Validation/CombinedMap.pdf", map_combined, width = 16, height = 16, units = "in", dpi = 320)

