## Data Descriptor
## Paper Graphics

## Step 1: Call libraries

library(ggplot2)
library(ggbreak)
library(tidyverse)

## Step 2: Create data availability plot

#create dataframe of extracted data periods
periods <- data.frame(variable = c("Wildfire Perimeters", "Incident Locations",
                                   "Elevation", "Slope", "Land Cover", "Land Cover", "Land Cover",
                                   "Biogeoclimatic Code", "Natural Disturbance Type",
                                   "Fuel Type", "Population", "Population", "Population", "Population",
                                   "Dwellings", "Dwellings", "Dwellings", "Dwellings",
                                   "Forest Age", "Forest Biomass"),
                      start = c(1916.75, 1949.75,
                                1916.75, 1916.75, 2018.75, 2013.75, 2005.75,
                                2016.75, 2016.75,
                                2019.75, 2019.75, 2014.75, 2009.75, 2001.75,
                                2019.75, 2014.75, 2009.75, 2001.75,
                                2020.75, 2020.75),
                      end = c(2025.25, 2025.25,
                              2025.25, 2025.25, 2024.25, 2018.75, 2013.75,
                              2025.25, 2025.25,
                              2025.25, 2025.25, 2019.75, 2014.75, 2009.75,
                              2025.25, 2019.75, 2014.75, 2009.75,
                              2023.25, 2023.25),
                      fill = c("#0D3E1F", "#144826",
                               "#1A522E", "#215C35", "#2E7044", "#2E7044", "#2E7044",
                               "#3B8353", "#418D5A",
                               "#489761", "#4EA169", "#4EA169", "#4EA169", "#4EA169",
                               "#55AB70", "#55AB70", "#55AB70", "#55AB70",
                               "#5BB578", "#62BF7F"),
                      y = c(12, 11,
                            10, 9, 8, 8, 8,
                            7, 6,
                            5, 4, 4, 4, 4,
                            3, 3, 3, 3,
                            2, 1))

#create dataframe of extracted data years
years <- data.frame(variable = c("Wildfire Perimeters", "Incident Locations",
                                 "Elevation", "Slope", "Land Cover", "Land Cover", "Land Cover",
                                 "Biogeoclimatic Code", "Natural Disturbance Type",
                                 "Fuel Type", "Population", "Population", "Population", "Population",
                                 "Dwellings", "Dwellings", "Dwellings", "Dwellings",
                                 "Forest Age", "Forest Biomass"),
                    x = c(2026, 2026,
                          2014, 2014, 2020, 2015, 2010,
                          2021, 2021,
                          2024, 2021, 2016, 2011, 2006,
                          2021, 2016, 2011, 2006,
                          2022, 2022),
                    y = c(12, 11,
                          10, 9, 8, 8, 8,
                          7, 6,
                          5, 4, 4, 4, 4,
                          3, 3, 3, 3,
                          2, 1))

#create ggplot
yearplot <- ggplot() +
  geom_rect(data = periods, aes(xmin = start, xmax = end, ymin = y - 0.2, ymax = y + 0.2, fill = fill),
            colour = "black", linewidth = 0.4) +
  scale_fill_identity() +
  geom_segment(data = years, aes(x = x, xend = x, y = y - 0.2, yend = y + 0.2),
               colour = "#BB002F", linewidth = 1) +
  geom_segment(aes(x = 1915.7, xend = 1915.7, y = 0.6, yend = 12.4), colour = "black", linewidth = 0.5) +
  geom_segment(aes(x = 2027, xend = 2027, y = 0.6, yend = 12.4), colour = "black", linewidth = 0.5) +
  geom_segment(aes(x = 1915.7, xend = 2027, y = 12.4, yend = 12.4), colour = "black", linewidth = 0.5) +
  geom_segment(aes(x = 1915.7, xend = 2027, y = 0.6, yend = 0.6), colour = "black", linewidth = 0.5) +
  scale_x_break(c(1922, 1948), scales = 2/3, space = 0.2, symbol = "slash") +
  scale_x_break(c(1952, 1999), scales = 27/4, space = 0.2, symbol = "slash") +
  scale_y_continuous(breaks = 1:12, labels = c("Forest Biomass", "Forest Age", "Dwellings", "Population", "Fuel Type",
                                               "Natural Disturbance Type", "Biogeoclimatic Code",
                                               "Land Cover", "Slope", "Elevation",
                                               "Incident Locations", "Wildfire Perimeters"),
                     position = "right", limits = c(0.6, 12.4), expand = expansion(mult = 0, add = 0)) +
  scale_x_continuous(breaks = seq(1920, 2025, by = 5), limits = c(1915.7, 2027),
                     expand = expansion(mult = 0)) +
  labs(x = "Availability (Year)", y = "Variable") +
  theme(text = element_text(family = "sans", size = 12, colour = "black"),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#888888"),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length.y = unit(-10, "pt"),
        axis.ticks.length.x = unit(6, "pt"))

#plot visual
print(yearplot)

#save plot
ggsave("Data/bcfire_years.pdf", yearplot, width = 6, height = 4, units = "in", dpi = 400)

#delete first page from pdf manually
#reconnect first four variable plots manually


