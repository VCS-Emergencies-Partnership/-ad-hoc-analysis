# Load libraries
library(tidyverse)
library(sf)
library(ggplot2)

# Get LRF boundaries from the geoportal
# https://geoportal.statistics.gov.uk/datasets/ons::local-resilience-forums-december-2020-boundaries-ew-buc-v2-2/explore
lrf_boundaries <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Resilience_Forums_December_2020_Boundaries_EW_BUC_V2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

# Create blank LRF region map
lrf_boundaries |>
  ggplot() +
  geom_sf() +
  theme_void()

# Create a file with LRF names for engagement team to complete
# lrf_boundaries |>
#   select(LRF20NM) |>
#   st_drop_geometry() |>
#   write_csv("analysis/lrf_boundaries/outputs/lrf_names.csv")

# Read in the data file containing the Green/Amber rating assigned by the engagement team
rating <- read_csv("analysis/lrf_boundaries/data/lrf_names.csv")

# Join this to the boundary file
lrf_rating <- lrf_boundaries |>
  left_join(rating)

# Produce map with the rating assigned
lrf_rating |>
  ggplot() +
  geom_sf(aes(fill = rating)) +
  theme_void() +
  scale_fill_manual(values = c("green", "yellow"), na.value="lightgrey") +
  theme(legend.position="none")

# Save the map
ggsave("lrf_rating.png", path = "analysis/lrf_boundaries/outputs/", width = 15, height = 8, dpi = 300)
