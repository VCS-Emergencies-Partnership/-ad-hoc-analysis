library(sf)
library(tidyverse)
library(geographr)
library(arrow)

# Combine floods and weather
weather <- read_sf("data/weather_spatial.geojson")
flood <- read_sf("data/floods_spatial.geojson")

flood_colour <- flood |>
  mutate(rating = case_when(
    severity_level == 1 ~ "amber",
    severity_level == 2 ~ "amber",
    severity_level == 3 ~ "red",
    severity_level == 4 ~ "red"
  )) |>
  mutate(alert_type = "flood") |>
  select(alert_type, rating, geometry)

warnings <- weather |>
  mutate(alert_type = weather, .before = everything()) |>
  select(-id, -weather) |>
  bind_rows(flood_colour) |>
  mutate(id = row_number(), .before = everything())

# Lookup
england_lookup <- lookup_postcode_oa_lsoa_msoa_lad |>
  filter(str_detect(lad_code, "^E")) |>
  left_join(lookup_counties_ua_lad, by = "lad_code")

boundaries_lsoa_projected <- boundaries_lsoa |>
  st_transform(crs = 27700)

# LSOAs in alerts 
lsoa_alerts <- warnings |>
  st_transform(crs = 27700) |>
  st_join(boundaries_lsoa_projected) |>
  st_drop_geometry() 

# Save data
lsoa_alerts |>
  write_feather("data/lsoa_alerts.feather")

england_lookup |>
  write_feather("data/england_lookup.feather")

warnings |>
  write_sf("data/warnings.geojson")


