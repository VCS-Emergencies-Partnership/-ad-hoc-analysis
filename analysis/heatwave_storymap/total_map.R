library(tidyverse)
library(sf)
library(geographr)

# Load heat scores
load("data/vuln_scores_heat_lsoa.rda")

heat_scores <- vuln_scores_heat_lsoa |>
  select(lsoa11_code, lsoa11_name, sphvi_climate_just_class, class_cleaned)

# Load boundary file
LTLA_boundaries <- sf::st_read("data/LSOA_2011_Boundaries.geojson") |>
  filter(str_detect(LSOA11CD, "^E"))

heat_scores <- heat_scores |>
  left_join(LTLA_boundaries, by = c("lsoa11_code" = "LSOA11CD")) |>
  left_join(geographr::lookup_lsoa11_ltla21) |>
  left_join(geographr::lookup_ltla21_vcsep22) |>
  select(lsoa11_code, lsoa11_name, ltla21_name, vcsep_region, sphvi_climate_just_class, class_cleaned, BNG_E, BNG_N, LONG, LAT, GlobalID, geometry) |>
  st_as_sf()

heat_scores |>
  sf::st_write("outputs/heat_scores.geojson")
