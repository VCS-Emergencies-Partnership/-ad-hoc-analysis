library(sf)
library(tidyverse)
library(geographr)
library(rmapshaper)

# Check of different boundary files to check same ----
# Shapefile with boundaries downloaded from https://britishredcross.maps.arcgis.com/apps/webappviewer/index.html?id=b2fec0e028554a5aac99d3519c81ab44
# Downloaded by Andrew Braye from BRC GIS 
raw_arcgis <- read_sf("gbrvcsep_admn_ad0_py_s0_public_MACAreas_BFE_BNG/gbrvcsep_admn_ad0_py_s0_public_MACAreas_BFE_BNG.shp")

# Shapefile with boundariesfrom https://github.com/VCS-Emergencies-Partnership/r-shiny-web-apps/tree/main/packages/dashboard/data/data_not_from_azure/reduced_boundaries
raw_github <- read_sf("github_files/vcsep_multiagencycells_wo-iom-ci_BFE.shp")

# ArcGIS
vcsep_region_boundaries_old_arcgis <- raw_arcgis |>
  st_transform(crs = 4326) |>
  ms_simplify() |>
  filter(!EPAreaNM %in% c("Northern Ireland and Isle of Man", "Scotland", "Wales"))

# Github - already simplified so don't need to 
vcsep_region_boundaries_old_github <- raw_github  |>
  st_transform(crs = 4326)  |>
  rename(EPAreaNM = lookup_loc) |>
  mutate(EPAreaNM = ifelse(EPAreaNM == "South and the Channel Islands", "South West", EPAreaNM)) |>
  filter(!EPAreaNM %in% c("Northern Ireland and Isle of Man", "Scotland", "Wales"))

object.size(vcsep_region_boundaries_old_arcgis)
object.size(vcsep_region_boundaries_old_github)

# Plotting
vcsep_region_boundaries_old_arcgis |>
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = EPAreaNM))

vcsep_region_boundaries_old_github |>
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = EPAreaNM))

# Comparing boundaries between 2 files
mid_diff <- st_difference(vcsep_region_boundaries_old_arcgis$geometry[1], vcsep_region_boundaries_old_github$geometry[1]) 
london_diff <- st_difference(vcsep_region_boundaries_old_arcgis$geometry[2], vcsep_region_boundaries_old_github$geometry[2]) 
north_diff <- st_difference(vcsep_region_boundaries_old_arcgis$geometry[3], vcsep_region_boundaries_old_github$geometry[3]) 
sw_diff <- st_difference(vcsep_region_boundaries_old_arcgis$geometry[4], vcsep_region_boundaries_old_github$geometry[4]) 
se_diff <- st_difference(vcsep_region_boundaries_old_arcgis$geometry[5], vcsep_region_boundaries_old_github$geometry[5]) 

mid_diff |
  ggplot() +
  geom_sf(fill = "red")

london_diff |>
  ggplot() +
  geom_sf(fill = "red")

north_diff |>
  ggplot() +
  geom_sf(fill = "red")

sw_diff |>
  ggplot() +
  geom_sf(fill = "red")

se_diff |>
  ggplot() +
  geom_sf(fill = "red")

# Small differences in the boundaries but overall seem to be covering the same areas
# Use Github version since smaller in object size
vcsep_region_boundaries_old <- vcsep_region_boundaries_old_github

# BRC Tactical Cells ----
lookup_lad_brc <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20local%20authority%20to%20tactical%20cell.csv")

ltla19_21 <- geographr::lookup_ltla_ltla |>
  select(ltla19_code, ltla21_code) 

lookup_lad_brc_ltla21 <- lookup_lad_brc |>
  left_join(ltla19_21, by = c("LAD19CD" = "ltla19_code")) |>
  select(ltla21_code, TacticalCell) |>
  distinct(ltla21_code, TacticalCell)

lookup_lad_brc_ltla21_sf <- lookup_lad_brc_ltla21 |>
  right_join(boundaries_ltla21, by = "ltla21_code") |>
  filter(str_detect(ltla21_code, "^E"))

brc_tc_sf <- lookup_lad_brc_ltla21_sf |>
  group_by(TacticalCell) |>
  summarise(geometry = st_union(geometry)) |>
  st_as_sf(crs = st_crs(boundaries_ltla21))

ggplot() +
  geom_sf(data = brc_tc_sf, colour = "blue", alpha = 0) +
  geom_sf(data = vcsep_region_boundaries_old, colour = "red", alpha = 0)
# VCSEP regions & BRC Tactical cells the same

# VCSEP region changes ----
# Becky Maynar stated on 21/03/22 that Thames Valley area has moved into SE region for VCSEP regions

# http://www.thamesvalleylrf.org.uk/useful-links/publications/risk-register/the-thames-valley-area.ashx
# The Thames Valley comprises a geographical area covering the unitary local
# authorities of Berkshire and Milton Keynes, and the county and district local authorities of Buckinghamshire and Oxfordshire.

# https://en.wikipedia.org/wiki/Berkshire_County_Council#:~:text=The%20council%20was%20abolished%2C%20and,Bracknell%20Forest%2C%20Reading%20and%20Slough.https://en.wikipedia.org/wiki/Berkshire_County_Council#:~:text=The%20council%20was%20abolished%2C%20and,Bracknell%20Forest%2C%20Reading%20and%20Slough.
# Berkshire county council: The council was abolished, and the ceremonial county is
# now governed by the six unitary authorities: West Berkshire, Windsor and Maidenhead, Wokingham, Bracknell Forest, Reading and Slough.
berkshire_ltlas <- c("West Berkshire", "Windsor and Maidenhead", "Wokingham", "Bracknell Forest", "Reading", "Slough")
thames_valley_one_tier <- c("Milton Keynes")
thames_valley_two_tier <- c("Oxfordshire", "Buckinghamshire")

thames_valley <- c(berkshire_ltlas, thames_valley_one_tier, thames_valley_two_tier)

thames_valley_ltla_utla <- geographr::lookup_ltla21_utla21 |>
  filter(utla21_name %in% c(thames_valley))

# Naming of BRC Tactical Cells & VCSEP regions slightly different
lookup_lad_vcsep_ltla21 <- lookup_lad_brc_ltla21_sf |>
  mutate(vcsep_region = if_else(ltla21_code %in% thames_valley_ltla_utla$ltla21_code, "South East", TacticalCell)) |>
  select(-TacticalCell) |>
  mutate(vcsep_region = case_when(
    vcsep_region == 'South and the Channel Islands' ~ 'South West',
    vcsep_region == 'Central' ~ 'Midlands & East',
    TRUE ~ vcsep_region
  ))

vcsep_region_boundaries_new <- lookup_lad_vcsep_ltla21 |> 
  group_by(vcsep_region) |>
  summarise(geometry = st_union(geometry)) |>
  st_as_sf(crs = st_crs(boundaries_ltla21))

thames_valley_boundary <- thames_valley_ltla_utla |> 
  left_join(boundaries_ltla21, by = "ltla21_code") |>
  summarise(geometry = st_union(geometry)) |>
  st_as_sf(crs = st_crs(boundaries_ltla21))

vcsep_region_boundaries_old |>
  ggplot() +
  geom_sf(colour = "blue") +
  geom_sf_label(aes(label = EPAreaNM)) +
  geom_sf(data = thames_valley_boundary, fill = "red", alpha = 0.2)

vcsep_region_boundaries_old |>
  ggplot() +
  geom_sf(colour = "blue") +
  geom_sf_label(aes(label = EPAreaNM))

vcsep_region_boundaries_new |>
  ggplot() +
  geom_sf(colour = "red") +
  geom_sf_label(aes(label = vcsep_region))

# Save updated boundaries & lookups
vcsep_region_boundaries_new |>
  st_write("vcsep_regions_updated/vcsep_regions_updated.geojson")

lookup_lad_vcsep_ltla21 |>
  select(-geometry) |>
  write_csv("vcsep_regions_updated/vcsep_regions_updated_ltlas.csv")
