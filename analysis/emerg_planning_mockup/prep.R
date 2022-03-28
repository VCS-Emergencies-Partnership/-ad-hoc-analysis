# Pulls DEFRA Flood warning API 

# Load packages
library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(sf)


# Call DEFRA flood API
query <- "http://environment.data.gov.uk/flood-monitoring/id/floods/"

#Clean JSON to dataframe
floods <- GET(query) |>
  content(type = "text", encoding = "UTF-8") |>
  fromJSON() |>
  pluck("items") |>
  as_tibble() |>
  unnest(floodArea, .names_repair = "minimal") |>
  clean_names()

floods_severity <- floods |>
  select(flood_area_id, severity_level)

floods_spatial <-  map_dfr(floods$polygon, st_read) |>
  select(flood_area_id = FWS_TACODE, geometry)

floods_spatial_level <- floods_spatial|>
  left_join(floods_severity, by = c("flood_area_id"))

# Save data 
floods_spatial_level |>
  st_write("data/floods_spatial.geojson")



  