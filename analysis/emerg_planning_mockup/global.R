library(shiny)
library(tidyverse)
library(geographr)
library(arrow)
library(leaflet)
library(sf)
library(rmapshaper) #ms_simpify

lookup <- read_feather("data/england_lookup.feather") # large file
lsoa_alerts <- read_feather("data/lsoa_alerts.feather")
warnings <- read_sf("data/warnings.geojson")
ind <- read_feather("data/indicators.feather")

ind_values <- ind |>
  select(-lsoa_code) |>
  colnames()

# ind_sf <- boundaries_lsoa |>
#   left_join(ind, by = "lsoa_code") 
# 
# pal_elderly <- colorNumeric(
#   palette = "magma",
#   domain = ind$prop_pop_elderly)
#  
# ind_sf |>
#   inner_join(chosen, by = "lsoa_code") |>
#   leaflet() |>
#   addProviderTiles(providers$CartoDB.Positron) |>
#   addPolygons(stroke = FALSE,
#               color = ~pal_elderly(prop_pop_elderly),
#               fillOpacity = 1) |>
#   addLegend("bottomright", pal = pal_elderly, values = ~prop_pop_elderly,
#             opacity = 1
#   )