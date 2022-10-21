# Pulls DEFRA Flood warning API and counts warnings grouped by severity & returns most recent change time

# Load packages
library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(lubridate)
library(googlesheets4)

# Call DEFRA flood API
query <- "http://environment.data.gov.uk/flood-monitoring/id/floods/"
response <- GET(query)
content <- content(response, type = "text", encoding = "UTF-8")

# Clean JSON to dataframe
floods <- content |>
  fromJSON() |>
  pluck("items") |>
  clean_names()

flood_counts <- floods |>
  filter(severity != "Warning no longer in force") |>
  group_by(severity_level) |>
  summarise(count = n()) |>
  mutate(time_code_run = Sys.time()) |>
  data.frame()


write_sheet(ss = "1nGq6gK3FLIJ_gTlt1D1gOMFzJ6iellyrWBbwHBdKPMQ", 
            sheet = "Sheet1",
            data = flood_counts)



