# Pulls DEFRA Flood warning API and counts warnings grouped by severity & returns most recent change time

# Load packages
library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(lubridate)

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
  mutate(time_code_run = Sys.time())

# To investigate relative filepaths in Rprojectsfor cron jobs
wd <- getwd()

flood_counts |>
  saveRDS(paste0(wd, "data_output/flood_counts.rds"))
