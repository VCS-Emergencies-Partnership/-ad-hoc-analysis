# Load packages
library(tidyverse)
library(geographr)
library(sf)
library(readxl)
library(shiny)
library(shinyWidgets)
library(janitor)
library(DT)
library(leaflet)

# Load in data
raw_data <- read_excel("data/mock_capability_mapping_data.xlsx", 
                       sheet = "capability_mapping", skip = 1)

# Fix that header topics using merged cells
categories_names <- read_excel("data/mock_capability_mapping_data.xlsx", 
                            sheet = "capability_mapping") |>
  colnames() %>%
  as_tibble() %>%
  rename(capability_category = value) |>
  mutate(capability_category = str_replace_all(capability_category, "...[0-9]+", NA_character_)) |>
  fill(capability_category, .direction = "down") |>
  drop_na()

sub_categories_names <- raw_data %>%
  select(!"Partner":"Provide DBS checked volunteers") |>
  colnames() %>%
  as_tibble() |>
  rename(capability_sub_category = value)
  
category_lookup <- bind_cols(categories_names, sub_categories_names) 
  

# Split rows when more than one LA per org then pivot
tidy_data <- raw_data |>
  separate_rows("Local Authorities", sep =", ") |> 
  pivot_longer(!"Partner":"Provide DBS checked volunteers", 
               names_to = "capability_sub_category",
               values_to = "response_type") |>
  left_join(category_lookup, by = "capability_sub_category") %>%
  janitor::clean_names()






