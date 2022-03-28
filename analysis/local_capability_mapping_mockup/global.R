# Load packages ----
library(tidyverse)
library(geographr)
library(sf)
library(readxl)
library(shiny)
library(shinyWidgets)
library(janitor)
library(DT)
library(leaflet)
library(cicerone)

# Guided tour text
source("guided_tour.R")

# Data loading ----
# Either use the mock regional data (lines 2-22) or core partner real data (25-26)
# Comment out the other

# Mock data (for app on shinyapps.io)
# raw_data <- read_excel("data/mock_capability_mapping_data.xlsx",
#   sheet = "capability_mapping", skip = 1
# )

#Core partner data
raw_data <- read_csv("core_partners/data/core_partners_reformatted.csv",
)


# Data wrangling ----
# Fix that header topics using merged cells
categories_names <- read_excel("data/mock_capability_mapping_data.xlsx",
  sheet = "capability_mapping"
) |>
  colnames() |>
  as_tibble() |>
  rename(capability_category = value) |>
  mutate(capability_category = str_replace_all(capability_category, "...[0-9]+", NA_character_)) |>
  fill(capability_category, .direction = "down") |>
  drop_na()

sub_categories_names <- raw_data |>
  select(!"Partner":"Number of volunteers") |>
  colnames() |>
  as_tibble() |>
  rename(capability_sub_category = value)

category_lookup <- bind_cols(categories_names, sub_categories_names)

# Split rows when more than one LA per org then pivot
tidy_data <- raw_data |>
  separate_rows("Local Authorities", sep = ", ") |>
  pivot_longer(!"Partner":"Number of volunteers",
    names_to = "capability_sub_category",
    values_to = "response_type"
  ) |>
  left_join(category_lookup, by = "capability_sub_category") |>
  janitor::clean_names() |>
  mutate(
    across(specialism_focus:number_of_volunteers, ~replace_na(.x, "Unanswered"))
  )



# Subcategory list for use in the capabilities drop down (multilevel dropdown)
# From https://stackoverflow.com/questions/50924933/r-shiny-selectinput-how-to-search-group-name-label
category_lookup_factors <- category_lookup |>
  mutate(across(everything(), as.factor)) |>
  arrange(capability_sub_category)

multilevel_cat_list <- split(as.list(levels(category_lookup_factors$capability_sub_category)), category_lookup_factors$capability_category)
