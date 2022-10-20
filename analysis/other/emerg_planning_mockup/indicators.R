library(geographr)
library(tidyverse)
library(readxl)
library(httr)
library(janitor)

source("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/R/utils.R")

# Elderly - LSOA
elderly <- 
  population_lsoa |> 
  rowwise() |> 
  mutate(elderly = sum(c_across(`70`:`90+`))) |>
  ungroup() |> 
  mutate(prop_pop_elderly = elderly / total_population) |> 
  select(lsoa_code, total_population, elderly, prop_pop_elderly)


# Fuel poverty - LSOA
# Source: https://www.gov.uk/government/statistics/sub-regional-fuel-poverty-data-2021
tf <- download_file("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/981910/2021-sub-regional-fuel-poverty-tables.xlsx"
  ,".xlsx")

fuel_poverty <- 
  read_excel(
    tf,
    sheet = "Table 3",  # LSOA-level
    skip = 2
  )

# Remove non-data rows
fuel_poverty <- 
  fuel_poverty |> 
  clean_names() |>
  filter(str_detect(lsoa_code, "E[0-9]+")) |> 
  select(
    lsoa_code,
    num_house = number_of_households1,
    num_house_fuel_pov = number_of_households_in_fuel_poverty1
  ) |>
  mutate(prop_house_fuel_pov = num_house_fuel_pov / num_house)

# Select only lsoa_code and indicator column
elderly_ind <- elderly |>
  select(lsoa_code, prop_pop_elderly)

fuel_poverty_ind <- fuel_poverty |>
  select(lsoa_code, prop_house_fuel_pov )

# Combine indicators 
indicators <- elderly_ind |>
  left_join(fuel_poverty_ind, by = "lsoa_code") 

# Save data
indicators |>
  write_feather("data/indicators.feather")
