# Number of volunteers for core partners from annual returns
# held by Charity Commissioners data 

# Load packages
library(tidyverse)
library(httr)

# Data source: https://register-of-charities.charitycommission.gov.uk/register/full-register-download 
# 'Annual return part a' & 'overview' data
# Longer term may investigate API https://api-portal.charitycommission.gov.uk/

GET(
  "https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

raw_main <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity.txt",
      full.names = TRUE
    )
  )

GET(
  "https://ccewuksprdoneregsadata1.blob.core.windows.net/data/txt/publicextract.charity_annual_return_parta.zip",
  write_disk(tf <- tempfile(fileext = ".zip"))
)

unzip(tf, exdir = tempdir())

raw_fin <-
  read_tsv(
    list.files(
      tempdir(),
      pattern = "publicextract.charity_annual_return_parta.txt",
      full.names = TRUE
    )
  )


# Multiple rows per some charities due to returns at different dates - take most recent
vol_count <- raw_fin |>
  group_by(organisation_number, registered_charity_number) |>
  filter(ar_received_date == max(ar_received_date)) |>
  ungroup() |>
  select(organisation_number, registered_charity_number, count_volunteers)

vol_count |>
  summarise(not_missing_prop = sum(!is.na(count_volunteers)) / n())

# Join volunteer data to main data table
vol_joined <- raw_main |>
  filter(charity_registration_status == "Registered") |>
  select(charity_name, charity_activities, organisation_number, registered_charity_number) |>
  left_join(vol_count) |>
  select(charity_name, count_volunteers, charity_activities, organisation_number, registered_charity_number)

vol_joined |>
  distinct(registered_charity_number, organisation_number, count_volunteers) |>  
  summarise(vol_info_prop = sum(!is.na(count_volunteers)) / n())
# 71% of charities have volunteer information

# National partners (charity numbers manually taken from websites)
partners <- tribble(
  ~charity_name, ~charity_number,
  "RE:ACT", 1163214,
  "VICTIM SUPPORT", 298028,
  "NCVO", 225922,
  "ROTARY", 1002059,
  "UKCF", 1004630,
  "BITC", 297716,
  "NATIONAL EMERGENCIES TRUST", 1182809,
  "BRITISH RED CROSS", 220949,
  "ROYAL VOLUNTARY SOCIETY", 2520413,
  "VOLUNTEERING MATTERS", 291222,
  "EQUALLY OURS", 1135357
)

vol_partners <- partners  |>
  left_join(vol_count, by = c("charity_number" = "registered_charity_number"))

# May not be representative since e.g. all Rotaries are different registered charities 
# Are other partners similar structure?
# Are these numbers for UK wide operations when we only want England (if possible)?

# Temp fix for Rotary 
rotary_count_volunteers <- raw_main |>
  filter(str_detect(charity_name, "ROTARY")) |>
  distinct(registered_charity_number, organisation_number) |>
  inner_join(vol_joined , by = c("registered_charity_number", "organisation_number")) |>
  summarise(count_volunteers = sum(count_volunteers, na.rm = T)) |>
  pull()

vol_partners_update <- vol_partners |>
  mutate(count_volunteers = ifelse(charity_name == "ROYAL VOLUNTARY SOCIETY", rotary_count_volunteers,count_volunteers))

# Save data ----
vol_partners_update |>
  write_csv("core_partners/data/core_partners_vol_numbers.csv")
  
