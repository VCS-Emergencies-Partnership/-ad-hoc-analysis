# This is to get current data collected from core partners to align with proposed future layout
# needed for the app.

# Load libraries ----
library(tidyverse)
library(readxl)

# Load in core partner data ----
raw_data <- read_excel("core_partners/data/raw/Collated Capability Mapping Profiles.xlsx",
  sheet = "Capability Mapping",
  skip = 1
)

lad_tibble <- read_csv("core_partners/data/lad_list.csv")

lad_vector <- lad_tibble |>
  summarise(lad_name = paste(lad_name, collapse = ", ")) |>
  pull()

# Dropping 2 columns in national data as not categorical enough for use
# Adding an entry since national is all Local Authorities
core_partners <- raw_data |>
  select(-c(
    "Any regional areas NOT supported",
    "Any responses  NOT supported"
  )) |>
  mutate(
    Website = NA,
    Contact = NA,
    .after = "Partner"
  ) |>
  mutate(
    "Local Authorities" = lad_vector,
    .after = "Partner"
  ) |>
  relocate("Any other Capabilities not listed",
    .after = "Specialism / Focus"
  ) |>
  mutate(across(everything(), ~ na_if(str_trim(.), "")))


# Add on volunteer numbers as calculated in core_partners_vol_numbers.R file from
# Charity Commissioner data
vol_numbers_raw <- read_csv("core_partners/data/core_partners_vol_numbers.csv")

vol_numbers_selected <- vol_numbers_raw |>
  select(
    charity_name,
    count_volunteers
  )

# Check not matching names
core_partners |>
  mutate(partner_upper = toupper(Partner)) |>
  anti_join(vol_numbers_selected, by = c("partner_upper" = "charity_name")) |>
  select(Partner)

core_partners_updated <- core_partners |>
  mutate(Partner = recode(Partner,
    "BRC" = "British Red Cross",
    "RVS" = "Royal Volunteering Society"
  ))

core_partners_names <- core_partners_updated  |>
  mutate(partner_upper = toupper(Partner)) |>
  left_join(vol_numbers_selected, by = c("partner_upper" = "charity_name")) |>
  relocate(
    "Number of volunteers" = count_volunteers,
    .after = "Provide DBS checked volunteers"
  ) |>
  select(-partner_upper)

# Save data ----
core_partners_names |>
  write_csv("core_partners/data/core_partners_reformatted.csv")
