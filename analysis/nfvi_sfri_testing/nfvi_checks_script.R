library(readxl)
library(dplyr)
library(compositr)
library(arsenal) # for comparedf

# Data downloaded https://www.climatejust.org.uk/map
# 'Download' button on left hand side of page
# 'Excel format' -> 'Neighbourhood Flood Vulnerability Index (NFVI) and Social Flood Risk Index (SFRI) data'
older_raw_data <- read_excel("data/Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_March2018.xlsx",
  sheet = "Data"
)
# 'Revised August 2018 for improved SFRI mapping (see Excel notes)'
raw_data <- read_excel("data/Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_August2018.xlsx",
  sheet = "Data"
)

data_nfvi <- raw_data |>
  select(-contains("SFRI"))

older_data_nfvi <- older_raw_data |>
  select(-contains("SFRI"))

comparedf(data_nfvi, older_data_nfvi)
# All NFRI data is same in both datasets

# Checks on supporting variables (e.g. a1, a2) to indicators (age) ----

# Function to standardise columns, add standardised columns together and then standardise resulting column
# Compare against indicator column in the raw data
stand_sum_stand <- function(data, columns, indicator) {
  data |>
    select(code, {{ columns }}) |>
    mutate(across({{ columns }}, standardise)) |>
    rowwise(code) |>
    summarise(stand_sum = sum({{ columns }})) |>
    ungroup() |>
    mutate(stand_sum_stand = standardise(stand_sum)) |>
    left_join(data |>
      select(code, {{ indicator }}),
    by = "code"
    ) |>
    mutate(perct_diff = (stand_sum_stand - {{ indicator }}) / {{ indicator }}) |>
    select(perct_diff) |>
    summary()
}

# Initial checks of each indicator ----

# AGE
data_nfvi |>
  stand_sum_stand(c(a1, a2), AGE)
# Different
# TO CHECK

# HEALTH
data_nfvi |>
  stand_sum_stand(c(h1, h2), HEALTH)
# Some outliers but most around 0
# OK

# INCOME
data_nfvi |>
  stand_sum_stand(c(i1, i2, i3, i4, i5), INCOME)
# Some outliers but most around 0
# OK

# INFO
data_nfvi |>
  stand_sum_stand(c(f1, f2), INFO)
# Some outliers but most around 0
# OK

# LOCAL KNOWLEDGE
data_nfvi |>
  stand_sum_stand(c(k1), LOC_KNOW)
# around 0% out
# OK

# TENURE
data_nfvi |>
  stand_sum_stand(c(t1, t2), TENURE)
# Some outliers but most around 0
# OK

# PHYSICAL MOBILITY
data_nfvi |>
  stand_sum_stand(c(m1, m2, m3), MOBILITY)
# Some outliers but most around 0
# OK

# CRIME
data_nfvi |>
  stand_sum_stand(c(c1), CRIME)
# around 0% out
# OK

# HOUSING CHAR
data_nfvi |>
  stand_sum_stand(c(l1), HOUSE_TYP)
# around 0% out
# OK

# FLOOD EXPERIENCE
data_nfvi |>
  stand_sum_stand(c(e1), FLOOD_EXP)
# around 0% out
# OK

# SERVICE AVAILABILITY
data_nfvi |>
  stand_sum_stand(c(s1, s2, s3, s4), SERVICE)
# Some big outliers (one 541%) but most around 0
# TO CHECK

# SOCIAL NETWORK
data_nfvi |>
  stand_sum_stand(c(n1, n2, n3), SOC_NET)
# Some big outliers (one 368%) but most around 0
# TO OK


# Checks on indicators not aligning ----

# AGE
data_nfvi |>
  stand_sum_stand(c(a1), AGE)
# Looks like AGE indicator is missing underlying variable a2?

data_nfvi |>
  select(code, c(s1, s2, s3, s4)) |>
  mutate(across(c(s1, s2, s3, s4), standardise)) |>
  rowwise(code) |>
  summarise(stand_sum = sum(c(s1, s2, s3, s4))) |>
  ungroup() |>
  mutate(stand_sum_stand = standardise(stand_sum)) |>
  left_join(data |>
    select(code, SERVICE),
  by = "code"
  ) |>
  mutate(perct_diff = (stand_sum_stand - SERVICE) / SERVICE) |>
  arrange(perct_diff)


# Check if adding in NI data for age and health make closer
# data from https://www.nomisweb.co.uk/census/2011/ks102uk
ni_a1_a2 <- read_csv("data/ni_census_age.csv") |>
  filter(date == 2011) |>
  mutate(
    a2 = (`Age: Age 75 to 84; Rural Urban: Total; measures: Value` +
      `Age: Age 85 to 89; Rural Urban: Total; measures: Value` +
      `Age: Age 90 and over; Rural Urban: Total; measures: Value`) /
      `Age: All usual residents; Rural Urban: Total; measures: Value`,
    a1 = `Age: Age 0 to 4; Rural Urban: Total; measures: Value` / `Age: All usual residents; Rural Urban: Total; measures: Value`
  ) |>
  select(
    name = geography,
    code = `geography code`,
    a1,
    a2
  )

# https://www.nisra.gov.uk/publications/2011-census-table-lookups
# Disability table QS303NI

# Stack the age and health columns for NI onto the other data
data_nfvi_add_ni <- data_nfvi |>
  select(code, c(a1, a2, h1, h2)) |>
  bind_rows(
    ni_age_data |>
      select(code, a1, a2)
  )


# TO FINISH

# Checks on indicators (age) to domains (e.g. susceptibility) ----

# Susceptibility -----

# Try indicators
suscept_indicators <- data_nfvi |>
  select(code, AGE, HEALTH) |>
  # already z-scores so don't need to standardise
  rowwise(code) |>
  summarise(sucept_calc = sum(c(AGE, HEALTH))) |>
  ungroup() |>
  mutate(stand_sum_stand = standardise(sucept_calc)) |>
  left_join(data_nfvi |>
    select(code, NVFI_SUS),
  by = "code"
  ) |>
  mutate(perct_diff = (stand_sum_stand - NVFI_SUS) / NVFI_SUS)
# No match

# Try underlying variables
suscept_underlying <- data_nfvi |>
  select(code, a1, a2, h1, h2) |>
  mutate(across(c(a1, a2, h1, h2), standardise)) |>
  rowwise(code) |>
  summarise(stand_sum = sum(c(a1, a2, h1, h2))) |>
  ungroup() |>
  mutate(stand_sum_stand = standardise(stand_sum)) |>
  left_join(data_nfvi |>
    select(code, NVFI_SUS),
  by = "code"
  ) |>
  mutate(perct_diff = (stand_sum_stand - NVFI_SUS) / NVFI_SUS)
# Closer but not exact (some outliers)

suscept_underlying |>
  select(perct_diff) |>
  summary()

suscept_underlying |>
  arrange(perct_diff)

# Ability to recover ------
recover_underlying_variables <- c("i1", "i2", "i3", "i4", "i5", "f1", "f2", "m1", "m2", "m3")

ability_recover_underlying <- data_nfvi |>
  select(code, all_of(recover_underlying_variables)) |>
  mutate(across(all_of(recover_underlying_variables), standardise)) |>
  rowwise(code) |>
  summarise(stand_sum = sum(c_across(all_of(recover_underlying_variables)))) |>
  ungroup() |>
  mutate(stand_sum_stand = standardise(stand_sum)) |>
  left_join(data_nfvi |>
    select(code, NVFI_REC),
  by = "code"
  ) |>
  mutate(perct_diff = (stand_sum_stand - NVFI_REC) / NVFI_REC)
# No match

recover_indicators <- c("INCOME", "INFO", "MOBILITY")

ability_recover_indicators <- data_nfvi |>
  select(code, all_of(recover_indicators)) |>
  rowwise(code) |>
  # already standardised so don't need to standardise
  summarise(stand_sum = sum(c_across(all_of(recover_indicators)))) |>
  ungroup() |>
  mutate(stand_sum_stand = standardise(stand_sum)) |>
  left_join(data_nfvi |>
    select(code, NVFI_REC),
  by = "code"
  ) |>
  mutate(perct_diff = (stand_sum_stand - NVFI_REC) / NVFI_REC)
# No match

# Ability to prepare ------
prepare_underlying_variables <- c("i1", "i2", "i3", "i4", "i5", "f1", "f2", "k1", "t1", "t2")

ability_prepare_underlying <- data_nfvi |>
  select(code, all_of(prepare_underlying_variables)) |>
  mutate(across(all_of(prepare_underlying_variables), standardise)) |>
  rowwise(code) |>
  summarise(stand_sum = sum(c_across(all_of(prepare_underlying_variables)))) |>
  ungroup() |>
  mutate(stand_sum_stand = standardise(stand_sum)) |>
  left_join(data_nfvi |>
              select(code, NVFI_REC),
            by = "code"
  ) |>
  mutate(perct_diff = (stand_sum_stand - NVFI_REC) / NVFI_REC)
# No match

prepare_indicators <- c("INCOME", "INFO", "MOBILITY")

ability_prepare_indicators <- data_nfvi |>
  select(code, all_of(prepare_indicators)) |>
  rowwise(code) |>
  # already standardised so don't need to standardise
  summarise(stand_sum = sum(c_across(all_of(prepare_indicators)))) |>
  ungroup() |>
  mutate(stand_sum_stand = standardise(stand_sum)) |>
  left_join(data_nfvi |>
              select(code, NVFI_REC),
            by = "code"
  ) |>
  mutate(perct_diff = (stand_sum_stand - NVFI_REC) / NVFI_REC)
# No match

