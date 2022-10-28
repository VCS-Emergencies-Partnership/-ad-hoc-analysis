# This scripts looks to run investigations on the NFVI data on the ClimtaeJust site with
# aim of replicating the values at each step of the build process of NFVI
# Build process on p.15 of http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/appendix_b_neighbourhood_flood_vulnerability_index_-_final_-_uploaded_4june2017_revised_and_upload_280617_-_minor_corrections.pdf

library(readxl)
library(readr)
library(dplyr)
library(compositr)
library(arsenal) # for comparedf
library(skimr)
library(ggplot2)
library(stringr)

# Data downloaded https://www.climatejust.org.uk/map
# 'Download' button on left hand side of page
# 'Excel format' -> 'Neighbourhood Flood Vulnerability Index (nfvi) and Social Flood Risk Index (SFRI) data'
tf <- download_file("http://maps.humanities.manchester.ac.uk/cj/2018/Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_March2018.xlsx", ".xlsx")

tf |>
  unzip(exdir = tempdir())

older_raw_data <- read_excel(list.files(
  tempdir(),
  pattern = "Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_August2018.xlsx",
  full.names = TRUE
),
sheet = "Data"
) 


# 'Revised August 2018 for improved SFRI mapping (see Excel notes)'
tf <- download_file("http://maps.humanities.manchester.ac.uk/cj/2018/Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_August2018.zip", ".zip")

tf |>
  unzip(exdir = tempdir())

raw_data <- read_excel(list.files(
  tempdir(),
  pattern = "Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_August2018.xlsx",
  full.names = TRUE
),
sheet = "Data"
) 

# Check if NFVI is same in old and new datasets -----
data_nfvi <- raw_data |>
  select(-contains("SFRI"))

older_data_nfvi <- older_raw_data |>
  select(-contains("SFRI"))

comparedf(data_nfvi, older_data_nfvi)
# All NFVI data is same in both datasets

# In data columns are labeled 'NVFI' but should be 'NFVI' as is named in the reports. 
colnames(data_nfvi) <- str_replace_all(colnames(data_nfvi), "NVFI", "NFVI")
  
# Checks on supporting variables (e.g. a1, a2) to indicators (age) ----

#' Function to standardise a set of standardised and summed columns
#' #'
#' @param data The data
#' @param id_columns The id columns
#' @param calc_columns The columns to perform calculations across
#'
standarise_summed_standarise_cols <- function(data, columns, indicator) {
  data |>
    select(code, {{ columns }}) |>
    mutate(across({{ columns }}, standardise)) |>
    rowwise(code) |>
    summarise(stand_sum = sum({{ columns }})) |>
    ungroup() |>
    mutate(standarise_summed_standarise_cols = standardise(stand_sum)) |>
    left_join(data |>
      select(code, {{ indicator }}),
    by = "code")
}

# Initial checks of each indicator ----

# Indicators which seem to match closely ----

# AGE
data_nfvi |>
  standarise_summed_standarise_cols(c(a1, a2), AGE)
# Different
# TO CHECK

# AGE
data_nfvi |>
  standarise_summed_standarise_cols(c(a1), AGE)
# Looks like AGE indicator is missing underlying variable a2?

# HEALTH
data_nfvi |>
  standarise_summed_standarise_cols(c(h1, h2), HEALTH) 

data_nfvi |>
  standarise_summed_standarise_cols(c(h1, h2), HEALTH) |>
  mutate(diff = standarise_summed_standarise_cols - HEALTH) |>
  ggplot(aes(x = diff)) +
  geom_density()
# OK

# INFO
data_nfvi |>
  standarise_summed_standarise_cols(c(f1, f2), INFO)

data_nfvi |>
  standarise_summed_standarise_cols(c(f1, f2), INFO) |>
  mutate(diff = standarise_summed_standarise_cols - INFO) |>
  ggplot(aes(x = diff)) +
  geom_density()
# OK

# LOCAL KNOWLEDGE
data_nfvi |>
  standarise_summed_standarise_cols(c(k1), LOC_KNOW)

data_nfvi |>
  standarise_summed_standarise_cols(c(k1), LOC_KNOW) |>
  mutate(diff = standarise_summed_standarise_cols - LOC_KNOW) |>
  ggplot(aes(x = diff)) +
  geom_density()
# OK

# TENURE
data_nfvi |>
  standarise_summed_standarise_cols(c(t1, t2), TENURE)

data_nfvi |>
  standarise_summed_standarise_cols(c(t1, t2), TENURE) |>
  mutate(diff = standarise_summed_standarise_cols - TENURE) |>
  ggplot(aes(x = diff)) +
  geom_density()
# OK

# HOUSING CHAR
data_nfvi |>
  standarise_summed_standarise_cols(c(l1), HOUSE_TYP)

data_nfvi |>
  standarise_summed_standarise_cols(c(l1), HOUSE_TYP) |>
  mutate(diff = standarise_summed_standarise_cols - HOUSE_TYP) |>
  ggplot(aes(x = diff)) +
  geom_density()
# OK

# FLOOD EXPERIENCE
data_nfvi |>
  standarise_summed_standarise_cols(c(e1), FLOOD_EXP)

data_nfvi |>
  standarise_summed_standarise_cols(c(e1), FLOOD_EXP) |>
  mutate(diff = standarise_summed_standarise_cols - FLOOD_EXP) |>
  ggplot(aes(x = diff)) +
  geom_density()
# OK

# SOCIAL NETWORK
data_nfvi |>
  standarise_summed_standarise_cols(c(n1, n2, n3), SOC_NET)

data_nfvi |>
  standarise_summed_standarise_cols(c(n1, n2, n3), SOC_NET) |>
  mutate(diff = standarise_summed_standarise_cols - SOC_NET) |>
  ggplot(aes(x = diff)) +
  geom_density()

# PHYSICAL MOBILITY
data_nfvi |>
  standarise_summed_standarise_cols(c(m1, m2, m3), MOBILITY)

data_nfvi |>
  standarise_summed_standarise_cols(c(m1, m2, m3), MOBILITY) |>
  mutate(diff = standarise_summed_standarise_cols - MOBILITY) |>
  ggplot(aes(x = diff)) +
  geom_density()

# CRIME
data_nfvi |>
  standarise_summed_standarise_cols(c(c1), CRIME)

data_nfvi |>
  standarise_summed_standarise_cols(c(c1), CRIME) |>
  mutate(diff = standarise_summed_standarise_cols - CRIME)

# Indicators which seem to not match as well ----

# SERVICE AVAILABILITY
data_nfvi |>
  standarise_summed_standarise_cols(c(s1, s2, s3, s4), SERVICE)

data_nfvi |>
  standarise_summed_standarise_cols(c(s1, s2, s3, s4), SERVICE) |>
  mutate(diff = standarise_summed_standarise_cols - SERVICE) |>
  ggplot(aes(x = diff)) +
  geom_density()
# Some big outliers but most around 0

# INCOME
data_nfvi |>
  standarise_summed_standarise_cols(c(i1, i2, i3, i4, i5), INCOME)

data_nfvi |>
  standarise_summed_standarise_cols(c(i1, i2, i3, i4, i5), INCOME)|>
  mutate(diff = standarise_summed_standarise_cols - INCOME) |>
  ggplot(aes(x = diff)) +
  geom_density()
# Some outliers but most around 0
# OK

# Checks on indicators (age) to domains (e.g. susceptibility) ----

# Susceptibility -----

# Try indicators
suscept_indicators <- data_nfvi |>
  select(code, AGE, HEALTH) |>
  # already z-scores so don't need to standardise
  rowwise(code) |>
  summarise(sucept_calc = sum(c(AGE, HEALTH))) |>
  ungroup() |>
  mutate(standarise_summed_standarise_cols = standardise(sucept_calc)) |>
  left_join(data_nfvi |>
    select(code, NFVI_SUS),
  by = "code"
  ) |>
  mutate(abs_diff = abs(standarise_summed_standarise_cols - NFVI_SUS)) |>
  arrange(desc(abs_diff))

suscept_indicators
# No match

# Try underlying variables
suscept_underlying <- data_nfvi |>
  select(code, a1, a2, h1, h2) |>
  mutate(across(c(a1, a2, h1, h2), standardise)) |>
  rowwise(code) |>
  summarise(stand_sum = sum(c(a1, a2, h1, h2))) |>
  ungroup() |>
  mutate(standarise_summed_standarise_cols = standardise(stand_sum)) |>
  left_join(data_nfvi |>
    select(code, NFVI_SUS),
  by = "code"
  ) |>
  mutate(abs_diff = abs(standarise_summed_standarise_cols - NFVI_SUS)) |>
  arrange(desc(abs_diff))

suscept_underlying

########################################################################################################################################################################
# Check if adding in NI data for age and health make closer ----
# Tables based on tables on p.8 of http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/appendix_b_neighbourhood_flood_vulnerability_index_-_final_-_uploaded_4june2017_revised_and_upload_280617_-_minor_corrections.pdf

# Age -----
# Data from https://www.nomisweb.co.uk/census/2011/ks102uk
ni_a1_a2 <- read_csv("data/ni_census_age.csv") |>
  filter(date == 2011) |>
  mutate(
    a2 = ((`Age: Age 75 to 84; Rural Urban: Total; measures: Value` +
            `Age: Age 85 to 89; Rural Urban: Total; measures: Value` +
            `Age: Age 90 and over; Rural Urban: Total; measures: Value`) /
      `Age: All usual residents; Rural Urban: Total; measures: Value`) * 100,
    a1 = (`Age: Age 0 to 4; Rural Urban: Total; measures: Value` / `Age: All usual residents; Rural Urban: Total; measures: Value`) * 100
  ) |>
  select(
    code = `geography code`,
    a1,
    a2
  )

data_nfvi |>
  select(code, a1, a2) |>
  bind_rows(ni_a1_a2) |>
  mutate(across(a1:a2, standardise)) |>
  rowwise(code) |>
  summarise(stand_sum = sum(a1:a2)) |>
  ungroup() |>
  mutate(standarise_summed_standarise_cols = standardise(stand_sum)) |>
  left_join(data_nfvi |>
              select(code, AGE),
            by = "code")
  
data_nfvi |>
  select(code, a1) |>
  bind_rows(ni_a1_a2 |>
              select(-a2)) |>
  mutate(age_calc = standardise(a1)) |>
  left_join(data_nfvi |>
              select(code, AGE),
            by = "code")

# h1: Disability / peope in ill health (% people whose day to day acvtivites are limited) -------
ni_soa_total_pop <- read_csv("data/ni_census_age.csv") |>
  filter(date == 2011) |>
  select(code = `geography code`,
         total_population = `Age: All usual residents; Rural Urban: Total; measures: Value`)

# Table finder: https://www.nomisweb.co.uk/census/2011/data_finder
# Source: https://www.nomisweb.co.uk/census/2011/ks301uk
ni_h1 <- read_csv("data/ni_disability.csv") |>
  filter(date == 2011) |>
  select(code = `geography code`, 
         `disability/health/care: Day-to-day activities limited a lot; measures: Value`, 
         `disability/health/care: Day-to-day activities limited a little; measures: Value`) |>
  left_join(ni_soa_total_pop) |>
  mutate(
    h1 = ((`disability/health/care: Day-to-day activities limited a lot; measures: Value` +
             `disability/health/care: Day-to-day activities limited a lot; measures: Value`) / total_population) * 100) |>
  select(
    code,
    h1
  )

# h2: % households with at least one person with long term limiting illness ----
# Unable to complete due to not being able to find this data for NI
# Only England and Wales: https://www.nomisweb.co.uk/census/2011/ks106ew


########################################################################################################################################################################

# Ability to prepare ------
prepare_indicators <- c("INCOME", "INFO", "MOBILITY")
ability_prepare_indicators <- data_nfvi |>
  select(code, all_of(prepare_indicators)) |>
  rowwise(code) |>
  # already standardised so don't need to standardise
  summarise(stand_sum = sum(c_across(all_of(prepare_indicators)))) |>
  ungroup() |>
  mutate(standarise_summed_standarise_cols = standardise(stand_sum)) |>
  left_join(data_nfvi |>
              select(code, NFVI_PRP),
            by = "code"
  ) |>
  mutate(abs_diff = (standarise_summed_standarise_cols - NFVI_PRP))

ability_prepare_indicators

prepare_underlying_variables <- c("i1", "i2", "i3", "i4", "i5", "f1", "f2", "k1", "t1", "t2")
ability_prepare_underlying <- data_nfvi |>
  select(code, all_of(prepare_underlying_variables)) |>
  mutate(across(all_of(prepare_underlying_variables), standardise)) |>
  rowwise(code) |>
  summarise(stand_sum = sum(c_across(all_of(prepare_underlying_variables)))) |>
  ungroup() |>
  mutate(standarise_summed_standarise_cols = standardise(stand_sum)) |>
  left_join(data_nfvi |>
              select(code, NFVI_PRP),
            by = "code"
  ) |>
  mutate(abs_diff = abs(standarise_summed_standarise_cols - NFVI_PRP)) |>
  arrange(desc(abs_diff))

ability_prepare_underlying
# No match

# Ability to recover ------
recover_underlying_variables <- c("i1", "i2", "i3", "i4", "i5", "f1", "f2", "m1", "m2", "m3")

ability_recover_underlying <- data_nfvi |>
  select(code, all_of(recover_underlying_variables)) |>
  mutate(across(all_of(recover_underlying_variables), standardise)) |>
  rowwise(code) |>
  summarise(stand_sum = sum(c_across(all_of(recover_underlying_variables)))) |>
  ungroup() |>
  mutate(standarise_summed_standarise_cols = standardise(stand_sum)) |>
  left_join(data_nfvi |>
              select(code, NFVI_REC),
            by = "code"
  ) |>
  mutate(abs_diff = abs(standarise_summed_standarise_cols - NFVI_REC)) |>
  arrange(desc(abs_diff))

ability_recover_underlying
# No match

# Community support ------
community_underlying_variables <- c("l1", "e1", "s1", "s2", "s3", "s4", "n1", "n2", "n3")

community_underlying <- data_nfvi |>
  # In methodology  p.17 http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/appendix_b_neighbourhood_flood_vulnerability_index_-_final_-_uploaded_4june2017_revised_and_upload_280617_-_minor_corrections.pdf
  # weighting of e1 & n3 is to be negative
  mutate(e1 = -1 * e1,
         n3 = -1 * n3) |>
  select(code, all_of(community_underlying_variables)) |>
  mutate(across(all_of(community_underlying_variables), standardise)) |>
  rowwise(code) |>
  summarise(stand_sum = sum(c_across(all_of(community_underlying_variables)))) |>
  ungroup() |>
  mutate(standarise_summed_standarise_cols = standardise(stand_sum)) |>
  left_join(data_nfvi |>
              select(code, NFVI_COM),
            by = "code"
  ) |>
  mutate(abs_diff = abs(standarise_summed_standarise_cols - NFVI_COM)) |>
  arrange(desc(abs_diff))

community_underlying
# No match


# Trying to create NFVI from domain columns ------
# Calculate from domains 
nfvi_calc <- data_nfvi |>
  rowwise(code) |>
  summarise(sum_stand_domains = sum(c_across(all_of(c("NFVI_SUS","NFVI_PRP", "NFVI_RES",  "NFVI_REC", "NFVI_COM" ))))) |>
  ungroup() |>
  mutate(calc_nfvi = standardise(sum_stand_domains)) |>
  left_join(data_nfvi |>
              select(code, NFVI),
            by = "code"
  ) |>
  mutate(abs_diff = abs(calc_nfvi - NFVI)) |>
  arrange(desc(abs_diff))

nfvi_calc 

nfvi_calc |>
  skim()

# Calculate from underlying indicators
# using weighting on page 17 http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/appendix_b_neighbourhood_flood_vulnerability_index_-_final_-_uploaded_4june2017_revised_and_upload_280617_-_minor_corrections.pdf
data_nfvi |>
  select(code, a1:n3) |>
  mutate(across(where(is.numeric), standardise)) |>
  mutate(calc_nfvi_underlying =
          0.05 * a1 + 
           0.05 * a2 + 
           0.05 * h1 + 
           0.05 * h2 +
           0.06 * i1 +
           0.06 * i2 +
           0.06 * i3 +
           0.06 * i4 +
           0.06 * i5 +
           0.06 * f1 +
           0.06 * f2 +
           0.04 * k1 +
           0.02 * t1 +
           0.02 * t2 +
           0.04 * m1 +
           0.04 * m2 +
           0.04 * m3 +
           0.02 * c1 +
           0.02 * l1 +
           -0.02 * e1 +
           0.02 * s1 +
           0.02 * s3 +
           0.02 * s4 + 
           0.02 * n1 +
           0.02 * n2 + 
           -0.02 * n3) |>
  select(code, calc_nfvi_underlying) |>
  left_join(data_nfvi |>
              select(code, nfvi),
            by = "code"
  )

