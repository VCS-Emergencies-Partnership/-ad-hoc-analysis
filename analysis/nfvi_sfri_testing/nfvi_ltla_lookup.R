# This script looks to replicate the NFVI rankings on last column of table Table 4‐4 on page 50 of report below (omitting LAs in Northern Ireland)
# http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/sayers_2017_-_present_and_future_flood_vulnerability_risk_and_disadvantage_-_final_report_-_uploaded_05june2017_printed_-_stan.pdf

library(readxl)
library(dplyr)
library(geographr)
library(stringr)
library(demographr)
library(compositr)

# Data downloaded https://www.climatejust.org.uk/map
# 'Download' button on left hand side of page
# 'Excel format' -> 'Neighbourhood Flood Vulnerability Index (NFVI) and Social Flood Risk Index (SFRI) data' 
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

# LSOA/DZ to LTLA lookups
scotland_dz_ltla <- lookup_dz11_iz11_ltla20 |>
  select(code = dz11_code, ltla_name = ltla20_name, ltla_code = ltla20_code)

eng_wales_lsoa_ltla <- lookup_lsoa11_ltla21 |>
  select(code = lsoa11_code, ltla_name = ltla21_name, ltla_code = ltla21_code)

dz_lsoa_ltla_lookup <- scotland_dz_ltla |>
  bind_rows(eng_wales_lsoa_ltla) |>
  mutate(country = case_when(
    str_detect(ltla_code, "^S") ~ "Scotland",
    str_detect(ltla_code, "^E") ~ "England",
    str_detect(ltla_code, "^W") ~ "Wales",
    TRUE ~ NA_character_
  ))

# LSOA/DZ populations at 2017 ----
# England & Wales
eng_wales_lsoa_pop_2017 <- population17_lsoa11  |>
  select(code = lsoa11_code, 
         total_population)

# Scotland
# https://webarchive.nrscotland.gov.uk/web/20210313181745/https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/mid-2017/detailed-data-zone-tables
tf <- download_file("https://webarchive.nrscotland.gov.uk/web/20210313181745/https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-17/sape-17-persons.xlsx",
                                  ".xlsx")

tf |>
  unzip(exdir = tempdir())

scot_dz_pop_2017 <- read_excel(tf, skip = 5) |>
  select(code = DataZone2011Code,
         total_population = `...4`)

eng_wales_scot_pop_2017 <- eng_wales_lsoa_pop_2017 |>
  bind_rows(scot_dz_pop_2017)



raw_data |>
  left_join(dz_lsoa_ltla_lookup, by = "code") |>
  group_by(ltla_name) |>
  summarise(NVFI = sum(NVFI)) |>
  arrange(desc(NVFI))
# does not match NFVI rankings on last column of table Table 4‐4 on page 50 of report below (omitting LAs in Northern Ireland)
# http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/sayers_2017_-_present_and_future_flood_vulnerability_risk_and_disadvantage_-_final_report_-_uploaded_05june2017_printed_-_stan.pdf


# states is the 'NFVI calculated as the population weighted average across all neighbourhoods exposed to flooding'
# try to replicate
raw_data |>
  # only neighbourhoods exposed to flooding
  filter(SFRIPFCG != 0 & SFRIPSWG != 0) |>
  left_join(dz_lsoa_ltla_lookup, by = "code") |>
  select(code, NVFI, ltla_name, ltla_code) |>
  left_join(eng_wales_scot_pop_2017,  by = "code") |>
  mutate(nfvi_mult_pop = NVFI * total_population) |>
  group_by(ltla_name, ltla_code) |>
  summarise(nfvi_pop_weighted = sum(nfvi_mult_pop)/ sum(total_population)) |>
  ungroup() |>
  arrange(desc(nfvi_pop_weighted)) |>
  print(n = 20)
# does not exactly match NFVI rankings on last column of table Table 4‐4 on page 50 of report below (omitting LAs in Northern Ireland)
# http://www.sayersandpartners.co.uk/uploads/6/2/0/9/6209349/sayers_2017_-_present_and_future_flood_vulnerability_risk_and_disadvantage_-_final_report_-_uploaded_05june2017_printed_-_stan.pdf

