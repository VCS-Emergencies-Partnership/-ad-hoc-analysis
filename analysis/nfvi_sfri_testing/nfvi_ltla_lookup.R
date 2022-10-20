library(readxl)
library(dplyr)
library(geographr)
library(stringr)
library(demographr)

# Data downloaded https://www.climatejust.org.uk/map
# 'Download' button on left hand side of page
# 'Excel format' -> 'Neighbourhood Flood Vulnerability Index (NFVI) and Social Flood Risk Index (SFRI) data' 
raw_data <- read_excel("data/Climate_Just_2017_Master_Excel_Sheet_NFVI_and_SFRI_August2018.xlsx",
                       sheet = "Data") 

scotland_dz_ltla <- lookup_dz11_iz11_ltla20 |>
  select(code = dz11_code, ltla_name = ltla20_name, ltla_code = ltla20_code)

eng_wales_lsoa_ltla <- lookup_lsoa11_ltla21 |>
  select(code = lsoa11_code, ltla_name = ltla21_name, ltla_code = ltla21_code)

ni_soa_lgd <- lookup_sa11_soa11_lgd18 |>
  select(code = soa11_code, ltla_name = lgd18_name, ltla_code = lgd18_code)

dz_lsoa_soa_ltla_lookup <- scotland_dz_ltla |>
  bind_rows(eng_wales_lsoa_ltla) |>
  bind_rows(ni_soa_lgd) |>
  mutate(country = case_when(
    str_detect(ltla_code, "^S") ~ "Scotland",
    str_detect(ltla_code, "^E") ~ "England",
    str_detect(ltla_code, "^W") ~ "Wales",
    str_detect(ltla_code, "^N") ~ "NI",
    TRUE ~ NA_character_
  ))

raw_data |>
  left_join(dz_lsoa_soa_ltla_lookup, by = "code") |>
  group_by(ltla_name) |>
  summarise(NVFI = sum(NVFI)) |>
  arrange(desc(NVFI))
# does not match NFVI rankings on last column of table Table 4‐4 on page 50 (omitting LAs in Northern Ireland)

# states is the 'NFVI calculated as the population weighted average across all neighbourhoods exposed to flooding'
# try to replicate
raw_data |>
  # only neighbourhoods exposed to flooding
  filter(SFRIPFCG != 0 & SFRIPSWG != 0) |>
  left_join(dz_lsoa_soa_ltla_lookup, by = "code") |>
  select(code, NVFI, ltla_name, ltla_code) |>
  inner_join(
    population19_lsoa11 |>
      select(lsoa11_code, total_population),
    by = c("code" = "lsoa11_code")) |>
  mutate(nfvi_mult_pop = NVFI * total_population) |>
  group_by(ltla_name, ltla_code) |>
  summarise(nfvi_pop_weighted = sum(nfvi_mult_pop)/ sum(total_population)) |>
  arrange(nfvi_pop_weighted)
  
