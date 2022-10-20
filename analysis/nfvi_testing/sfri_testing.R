library(geographr)

# Data downloaded https://www.climatejust.org.uk/map
# 'Download' button on left hand side of page
# 'Excel format' -> 'Neighbourhood Flood Vulnerability Index (NFVI) and Social Flood Risk Index (SFRI) data'
raw <-
  read_sf("analysis/area-planning/data/climate-just-maps/CJ2017_NFVI_SRFI_Full_Dataset_AUGUST_2018.shp")

# Positive scores = more vulnerable/risk
# https://www.climatejust.org.uk/sites/default/files/INFO_Sheet_SFRI.pdf

raw |>
  st_drop_geometry() |>
  distinct(SFRCPFCG)

# From https://www.climatejust.org.uk/sites/default/files/INFO_Sheet_SFRI.pdf
# SFRI = social flood risk index (numeric measure)
# SFRC = social flood risk category (categorised)
# P = Present day (other options are '2' and '4' for 2050s 2 & 4 degrees scenarios)
# FC = Coastal & Fluvial (other option is 'SW' for surface water)
raw |>
  st_drop_geometry() |>
  mutate(id = row_number()) |>
  select(id, SFRIPFCG, SFRCPFCG) |> 
  group_by(SFRCPFCG) |>
  summarise(min_index = min(SFRIPFCG),
            max_index = max(SFRIPFCG), 
            count_lsoas = n()) |>
  arrange(desc(min_index))

raw |>
  st_drop_geometry() |>
  mutate(id = row_number()) |>
  select(id, SFRIPSWG, SFRCPSWG) |> 
  group_by(SFRCPSWG) |>
  summarise(min_index = min(SFRIPSWG),
            max_index = max(SFRIPSWG), 
            count_lsoas = n()) |>
  arrange(desc(min_index))

# Cut offs for categorization not based on quantiles of each column, based on defined cut offs (unsure where cut offs come from)
# Extreme > 100
# 50 < Acute < 100
# 25 < Very high < 50
# 12.5 < High < 25
# 5 < Moderate < 12.5
# 0 < Low < 5
# 0 = No exposed population
# 0 > Exposed, NFVI below the UK mean


glimpse(raw)

# Dividing the SFRI_I by NVFI should give the Expected annual probability of flooding (individual)
raw |>
  st_drop_geometry() |>
  mutate(eai_fc = SFRIPFCI / NVFI,
         eai_sw = SFRIPSWI / NVFI) |>
  select(code, eai_fc, eai_sw)|> 
  summary()
# These are numbers outside range of 0 to 1 so can't be probabilities

# Datazone or LSOA lookup to LTLA 
# JRF report done in 2017 so LTLAs would have been around this time
scotland_dz_ltla <- geographr::lookup_dz11_iz11_ltla20 |>
  select(code = dz11_code, ltla_name = ltla20_name, ltla_code = ltla20_code)

eng_wales_lsoa_ltla <- geographr::lookup_lsoa11_ltla21 |>
  select(code = lsoa11_code, ltla_name = ltla21_name, ltla_code = ltla21_code)

dz_lsoa_ltla_lookup <- scotland_dz_ltla |>
  bind_rows(eng_wales_lsoa_ltla)

# Present day SFRI
sfri_ltla_lookup <- raw |>
  st_drop_geometry() |>
  select(code, NAME, COUNTRY, SFRIPFCG, SFRIPSWG) |>
  left_join(dz_lsoa_ltla_lookup, by = "code")


sfri_ltla_lookup |>
  group_by(ltla_code, ltla_name) |>
  summarise(mean_sw = mean(SFRIPSWG)) |>
  arrange(desc(mean_sw))

sfri_ltla_lookup |>
  group_by(ltla_code, ltla_name) |>
  summarise(mean_fc = mean(SFRIPFCG)) |>
  arrange(desc(mean_fc))

-----------------------
raw |>
  select(code, NAME, COUNTRY, SFRIPFCI) |> 
  ggplot() +
  geom_sf(aes(fill = SFRIPFCI))
