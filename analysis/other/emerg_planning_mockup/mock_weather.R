# Mock up a severe weather warning 
# LTLA: South Staffordshire - E07000196
# UTLA: Staffordshire - E10000028

library(geographr)
library(sf)

warning1 <- boundaries_counties_ua |>
  filter(county_ua_code == "E10000028") |>
  mutate(id = 1, rating = "amber", weather = "rain")  |>
  select(id, rating, weather, geometry)

warning2 <- boundaries_lad |>
  filter(lad_code == "E07000196") |>
  mutate(id = 2, rating = "red", weather = "snow")  |>
  select(id, rating, weather, geometry)

weather_warnings <- warning1 |>
  bind_rows(warning2)

# Save data 
weather_warnings |>
  st_write("data/weather_spatial.geojson", append = FALSE)
