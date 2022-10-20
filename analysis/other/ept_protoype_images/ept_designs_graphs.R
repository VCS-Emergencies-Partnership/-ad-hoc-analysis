# Packages ----
library(geographr)
library(sf)
library(demographr)
library(tidyverse)
library(leaflet)
library(mapview)

# Selection map --------
boundaries_map <- boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) 

# Save default map 
boundaries_map |>
setView(lat = 53.00366, lng = -2.547855, zoom = 7) |>
  mapshot(file = "default_map.png")

northumberland_ltla <- boundaries_ltla21 |>
  filter(str_detect(ltla21_name, "Northumberland"))

selected_map <- boundaries_map |>
  addPolygons(
    data = northumberland_ltla,
    fillColor = "red",
    fillOpacity = 1,
    color = "black",
    weight = 0.5
  ) 

# Save selected map 
selected_map |>
  setView(lat = 53.00366, lng = -2.547855, zoom = 7) |>
  mapshot(file = "selected_map.png")


# Mock RI data ----
bivariate_color_scale <- tibble(
  # High inequality, high income
  "3 - 3" = "#000000",
  "2 - 3" = "#b36600",
  # Low inequality, high income #f0f0f0
  "1 - 3" = "#f3b300",
  "3 - 2" = "#376387",
  # Medium inequality, medium income
  "2 - 2" = "#b3b3b3",
  "1 - 2" = "#f3e6b3",
  # High inequality, low income
  "3 - 1" = "#509dc2",
  "2 - 1" = "#b4d3e1",
  # Low inequality, low income "#f3f3f3"
  "1 - 1" = "#d9d9d9"
) %>%
  gather("group", "fill_colour")

set.seed(4)
ri_msoa <- boundaries_msoa11 |>
  filter(str_detect(msoa11_name, "Northumberland")) |>
  mutate(
    vuln_quantiles = round(runif(38, min = 1, max = 3), 0),
    cap_quantiles = round(runif(38, min = 1, max = 3), 0)
  ) |>
  mutate(group = paste(
    as.numeric(vuln_quantiles), "-",
    as.numeric(cap_quantiles)
  )) |>
  left_join(bivariate_color_scale, by = "group")

# Mock areas high risk of hazard
mock_high_risk_msoas <- c("E02005702", "E02005704", "E02005720")

ri_msoa_high_risk <- ri_msoa |>
  filter(msoa11_code %in% mock_high_risk_msoas)

# Mock areas low resilience
ri_msoa_low_res <- ri_msoa |>
  filter(group == "3 - 3")

# Mock areas low resilience & high risk of hazard
ri_msoa_low_res_high_risk <- ri_msoa |>
  filter(group == "3 - 3", msoa11_code %in% mock_high_risk_msoas)

# Mock RI map ----
# RI map
ri_map_default <- ri_msoa |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillColor = ~fill_colour,
    fillOpacity = 1,
    color = "black",
    weight = 0.5
  )

ri_map_default |>
  mapshot(file = "ri_map_default.png")

# RI map - mock areas high risk of hazard
ri_map_high_risk <- ri_msoa |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) |>
  addPolygons(
    data = ri_msoa_high_risk,
    fillColor = ~fill_colour,
    fillOpacity = 1,
    color = "black",
    weight = 0.5
  )

ri_map_high_risk |>
  mapshot(file = "ri_map_high_risk.png")

# RI map - low resilience
ri_map_low_res <- ri_msoa |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) |>
  addPolygons(
    data = ri_msoa_low_res,
    fillColor = ~fill_colour,
    fillOpacity = 1,
    color = "black",
    weight = 0.5
  )

ri_map_low_res |>
  mapshot(file = "ri_map_low_res.png")

# RI map - low resilience & high risk
ri_map_low_res_high_risk <- ri_msoa |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) |>
  addPolygons(
    data = ri_msoa_low_res_high_risk,
    fillColor = ~fill_colour,
    fillOpacity = 1,
    color = "black",
    weight = 0.5
  )

ri_map_low_res_high_risk |>
  mapshot(file = "ri_map_low_res_high_risk.png")

# RI legend -----
# Separate the groups
forlegend <- bivariate_color_scale %>%
  separate(group,
    into = c(
      "vuln_quantiles",
      "cap_quantiles"
    ),
    sep = " - "
  ) %>%
  mutate(
    vuln = as.integer(vuln_quantiles),
    cap = as.integer(cap_quantiles)
  )

# Legend
ggplot(forlegend, aes(vuln, cap, fill = fill_colour)) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_discrete(position = "top") +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    plot.margin = margin(t = 10, b = 10, l = 10)
  ) +
  labs(
    x = "Higher vulnerability",
    y = "Higher capacity"
  ) +
  theme(
    axis.title = element_text(color = "black", size = 40),
    axis.title.y = element_text(
      angle = 90,
      vjust = -8,
      hjust = 0.5
    )
  ) +
  coord_fixed() +
  annotate(
    "segment",
    x = 0.5,
    y = 3.75,
    xend = 3.5,
    yend = 3.75,
    size = 3,
    col = "black",
    arrow = arrow(length = unit(0.3, "cm"))
  ) +
  annotate(
    "segment",
    x = 0.2,
    y = 3.5,
    xend = 0.2,
    yend = 0.5,
    size = 3,
    col = "black",
    arrow = arrow(length = unit(0.3, "cm"))
  ) +
  annotate(
    geom = "text",
    x = 3,
    y = 3,
    label = "Most in\n need",
    color = "white",
    size = 10.5438
  ) +
  annotate(
    geom = "text",
    x = 1,
    y = 1,
    label = "Least in\n need",
    color = "black",
    size = 10.5438
  )


# Area profiles data - distance to services -------
population_msoa_20_codes_11 <- population_msoa_20_codes_11 |>
  rename(msoa11_name = msoa_11_name, msoa11_code = msoa_11_code)

# Mock distance to services data
ri_msoa_sub <- ri_msoa |>
  st_drop_geometry() |>
  select(msoa11_code, group)
       
msoa_service_distance <- population_msoa_20_codes_11 |>
  filter(str_detect(msoa11_name, "Northumberland")) |>
  mutate(distance_to_service_km = total_population / 1000) |>
  mutate(distance_to_service_km = if_else(msoa11_code == "E02005729", 10.3,  distance_to_service_km)) |>
  select(msoa11_code, distance_to_service_km) |>
  left_join(boundaries_msoa11, by = "msoa11_code") |>
  left_join(ri_msoa_sub , by = "msoa11_code") |>
  st_as_sf()

# Mock areas high risk of hazard
msoa_service_distance_high_risk <- msoa_service_distance |>
  filter(msoa11_code %in% mock_high_risk_msoas)

# Mock areas low resilience
msoa_service_distance_low_res <- msoa_service_distance |>
  filter(group == "3 - 3")

# Mock areas low resilience & high risk of hazard
msoa_service_distance_low_res_high_risk <- msoa_service_distance |>
  filter(group == "3 - 3", msoa11_code %in% mock_high_risk_msoas)

# Make continuous palette
min_ind <- min(msoa_service_distance$distance_to_service_km)
max_ind <- max(msoa_service_distance$distance_to_service_km)
bins <- round(seq.int(min_ind, max_ind, by = (max_ind - min_ind) / 5), 0)
pal <- colorBin("Greens", domain = msoa_service_distance$distance_to_service_km, bins = bins)

# Area profiles map 
service_map_default <- msoa_service_distance |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) |>
  addPolygons(
    data = msoa_service_distance,
    fillColor = ~ pal(distance_to_service_km),
    fillOpacity = 0.9,
    color = "black",
    weight = 0.5
  ) |>
  addLegend(
    pal = pal,
    values = ~distance_to_service_km,
    opacity = 0.9,
    title = "Distance to services (km)",
    position = "bottomright"
  )

service_map_default |>
  mapshot(file = "service_map_default.png")

# Area profiles map - mock areas high risk of hazard
service_map_high_risk <- msoa_service_distance |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) |>
  addPolygons(
    data = msoa_service_distance_high_risk,
    fillColor = ~ pal(distance_to_service_km),
    fillOpacity = 0.9,
    color = "black",
    weight = 0.5
  ) |>
  addLegend(
    pal = pal,
    values = ~distance_to_service_km,
    opacity = 0.9,
    title = "Distance to services (km)",
    position = "bottomright"
  )

service_map_high_risk |>
  mapshot(file = "service_map_high_risk.png")

# Area profiles map - mock areas of low resilience
service_map_low_res <- msoa_service_distance |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) |>
  addPolygons(
    data = msoa_service_distance_low_res,
    fillColor = ~ pal(distance_to_service_km),
    fillOpacity = 0.9,
    color = "black",
    weight = 0.5
  ) |>
  addLegend(
    pal = pal,
    values = ~distance_to_service_km,
    opacity = 0.9,
    title = "Distance to services (km)",
    position = "bottomright"
  )

service_map_low_res |>
  mapshot(file = "service_map_low_res.png")

# Area profiles map - low resilience & high risk
service_map_low_res_high_risk <- msoa_service_distance |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) |>
  addPolygons(
    data = msoa_service_distance_low_res_high_risk,
    fillColor = ~ pal(distance_to_service_km),
    fillOpacity = 0.9,
    color = "black",
    weight = 0.5
  ) |>
  addLegend(
    pal = pal,
    values = ~distance_to_service_km,
    opacity = 0.9,
    title = "Distance to services (km)",
    position = "bottomright"
  )

service_map_low_res_high_risk |>
  mapshot(file = "service_map_low_res_high_risk.png")

# Area profiles data - house overcrowding -------

msoa_overcrowding <-  msoa_service_distance |>
  mutate(overcrowding = distance_to_service_km + runif(1, min = -5, max = 5)) |>
  select(-distance_to_service_km)

# Mock areas high risk of hazard
msoa_overcrowding_high_risk <- msoa_overcrowding |>
  filter(msoa11_code %in% mock_high_risk_msoas)

# Mock areas low resilience
msoa_overcrowding_low_res <- msoa_overcrowding |>
  filter(group == "3 - 3")

# Mock areas low resilience & high risk of hazard
msoa_overcrowding_low_res_high_risk <- msoa_overcrowding |>
  filter(group == "3 - 3", msoa11_code %in% mock_high_risk_msoas)

# Make continuous palette
min_ind <- min(msoa_overcrowding$overcrowding)
max_ind <- max(msoa_overcrowding$overcrowding)
bins <- round(seq.int(min_ind, max_ind, by = (max_ind - min_ind) / 5), 0)
pal <- colorBin("Greens", domain = msoa_overcrowding$overcrowding, bins = bins)

# Area profiles map 
overcrowding_map_default <- msoa_overcrowding |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) |>
  addPolygons(
    data = msoa_overcrowding,
    fillColor = ~ pal(overcrowding),
    fillOpacity = 0.9,
    color = "black",
    weight = 0.5
  ) |>
  addLegend(
    pal = pal,
    values = ~overcrowding,
    opacity = 0.9,
    title = "% housing overcrowding",
    position = "bottomright"
  )

overcrowding_map_default |>
  mapshot(file = "overcrowding_map_default.png")

# Area profiles map - mock areas high risk of hazard
overcrowding_map_high_risk <- msoa_overcrowding |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) |>
  addPolygons(
    data = msoa_overcrowding_high_risk,
    fillColor = ~ pal(overcrowding),
    fillOpacity = 0.9,
    color = "black",
    weight = 0.5
  ) |>
  addLegend(
    pal = pal,
    values = ~overcrowding,
    opacity = 0.9,
    title = "% housing overcrowding",
    position = "bottomright"
  )

overcrowding_map_high_risk |>
  mapshot(file = "overcrowding_map_high_risk.png")

# Area profiles map - mock areas of low resilience
overcrowding_map_low_res <- msoa_overcrowding |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) |>
  addPolygons(
    data = msoa_overcrowding_low_res,
    fillColor = ~ pal(overcrowding),
    fillOpacity = 0.9,
    color = "black",
    weight = 0.5
  ) |>
  addLegend(
    pal = pal,
    values = ~overcrowding,
    opacity = 0.9,
    title = "% housing overcrowding",
    position = "bottomright"
  )

overcrowding_map_low_res |>
  mapshot(file = "overcrowding_map_low_res.png")

# Area profiles map - low resilience & high risk
overcrowding_map_low_res_high_risk <- msoa_overcrowding |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.5
  ) |>
  addPolygons(
    data = msoa_overcrowding_low_res_high_risk,
    fillColor = ~ pal(overcrowding),
    fillOpacity = 0.9,
    color = "black",
    weight = 0.5
  ) |>
  addLegend(
    pal = pal,
    values = ~overcrowding,
    opacity = 0.9,
    title = "% housing overcrowding",
    position = "bottomright"
  )

overcrowding_map_low_res_high_risk |>
  mapshot(file = "overcrowding_map_low_res_high_risk.png")

# Whole area map -------
area_map_whole <- msoa_service_distance |>
  summarise(geometry = st_union(geometry)) |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fill = "black",
    fillOpacity = 0.2,
    color = "black",
    weight = 0.7
  )  |>
  addLegend(
    colors = "black",
    labels = "Selected area",
    opacity = 0.2,
    position = "bottomright"
  )

area_map_whole |>
  mapshot(file = "area_map_whole.png")

# High risk map
area_map_whole_high_risk <- msoa_service_distance |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.7
  ) |>
  addPolygons(
    data = msoa_service_distance_high_risk,
    fillColor = "black",
    fillOpacity = 0.7,
    color = "black",
    weight = 0.7
  )

area_map_whole_high_risk |>
  mapshot(file = "area_map_whole_high_risk.png")

# Low resilience map
area_map_whole_low_res <- msoa_service_distance |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.7
  ) |>
  addPolygons(
    data = msoa_service_distance_low_res,
    fillColor = "black",
    fillOpacity = 0.7,
    color = "black",
    weight = 0.7
  ) |>
  addLegend(
    colors = "black",
    labels = "Area of low resilience",
    opacity = 0.9,
    position = "bottomright"
  )

area_map_whole_low_res |>
  mapshot(file = "area_map_whole_low_res.png")

# Low resilience & hiigh risk map 
area_map_whole_low_res_high_risk <- msoa_service_distance |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillOpacity = 0,
    color = "black",
    weight = 0.7
  ) |>
  addPolygons(
    data = msoa_service_distance_low_res_high_risk,
    fillColor = "black",
    fillOpacity = 0.7,
    color = "black",
    weight = 0.7
  ) |>
  addLegend(
    colors = "black",
    labels = "Area of low resilience & high risk to hazard",
    opacity = 0.9,
    position = "bottomright"
  )

area_map_whole_low_res_high_risk |>
  mapshot(file = "area_map_whole_low_res_high_risk.png")

###############################################################################################################

# Testing visuals in ggplot (rather than leaflet)

# msoa_service_distance |>
#   ggplot() +
#   geom_sf(aes(fill = distance_to_service_km, geometry = geometry)) +
#   scale_fill_distiller(palette = "Oranges") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_blank())

# ri_msoa |>
#   ggplot() +
#   geom_sf(aes(geometry = geometry, fill = fill_colour), data = ri_msoa) +
#   scale_fill_identity() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_blank(),
#         axis.ticks = element_blank(), axis.text = element_blank())

population_msoa_19_codes_11 |>
  filter(str_detect(msoa11_name, "Northumberland")) |>
  left_join(boundaries_msoa11, by = "msoa11_code") |>
  summarise(geometry = st_union(geometry)) |>
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_blank()
  )

sub_areas <- population_msoa_19_codes_11 |>
  filter(msoa11_code %in% c("E02005702", "E02005704")) |>
  left_join(boundaries_msoa11, by = "msoa11_code") |>
  select(geometry, msoa11_code, total_population)

population_msoa_19_codes_11 |>
  filter(str_detect(msoa11_name, "Northumberland")) |>
  left_join(boundaries_msoa11, by = "msoa11_code") |>
  summarise(geometry = st_union(geometry)) |>
  ggplot() +
  geom_sf(aes(geometry = geometry), fill = "white") +
  geom_sf(aes(geometry = geometry), fill = "darkgrey", data = sub_areas) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_blank()
  )

population_msoa_19_codes_11 |>
  filter(str_detect(msoa11_name, "Northumberland")) |>
  left_join(boundaries_msoa11, by = "msoa11_code") |>
  summarise(geometry = st_union(geometry)) |>
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  geom_sf(aes(fill = total_population, geometry = geometry), data = sub_areas) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_blank()
  )

population_msoa_19_codes_11 |>
  filter(str_detect(msoa11_name, "Northumberland")) |>
  select(msoa11_code, total_population) |>
  left_join(boundaries_msoa11, by = "msoa11_code") |>
  ggplot() +
  geom_sf(aes(fill = total_population, geometry = geometry)) +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_blank()
  )



# Testing in ggplot (instead of leaflet)

