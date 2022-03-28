

chosen <- lookup |>
  filter(postcode %in% c("WS67DU", "WS67HB", "WS114NP")) |>
  distinct(lsoa_code) 


chosen_sf <- boundaries_lsoa |>
  inner_join(chosen, by = "lsoa_code")

st_bbox(chosen_sf)

output$map <- renderLeaflet({
  
  boundaries_countries |> 
  leaflet() |>
    addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(stroke = FALSE,
                  color = ~pal_elderly(prop_pop_elderly),
                  fillOpacity = 1) |>
      addLegend("bottomright", pal = pal_elderly, values = ~prop_pop_elderly,
                opacity = 1
})

ind_sf |>
  inner_join(chosen, by = "lsoa_code") |>
|>
  
  leafletProxy("map") |>
  
