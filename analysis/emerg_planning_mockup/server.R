
server <- function(input, output, session) {


  # Server side rendering of area choices
  updateSelectizeInput(session,
    "areas_selected",
    choices = lookup$postcode,
    server = TRUE
  )

  # Skip from areas to profiles
  observeEvent(input$forward_area, {
    updateTabsetPanel(session,
      "tabs",
      selected = "Profile"
    )
  })


  # Filter LSOAs selected
  lsoas_selected <- reactive({
   lookup |>
      filter(postcode %in% input$areas_selected) |>
      distinct(lsoa_code)
  })

  # Filter LSOAs with spatial element
  lsoas_selected_sf <- reactive({
    lsoas_selected() |>
      left_join(boundaries_lsoa_projected, by = "lsoa_code")
  })

  # Summarise number of warnings in LSOAs selected
  warnings_count <- reactive({
    lsoa_alerts |>
      inner_join(lsoas_selected(), by = "lsoa_code") |>
      group_by(alert_type, rating) |>
      summarise(count_alerts = n_distinct(id))
  })


  output$warnings_count_table <- DT::renderDataTable({
    warnings_count()
  })
  
  # output$map <- renderLeaflet({
  #   
  #   ind_sf |>
  #     select(-prop_house_fuel_pov) |>
  #     leaflet() |>
  #     addProviderTiles(providers$CartoDB.Positron) |>
  #     addPolygons(stroke = FALSE,
  #                 color = ~pal_elderly(prop_pop_elderly),
  #                 fillOpacity = 1) |>
  #     addLegend("bottomright", pal = pal_elderly, values = ~prop_pop_elderly,
  #               opacity = 1)
  # })
    
}
