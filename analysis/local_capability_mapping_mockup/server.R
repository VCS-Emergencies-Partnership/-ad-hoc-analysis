shinyServer(function(input, output) {
  output$capability_sub_category_picker <- renderUI({
    sub_categories_possible <- category_lookup |>
      filter(capability_category %in% input$chosen_capability_category) %>%
      pull(capability_sub_category)

    pickerInput(
      inputId = "chosen_capability_sub_category",
      label = "Capability sub category:",
      choices = sort(sub_categories_possible),
      selected = sort(sub_categories_possible),
      options = list(`actions-box` = TRUE, title = "Please select capability sub category"),
      multiple = T
    )
  })



  filtered_data <- reactive({
    vols_selected <- ifelse(input$provides_vols, "Yes", unique(tidy_data$provide_dbs_checked_volunteers))
    response_selected <- ifelse(input$primary_only, "Primary", unique(tidy_data$response_type))

    tidy_data |>
      filter(
        local_authorities %in% input$chosen_lad,
        initial_response_time %in% input$chosen_inital_response_time,
        capability_category %in% input$chosen_capability_category,
        capability_sub_category %in% input$chosen_capability_sub_category,
        provide_dbs_checked_volunteers %in% vols_selected,
        response_type %in% response_selected
      ) |>
      filter(response_type != "Not provided")
  })

  output$table <- DT::renderDataTable({
    filtered_data() |>
      distinct(partner, website, contact, specialism_focus)
  })

  output$map <- renderLeaflet({
    
    validate(need(input$chosen_lad != "", 'Choose a local authority'))
    
    validate(need(nrow(filtered_data()) != 0, 'No organisations meet your criteria'))
    
    lads <- filtered_data() |>
      distinct(local_authorities) |>
      pull()

    lads_to_map <- geographr::boundaries_lad |>
      filter(lad_name %in% lads)


    leaflet() |>
      setView(lat = 52.75, lng = -2.0, zoom = 6) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        data = lads_to_map,
        layerId = ~geometry,
        weight = 0.7,
        opacity = 0.5,
        color = "#5C747A",
        dashArray = "0.1",
        fillOpacity = 0.4,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.4,
          bringToFront = TRUE
        ),
        label = lads_to_map$lad_name
      )
  })
})
