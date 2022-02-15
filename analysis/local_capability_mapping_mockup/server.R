shinyServer(function(input, output) {
  
  # initialise then start the guide
  guide$init()$start()
  
  output$capability_dropdown <- renderUI({
    if (input$chosen_detail_level) {
      pickerInput(
        inputId = "chosen_capability_sub_category",
        label = "Capability sub category:",
        choices = multilevel_cat_list,
        options = list(`actions-box` = TRUE, title = "Please select capability subcategories"),
        multiple = T
      )
    } else {
      pickerInput(
        inputId = "chosen_capability_category",
        label = "Capability category:",
        choices = sort(unique(tidy_data$capability_category)),
        options = list(`actions-box` = TRUE, title = "Please select capability categories"),
        multiple = T
      )
    }
  })

  filtered_data <- reactive({
    vols_selected <- if (input$provides_vols) {
      "Yes"
    } else {
      unique(tidy_data$provide_dbs_checked_volunteers)
    }
    response_selected <- if (input$primary_only) {
      "Primary"
    } else {
      unique(tidy_data$response_type)
    }

    tidy_data_filtered <- tidy_data |>
      filter(
        local_authorities %in% input$chosen_lad,
        initial_response_time %in% input$chosen_inital_response_time,
        provide_dbs_checked_volunteers %in% vols_selected,
        response_type %in% response_selected
      ) |>
      filter(response_type != "Not provided")

    if (input$chosen_detail_level) {
      tidy_data_filtered |>
        filter(capability_sub_category %in% input$chosen_capability_sub_category)
    } else {
      tidy_data_filtered |>
        filter(capability_category %in% input$chosen_capability_category)
    }
  })

  output$table <- DT::renderDataTable({
    filtered_data() |>
      distinct(Name = partner, Wesbite = website, Contact = contact, Summary = specialism_focus, "No. of volunteers" = number_of_volunteers)
  })

  output$map <- renderLeaflet({
    validate(need(input$chosen_lad != "", "Choose a local authority"))

    validate(need(nrow(filtered_data()) != 0, "No organisations meet your criteria"))

    lads <- filtered_data() |>
      distinct(local_authorities) |>
      pull()

    lads_to_map <- geographr::boundaries_lad |>
      filter(lad_name %in% lads)

    full_map_bbox <- boundaries_lad |>
      filter(lad_name %in% unique(tidy_data$local_authorities)) |>
      st_bbox()

    leaflet() |>
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
      ) |>
      fitBounds(
        lng1 = as.numeric(full_map_bbox["xmin"]),
        lat1 = as.numeric(full_map_bbox["ymin"]),
        lng2 = as.numeric(full_map_bbox["xmax"]),
        lat2 = as.numeric(full_map_bbox["ymax"])
      )
  })
})
