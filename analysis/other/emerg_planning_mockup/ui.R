ui <- fluidPage(
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Area",
      selectizeInput("areas_selected",
        label = "Enter postcode(s)",
        choices = NULL,
        multiple = TRUE
      ),
      actionButton(
        "forward_area",
        "See profile"
      )
    ),
    tabPanel(
      "Profile",
      DT::dataTableOutput("warnings_count_table"),
      selectInput("ind_selected",
        label = "Select indicator",
        choices = ind_values
      ),
     # leafletOutput("map")
    )
  )
)
