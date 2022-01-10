shinyUI(fluidPage(
  titlePanel("Local capability mapping"),
  "This app aims to show who can do what and where in an emergency (preparedness). This is using mock data.",
  br(),
  br(),
  fluidRow(
    column(
      4,
      pickerInput(
        inputId = "chosen_lad",
        label = "Local Authorities:",
        choices = sort(unique(tidy_data$local_authorities)),
        options = list(`actions-box` = TRUE, title = "Please select local authorities"),
        multiple = T
      )
    ),
    column(
      4,
      pickerInput(
        inputId = "chosen_capability_category",
        label = "Capability category:",
        choices = sort(unique(tidy_data$capability_category)),
        selected = sort(unique(tidy_data$capability_category))[1],
        options = list(`actions-box` = TRUE, title = "Please select capability category"),
        multiple = F
      )
    ),
    column(
      4,
      uiOutput("capability_sub_category_picker")
    )
  ),
  fluidRow(
    column(
      4,
      pickerInput(
        inputId = "chosen_inital_response_time",
        label = "Initial response time:",
        choices = sort(unique(tidy_data$initial_response_time)),
        selected = sort(unique(tidy_data$initial_response_time)),
        options = list(`actions-box` = TRUE, title = "Please select inital response time"),
        multiple = T
      )
    ),
    column(
      4,
      checkboxInput(
        inputId = "provides_vols",
        label = "Provides DBS vols",
        value = FALSE
      )
    ),
    column(
      4,
      checkboxInput(
        inputId = "primary_only",
        label = "Primary response only",
        value = FALSE
      )
    )
  ),
  br(),
  br(),
  fluidRow(
    column(6, DT::DTOutput("table")), 
    column(6, leafletOutput("map"))
  )
))
