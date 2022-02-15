shinyUI(fluidPage(
  # For step through guide
  use_cicerone(), 
  # Ste up of Hotjar code https://divadnojnarg.github.io/post/shinyapp_hotjar/
  tags$head(includeScript("www/hotjar.js")),
  titlePanel("Local capability mapping"),
  "This app aims to help partners understand each other i.e who could do what and where in an emergency (preparedness). This is using mock data.",
  br(),
  br(),
  fluidRow(
    column(
      3,
      div(class = "capability_select",  # use this <div> for help guide
      uiOutput("capability_dropdown")
      )
    ),
    column(
      3,
      checkboxInput(
        inputId = "chosen_detail_level",
        label = "Capabilities at subcategory level",
        value = FALSE
      )
    )
  ),
  fluidRow(
    column(
      3,
      pickerInput(
        inputId = "chosen_lad",
        label = "Local Authorities:",
        choices = sort(unique(tidy_data$local_authorities)),
        selected = sort(unique(tidy_data$local_authorities)),
        options = list(`actions-box` = TRUE, title = "Please select local authorities"),
        multiple = T
      )
    ),
    column(
      3,
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
      2,
      checkboxInput(
        inputId = "provides_vols",
        label = "Provides DBS vols",
        value = FALSE
      )
    ),
    column(
      2,
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
