library(shiny)
library(googlesheets4)
library(googledrive)

gs4_auth(cache = ".secrets", email = TRUE)


# Uncomment this code to run but don't want it to keep re-pulling
# data from googlsheets while not in use 

# Need this to repull the refreshed data from googlesheets
# flood_counts <- reactive({
#   invalidateLater(60000)
#   read_sheet("1nGq6gK3FLIJ_gTlt1D1gOMFzJ6iellyrWBbwHBdKPMQ")
# })


ui <- fluidPage(
  titlePanel("Count of flood warnings"),
  mainPanel(
    dataTableOutput("table")
  )
)


server <- function(input, output) {
  output$table <- renderDataTable(flood_counts())
}

shinyApp(ui = ui, server = server)
