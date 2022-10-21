library(shiny)

flood_counts <- readRDS("local_deploy/data_output/flood_counts.rds")

ui <- fluidPage(
  titlePanel("Count of flood warnings"),
  mainPanel(
    dataTableOutput("table")
  )
)


server <- function(input, output) {
  output$table <- renderDataTable(flood_counts)
}

shinyApp(ui = ui, server = server)
