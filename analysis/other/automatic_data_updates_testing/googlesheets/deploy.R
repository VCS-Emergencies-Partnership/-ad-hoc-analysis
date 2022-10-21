library(rsconnect)

setAccountInfo(name = Sys.getenv("SHINY_ACC_NAME"),
               token = Sys.getenv("TOKEN"),
               secret = Sys.getenv("SECRET"))

# Initial deployment setting name 
deployApp(
  appFiles = c("googlesheets/app.R"),
  appName = 'googlesheets',
  account = Sys.getenv("SHINY_ACC_NAME"),
  appTitle = "shinyapplication")


