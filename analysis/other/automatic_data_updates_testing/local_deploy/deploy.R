library(rsconnect)

setAccountInfo(name = Sys.getenv("SHINY_ACC_NAME"),
               token = Sys.getenv("TOKEN"),
               secret = Sys.getenv("SECRET"))

# Initial deployment setting name 
deployApp(
  appFiles = c("app.R", "data_output"),
  appName = 'tech_stack_test_main',
  account = Sys.getenv("SHINY_ACC_NAME"),
  appTitle = "shinyapplication")
