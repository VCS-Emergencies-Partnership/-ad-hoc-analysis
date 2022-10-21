# This script needs to be once to interactively authorise
# Then secrets created to push to shinyapps.io

# From 2nd answer on https://stackoverflow.com/questions/63535190/connect-to-googlesheets-via-shiny-in-r-with-googlesheets4
# Save email address as object 'EMAIL' in .Renviron file 
# Also see https://gargle.r-lib.org/articles/non-interactive-auth.html

library(googlesheets4)
# Set authentication token to be stored in a folder called `.secrets`
options(gargle_oauth_cache = ".secrets")
# Trigger auth on purpose --> store a token in the specified cache
gs4_auth(cache = "googlesheets/.secrets", email = Sys.getenv("EMAIL"))
# See your token file in the cache, if you like
list.files("googlesheets/.secrets/")
# Check that the non-interactive authentication works by first deauthorizing and 
# then authenticate using token. If no browser opens, the authentication works.
gs4_deauth()
gs4_auth(cache = "googlesheets/.secrets", email = Sys.getenv("EMAIL"))
