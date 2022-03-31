library(dplyr)
library(sf)

# AzureStor blob function https://rdrr.io/cran/AzureStor/man/blob.html

# Establish connection with blob storage
get_container = function() {
  blob = AzureStor::storage_endpoint(Sys.getenv("BLOB_ENDPOINT"),
                                     sas = Sys.getenv("BLOB_SAS"))
  AzureStor::storage_container(blob, "processed")
}

# Test saving to memory of R session (rather than download)
# as a raw vector with dest=NULL
# https://github.com/Azure/AzureStor

rawvec <- get_container() %>%
  AzureStor::download_blob(src = "lad19_eng_wales_sc_ni.geojson",
                           dest = NULL)

charvec <- rawToChar(rawvec)
lad_uk <- read_sf(charvec)


# Download all files
download_all = function() {
  get_container() %>%
    AzureStor::multidownload_blob(src = "*",
                                  dest = "./data/",
                                  overwrite = TRUE)
}

download_all()

# download single file 
# dest needs to be the full filepath & name of where want to save (differs from multidownload_blob)
file_chosen <- "lad19_eng_wales_sc_ni.geojson"
file_path_chosen <- paste0("./data/", file_chosen)

download = function(file_name, file_path) {
  get_container() %>%
    AzureStor::download_blob(src = file_name,
                             dest = file_path,
                             overwrite = TRUE)
}

download(file_chosen, file_path_chosen)