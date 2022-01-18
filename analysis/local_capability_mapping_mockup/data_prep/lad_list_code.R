library(geographr)
library(sf)
library(tidyverse)

lad_list <- geographr::boundaries_lad |>
  select(lad_name) |>
  st_drop_geometry()

write_csv(lad_list, "lad_list.csv")
