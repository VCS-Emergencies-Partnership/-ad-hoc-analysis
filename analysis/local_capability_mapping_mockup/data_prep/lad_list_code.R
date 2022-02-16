library(geographr)
library(sf)
library(tidyverse)

lad_list <- boundaries_lad |>
  filter(str_detect(lad_code, "^E")) |>
  select(lad_name) |>
  st_drop_geometry()

write_csv(lad_list, "core_partners/data/lad_list.csv")
