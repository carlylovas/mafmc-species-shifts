# Pull observer data from confidential repository and clean

## File paths ----
box_path  <- "/Users/clovas/Library/CloudStorage/Box-Box/"
proj_path <-  paste0(box_path, "CONFIDENTIAL_GARFO_MAFMC_2025/Observer data/")

path <- paste0(proj_path, "DR25-200_Mills_Allyn.xlsx")
path |>
  excel_sheets() |> 
  set_names()  |>
  map(read_excel, path = path) -> all_observer

haul_data <- tibble(rbind(all_observer$haul_1989_2009, all_observer$haul_2010_2024)) |>
  janitor::clean_names()

catch <- all_observer[str_detect(names(all_observer), "catch_")]

## 1989-1995 missing a column for year, using LINK1 to extract year column

catch_samp <- tibble(rbindlist(catch[c(1:7)])) |>
  mutate(YEAR = as.numeric(str_sub(LINK1, start = 4, end = -9)),
         SPECIES_ITIS = as.numeric(SPECIES_ITIS),
         LIVE_WT = as.numeric(LIVE_WT))

catch_data <- tibble(rbindlist(catch[c(8:36)])) |> # this is every year onward
  full_join(catch_samp) |>
  arrange(YEAR) |>
  janitor::clean_names()


## Cleaning

# demo_haul |>
#   select(link1, link3, negear, targspec1, targspec2, targspec3, gis_lathbeg, gis_lonhbeg, gis_latsbeg, gis_lonsbeg) |>
#   full_join(demo_catch) |>
#   relocate(year, .after = link3) |>
#   relocate(comname, .after = year) |> 
#   mutate(kept = ifelse(str_detect(fishdispdesc, "KEPT"), "Kept", "Discarded")) |>
#   select(link3, year, negear, comname, targspec1, targspec2, targspec3, gis_lathbeg, gis_lonhbeg, gis_latsbeg, gis_lonsbeg, hailwt, live_wt, kept) |>
#   filter(!is.na(live_wt)) -> catch_haul
# 
# catch_haul |>
#   filter(!is.na(gis_latsbeg) & !gis_latsbeg == 1 & (is.na(gis_lathbeg))) |>
#   select(!c(gis_lathbeg, gis_lonhbeg)) |>
#   rename(lat = gis_latsbeg,
#          lon = gis_lonsbeg) -> set_lats # evidently, these are all rod and reel?
# 
# catch_haul |>
#   filter(!link3 %in% set_lats$link3) |>
#   select(!c(gis_latsbeg, gis_lonsbeg)) |>
#   rename(lat = gis_lathbeg,
#          lon = gis_lonhbeg) |>
#   full_join(set_lats) |>
#   drop_na() |> 
#   mutate(decade =  10*year%/%10) -> catch_coords