# Pull Vessel Trip Report (VTR data) from confidential repository and clean
library(here)
library(tidyverse)
library(sf)

## File paths ----
box_path  <- "/Users/clovas/Library/CloudStorage/Box-Box/"
proj_path <-  paste0(box_path, "CONFIDENTIAL_GARFO_MAFMC_2025/",
                     "VTR_and_Dealerdata_by_port_and_species_1964-2024")
## Read in files ----
read_vtr <- function(file_path){
  out <- read_csv(file_path) |>
    mutate(VTRSERNO = as.character(VTRSERNO),
           CALC_INSHR_AREA = as.numeric(CALC_INSHR_AREA),
           CALC_LAT_DEG = as.numeric(CALC_LAT_DEG),
           CALC_LON_DEG = as.numeric(CALC_LON_DEG))
  return(out)
}

all_vtr <- tibble("file_path" = list.files(proj_path, pattern = ".csv", full.names = TRUE)) |>
  mutate("data" = map(file_path, read_vtr)) |>
  unnest(data) |>
  select(!file_path) |>
  janitor::clean_names()

## Cleaning ---- 
all_vtr |>
  select(year, sub_trip_id, calc_lat_deg, calc_lat_min, calc_lat_sec, calc_lon_deg, calc_lon_min, calc_lon_sec) |>
  mutate(across(c(year:calc_lon_sec), as.numeric)) |>
  distinct() -> coords

## Longitude ----
degrees <- coords |>
  select(year, sub_trip_id, calc_lon_deg) |>
  rename("deg" = "calc_lon_deg") 

minutes <- coords |>
  select(year, sub_trip_id, calc_lon_min) |>
  mutate(min = calc_lon_min/60) 

seconds <- coords |>
  select(year, sub_trip_id, calc_lon_sec) |>
  mutate(sec = calc_lon_sec/3600) 

degrees |>
  left_join(minutes) |>
  left_join(seconds) |>
  mutate(lon = -1*(deg+min+sec)) -> lon

## Latitude ----
degrees <- coords |>
  select(year, sub_trip_id, calc_lat_deg) |>
  rename("deg" = "calc_lat_deg") 

minutes <- coords |>
  select(year, sub_trip_id, calc_lat_min) |>
  mutate(min = calc_lat_min/60)

seconds <- coords |>
  select(year, sub_trip_id, calc_lat_sec) |>
  mutate(sec = calc_lat_sec/3600) 

degrees |>
  left_join(minutes) |>
  left_join(seconds) |>
  mutate(lat = (deg+min+sec)) -> lat

## Combine 
lat |>
  select(year, sub_trip_id, lat) |>
  left_join(lon |> select(year, sub_trip_id, lon)) -> lat_lon

coords |>
  full_join(lat_lon) -> coords

## Species caught ----
all_vtr |>
  # filter(trip_type == "COMMERCIAL") |> # keeping all trip types for now
  select(year, sub_trip_id, trip_type, port_code, gearcode, species_name, kept, discarded) |>
  filter(kept > 0) |>
  distinct() -> species_catch

## Locations ----
all_vtr |> 
  select(port_code, port_name, state_abb) |>
  distinct() |>
  filter(!is.na(port_code)) |>
  filter(state_abb %in% c("ME","NH","MA","RI","CT","NY","NJ","MD","DE","VA","NC","SC","GA","FL")) |>
  mutate(state_abb = factor(state_abb, levels = c("ME","NH","MA","RI","CT","NY","NJ","MD","DE","VA","NC","SC","GA","FL"))) |>
  arrange(state_abb, port_name) -> locations

## Clean VTR ----
species_catch |>
  left_join(coords |> select(year, sub_trip_id, lat, lon)) |>
  left_join(locations) |>
  # mutate(lat = round(lat, digits = 2), # rounding to better calculate density later on
  #        lon = round(lon, digits = 2)) |>
  distinct() |>
  filter(lat > 20) |>
  filter(lon > -80 & lon < -60) -> clean_vtr

# Crop to Council zones (removes far outliers)
sf_use_s2(FALSE)

shp_path <- here("data", "shapefiles", "Council_Scopes.shp")

boundaries <- st_read(shp_path, quiet = TRUE)
boundaries <- fortify(boundaries)

east_coast <- boundaries |>
  filter(Council %in% c("New England", "Mid-Atlantic", "South Atlantic")) |>
  mutate(factor = factor(Council, levels = c("New England", "Mid-Atlantic", "South Atlantic"))) 

clean_vtr |>
  st_as_sf(coords = c("lon","lat"), crs = st_crs(east_coast)) -> vtr_sf

vtr_crop <- vtr_sf |>
  st_join(east_coast, join = st_intersects)

vtr_crop |>
  cbind(st_coordinates(vtr_crop)) |> # convoluted but whatever
  st_drop_geometry() |>
  filter(!is.na(Council)) |>
  rename("lon" = "X",
         "lat" = "Y") |>
  select(year, sub_trip_id, trip_type, port_code, gearcode, species_name, lat, lon, kept, discarded, port_name, state_abb, Council) -> vtr_crop

# Note: in cropping to the management council zones, we lose inshore area coverage. We might be able to remedy this by intersecting with the landmass to filter out any points on land.
# I also can't seem to re-run it without it hanging up)
## Save out ----
write_csv(vtr_crop, here("data","processed","vessel_trip_reports.csv"))
