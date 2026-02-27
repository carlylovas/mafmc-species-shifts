# Pull Vessel Trip Report (VTR data) from confidential repository and clean

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

# ggplot() + 
#   geom_histogram(data = coords, aes(x = calc_lon_deg))

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
  filter(trip_type == "COMMERCIAL") |>
  select(year, sub_trip_id, port_code, gearcode, species_name, kept, discarded) |>
  filter(kept > 0) |>
  distinct() -> species_catch

## Locations ----
all_vtr |> 
  select(port_code, port_name, state_abb) |>
  distinct() |>
  filter(!is.na(port_code)) |>
  filter(state_abb %in% c("ME","NH","MA","RI","CT","NY","NJ","MD","DE","VA")) |>
  mutate(state_abb = factor(state_abb, levels = c("ME","NH","MA","RI","CT","NY","NJ","MD","DE","VA"))) |>
  arrange(state_abb, port_name) -> locations

## Clean VTR ----
species_catch |>
  left_join(coords |> select(year, sub_trip_id, lat, lon)) |>
  left_join(locations) |>
  filter(lat > 0) -> clean_vtr

clean_vtr |>
  filter(is.na(port_name)) |>
  select(port_code) |>
  distinct() -> port_nas