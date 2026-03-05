#### Vessel Trip Report Fleet Distribution Metrics ####

# Libraries ----
library(here)
library(tidyverse)
library(gmRi)
library(sf)

# Load data ----
obs_dat <- read_csv(here("data","processed","observer_catch_haul_combined.csv"))

# Latitude Accumulation Maps ----
sf_use_s2(FALSE)

shp_path <- here("data", "shapefiles", "Council_Scopes.shp")

boundaries <- st_read(shp_path, quiet = TRUE)
boundaries <- fortify(boundaries)

east_coast <- boundaries |>
  filter(Council %in% c("New England", "Mid-Atlantic", "South Atlantic")) |>
  mutate(factor = factor(Council, levels = c("New England", "Mid-Atlantic", "South Atlantic")))

usa <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf")
can <- rnaturalearth::ne_states(country = "canada", returnclass = "sf")

decade_bins <- obs_dat |> 
  mutate(decade = 10*year%/%10) |>
  group_by(comname, decade) |>
  summarise(`5%` = Hmisc::wtd.quantile(lat,  w = live_wt, probs = 0.05),
            `95%` = Hmisc::wtd.quantile(lat, w = live_wt, probs = 0.95))

obs_dat |>
  mutate(decade = 10*year%/%10) |>
  right_join(decade_bins, by = join_by(comname, decade)) |>
  group_by(comname, decade) |>
  mutate(partition = case_when(
    lat <= `5%` ~ "Trailing", 
    lat > `5%` & lat < `95%` ~ "Center",
    lat >= `95%` ~ "Leading"
  )) |>
  mutate(partition = factor(partition, levels = c("Trailing", "Center", "Leading"))) -> lat_accum

accum_maps <- lat_accum |>
  group_by(comname) |>
  nest() |>
  mutate(map = map2(data, comname, function(x,y){
    ggplot() + 
      geom_sf(data = usa) + geom_sf(data = can) + geom_sf(data = east_coast, fill = "transparent") +
      coord_sf(ylim = c(35,45), xlim = c(-66,-78)) + 
      geom_point(data = x, aes(x = lon, y = lat, color = partition, group = partition, alpha = live_wt)) +
      stat_density2d(data = x, aes(x = lon, y = lat), alpha = 0.8, color = "#ebcb27") +
      facet_wrap(~decade, nrow = 1) +
      guides(color = guide_legend(title = "Fleet distributions"),
             alpha = guide_legend(title = "Kept catch")) +
      scale_x_continuous(breaks = c(-66, -70, -74, -78)) +
      scale_color_manual(values = c("#363b45", "#00608a","#C1DEFF")) +
      theme(text = element_text("Avenir"),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.box = "vertical",
            strip.background = element_blank(),
            strip.text = element_text(hjust = 0, face = "bold", size = 10),
            panel.grid.major = element_line(color = "#e9e9e9", linewidth = 0.1, linetype = 3),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent"),
            panel.border = element_rect(fill = "transparent", linetype = 1, linewidth = 0.5, color = "#535353"))
  }))

