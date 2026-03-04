#### NEFSC Spring-Fall Bottom Trawl Survey Distribution Metrics ####

# Libraries ----
library(here)
library(tidyverse)
library(gmRi)
library(sf)

# Load data ----
trawl_dat <- read_csv(here("data","processed","nefsc_trawl.csv"))

# Center of biomass ----
cob <- trawl_dat |>
  group_by(comname, year, season) |>
  summarise(
    lat = matrixStats::weightedMean(x = lat, w = total_biomass_kg),
    lon = matrixStats::weightedMean(x = lon, w = total_biomass_kg),
    decade = 10*year%/%10
  )
# Leading-center-trailing edges ----
percentiles <- trawl_dat |>
  group_by(comname, year, season) |>
  summarise(
    `95%` = Hmisc::wtd.quantile(x = lat, weights = total_biomass_kg, probs = 0.95),
    `50%` = Hmisc::wtd.quantile(x = lat, weights = total_biomass_kg, probs = 0.50),
    `5%`  = Hmisc::wtd.quantile(x = lat, weights = total_biomass_kg, probs = 0.05)
  ) |> 
  pivot_longer(cols = c(`95%`,`50%`,`5%`), names_to = "percentile", values_to = "lat") |>
  group_by(comname, percentile, season) |>
  mutate(rmean  = zoo::rollapplyr(lat, width = 5, FUN = mean, align = "center", partial = T),
         percentile = factor(percentile, levels = c('5%','50%','95%')))

# Plotting ----
## Maps
usa <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf")
can <- rnaturalearth::ne_states(country = "canada", returnclass = "sf")

cob |>
  group_by(comname) |>
  nest() |>
  mutate(map = map2(data, comname, function(x,y){
    ggplot() +
      geom_sf(data = usa) + geom_sf(data = can) + coord_sf(xlim = c(-66,-78), ylim = c(34,46)) +
      geom_point(data = x, aes(x = lon, y = lat, color = season)) +
      scale_x_continuous(breaks = c(-76, -72, -68)) +
      gmRi::scale_color_gmri() +
      facet_wrap(~decade, nrow = 1) + 
      guides(color = guide_legend(title = "Season")) +
      labs(title = "Center of biomass") +
      theme(text = element_text("Avenir"),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.box = "vertical",
            strip.background = element_blank(),
            strip.text = element_text(hjust = 0, face = "plain", size = 10),
            panel.grid.major = element_line(color = "#535353", linewidth = 0.1, linetype = 3),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent"),
            panel.border = element_rect(fill = "transparent", linetype = 1, linewidth = 0.5, color = "#535353"))
  })) |>
  select(!data) -> maps

## Percentile plots
percentiles |>
  group_by(comname) |>
  nest() |>
  mutate(percentile = map2(data, comname, function(x,y){
    ggplot(data = x) +
      geom_line(aes(x = year, y = rmean, color = percentile), linetype = 2) +
      geom_smooth(aes(x = year, y = rmean, color = percentile), method = "lm", se = F) + 
      labs(title = "Latitude percentiles") +
      facet_wrap(~season, nrow = 1) +
      guides(color = guide_legend("Percentiles")) +
      gmRi::scale_color_gmri() +
      theme(text = element_text(family = "Avenir"),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.box = "vertical",
            strip.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "transparent"),
            panel.border = element_rect(colour = "gray"),
            panel.grid = element_line(colour = "lightgray", linewidth = 0.25),
            panel.grid.minor = element_line(color = "lightgray", linewidth = rel(0.5)))
  })) |>
  select(!data) -> edges

## Distance between centroids
cob |>
  group_by(comname) |>
  nest() |>
  mutate(centers = map2(data, comname, function(x,y){
    ggplot(data = x) +
      geom_line(aes(x = year, y = lat, group = year), color = "#535353", alpha = 0.8) +
      geom_point(aes(x = year, y = lat, color = season)) +
      guides(color = guide_legend(title = "Season")) +
      labs(title = "Center of latitude") + 
      gmRi::scale_color_gmri() +
      theme(text = element_text(family = "Avenir"),
            axis.title = element_blank(),
            legend.position = "bottom",
            legend.box = "vertical",
            strip.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "transparent"),
            panel.border = element_rect(colour = "gray"),
            panel.grid = element_line(colour = "lightgray", linewidth = 0.25),
            panel.grid.minor = element_line(color = "lightgray", linewidth = rel(0.5)))
  })) |>
  select(!data) -> centers

# Combine ----
all_plots <- maps |>
  full_join(edges) |>
  full_join(centers) |>
  group_by(comname) |>
  nest() |>
  mutate(wrap = map2(data, comname, function(x,y){
    map        <- x$map[[1]]
    center     <- x$centers[[1]]
    percentile <- x$percentile[[1]]
    out <- gridExtra::grid.arrange(map, center, percentile, nrow = 2, layout_matrix=rbind(c(1,1), c(2,3)), 
                                   top = grid::textGrob(paste(str_to_sentence(comname)), 
                                                        gp = grid::gpar(col = "black", fontsize = 15, fontfamily = "Avenir", fontface = "bold"),
                                                        just = "center"))
    # ggsave(plot = out, filename = here("Figures", paste0(comname,"case study.png")), width = 13, height = 10)
  }))

# Latitude Accumulation Maps ----
sf_use_s2(FALSE)

shp_path <- here("data", "shapefiles", "Council_Scopes.shp")

boundaries <- st_read(shp_path, quiet = TRUE)
boundaries <- fortify(boundaries)

east_coast <- boundaries |>
  filter(Council %in% c("New England", "Mid-Atlantic", "South Atlantic")) |>
  mutate(factor = factor(Council, levels = c("New England", "Mid-Atlantic", "South Atlantic")))

decade_bins <- trawl_dat |>
  mutate(decade = 10*year%/%10) |>
  group_by(comname, decade) |>
  summarise(`5%` = Hmisc::wtd.quantile(lat, w = total_biomass_kg, probs = 0.05),
            `95%` = Hmisc::wtd.quantile(lat, w = total_biomass_kg, probs = 0.95))

trawl_dat |>
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
      geom_point(data = x, aes(x = lon, y = lat, color = partition, group = partition, alpha = total_biomass_kg)) +
      stat_density2d(data = x, aes(x = lon, y = lat), alpha = 0.8, color = "#ebcb27") +
      facet_wrap(~decade, nrow = 1) +
      guides(color = guide_legend(title = "Biomass distributions"),
             alpha = guide_legend(title = "Total biomass")) +
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


