# Create new data frame with lg_total and lghat_total for each age group in each income group
practice_lg_glhat <- res_full %>%
  filter(name == "deaths") %>%
  # Group by iso3c, replicate, and age_group to calculate the totals
  group_by(iso3c, replicate, age_group) %>%
  mutate(
    lg_total = sum(lg_total),
    lghat_total = sum(lghat_total)
  ) %>%
  ungroup() %>%
  # Group by income_group and age_group to summarize the totals
  group_by(income_group, age_group) %>%
  summarise(
    lg_total_low = lf(lg_total),
    lg_total_med = mf(lg_total),
    lg_total_high = hf(lg_total),
    lghat_total_low = lf(lghat_total),
    lghat_total_med = mf(lghat_total),
    lghat_total_high = hf(lghat_total),
    .groups = 'drop'
  )



# load map data

library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(ggplot2)
library(sf)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sum_qaly_iso3c, aes(x = lon, y = lat), color = "red", size = 3) +
  theme_minimal() +
  labs(title = "World Map with Data Points")

worldsf <- readRDS("/analysis/data/raw/worldsf.Rds")

#merge data with sf
world_and_qaly <- dplyr::left_join(
  dplyr::select(world, "iso3c"),
  dplyr::select(sum_qaly_iso3c, "qalys_averted_sum", iso3c),
  by = "iso3c"
)

#where to plot
xlim <- c(-155,170)
ylim <- c(-50,60)
legendPos <-  c(0.15, 0.31)
#plot
ggplot2::ggplot(world_and_qaly) +
  ggplot2::geom_rect(ggplot2::aes(xmin = -180, xmax = 180, ymin = -75, ymax = 90), fill = "light blue", inherit.aes = FALSE) +
  ggplot2::geom_sf(ggplot2::aes_string(fill="varCat"), colour="black", size = 1/10) +
  ggplot2::theme_void() +
  ggplot2::scale_fill_brewer(palette = "OrRd", na.value = "grey")+#,  na.translate = F) +
  ggplot2::coord_sf(xlim = xlim, ylim = ylim) +
  ggplot2::labs(fill = lab, title = title) +
  ggplot2::theme(legend.position = legendPos,
                 legend.title = element_text( size=8), legend.text=element_text(size=8),
                 legend.key.size = unit(0.5, 'cm'))

ggplot2


library(geojsonio)

geojson_read(
  x,
  parse = FALSE,
  what = "list",
  stringsAsFactors = FALSE,
  query = NULL,
  ...
)

world_geojson <- "https://r2.datahub.io/clvyjaryy0000la0cxieg4o8o/master/raw/data/countries.geojson"
saveRDS(world_geojson, "analysis/data/raw/world_geojson.rds")

geojson_read(world_geojson)
geojson_read(world_geojson, parse = TRUE)

world_geojson <- geojson_read(world_geojson, what = "sp")
world_sf <- st_as_sf(world_geojson)
