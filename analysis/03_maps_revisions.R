##########################
# Combined Maps: Discounted Extra-Welfarist vs New Welfarist (Stacked, separate scales)
##########################

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(patchwork)
library(MetBrewer)

# ------------------------
# Prepare world map
# ------------------------
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name_long != "Antarctica") %>%
  mutate(iso_a3 = if_else(name_long == "France", "FRA", iso_a3))

# ------------------------
# Join data to world map
# ------------------------
world_disc_exwelf <- world %>% left_join(disc_exwelfarist_pp_gdppc_iso3c, by = "iso_a3")
world_new_welf <- world %>% left_join(new_welfarist_pp_pgdp_iso3c, by = "iso_a3")

# ------------------------
# Determine separate color scales
# ------------------------
min_disc_exwelf <- min(world_disc_exwelf$disc_exwelf_med_disc_exwelf_disc_exwelf, na.rm = TRUE)
max_disc_exwelf <- max(world_disc_exwelf$disc_exwelf_med_disc_exwelf_disc_exwelf, na.rm = TRUE)

min_new_welf <- min(world_new_welf$new_welf_med_new_welf, na.rm = TRUE)
max_new_welf <- max(world_new_welf$new_welf_med_new_welf, na.rm = TRUE)

# ------------------------
# Define palette
# ------------------------
palette_signac <- met.brewer("Signac")

# ------------------------
# Create maps
# ------------------------
disc_exwelf_plot <- ggplot() +
  geom_sf(data = world_disc_exwelf, fill = NA, color = "grey80", size = 0.2) +
  geom_sf(data = world_disc_exwelf %>% filter(!is.na(disc_exwelf_med_disc_exwelf_disc_exwelf)),
          aes(fill = disc_exwelf_med_disc_exwelf_disc_exwelf), color = "grey30", size = 0.2) +
  scale_fill_gradientn(colors = palette_signac,
                       limits = c(min_disc_exwelf, max_disc_exwelf),
                       name = "Benefits \n(% GDP per person)") +
  theme_minimal() +
  labs(title = "Extra-welfarist approach") +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12))

new_welf_plot <- ggplot() +
  geom_sf(data = world_new_welf, fill = NA, color = "grey80", size = 0.2) +
  geom_sf(data = world_new_welf %>% filter(!is.na(new_welf_med_new_welf)),
          aes(fill = new_welf_med_new_welf), color = "grey30", size = 0.2) +
  scale_fill_gradientn(colors = palette_signac,
                       limits = c(min_new_welf, max_new_welf),
                       name = "Benefits \n(% GDP per person)") +
  theme_minimal() +
  labs(title = "Welfarist approach") +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12))

# ------------------------
# Combine maps stacked vertically with labels A and B
# ------------------------
combined_plot <- disc_exwelf_plot / new_welf_plot +
  plot_annotation(tag_levels = "A")  # separate legends by default

# ------------------------
# Display
# ------------------------
print(combined_plot)

# ------------------------
# Save high-resolution figure
# ------------------------
ggsave("analysis/plots/stacked_maps_welfarist_vs_extrawelf.tiff",
       plot = combined_plot,
       device = "tiff",
       dpi = 600,
       width = 7.5,  # inches
       height = 10,  # taller for vertical layout
       units = "in",
       compression = "lzw")


#### for extra-welfarist undiscounted

# Join undiscounted extra-welfarist data to world map
world_undisc_exwelf <- world %>%
  left_join(undisc_exwelfarist_pp_gdppc_iso3c, by = "iso_a3")

# Median column
median_col <- "undisc_exwelf_med_undisc_exwelf_undisc_exwelf"

# Calculate min and max for the color scale
min_undisc_exwelf <- min(world_undisc_exwelf[[median_col]], na.rm = TRUE)
max_undisc_exwelf <- max(world_undisc_exwelf[[median_col]], na.rm = TRUE)

# Create map
undisc_exwelfarist_plot <- ggplot() +
  geom_sf(data = world_undisc_exwelf, fill = NA, color = "grey80", size = 0.2) +
  geom_sf(data = world_undisc_exwelf %>% filter(!is.na(.data[[median_col]])),
          aes(fill = .data[[median_col]]), color = "grey30", size = 0.2) +
  scale_fill_gradientn(colors = met.brewer("Signac"),
                       limits = c(min_undisc_exwelf, max_undisc_exwelf),
                       name = "Benefits \n(% GDP per person)") +
  theme_minimal() +
  labs(title = "Extra-welfarist approach") +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom")

# Display map
print(undisc_exwelfarist_plot)

# Save high-resolution TIFF
ggsave("analysis/plots/undisc_exwelfarist_map.tiff",
       plot = undisc_exwelfarist_plot,
       device = "tiff",
       dpi = 600,
       width = 7.5,
       height = 6,
       units = "in",
       compression = "lzw")
