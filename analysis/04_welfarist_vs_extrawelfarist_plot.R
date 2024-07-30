##############
# UNDISCOUNTED WELFARIST: UNDISCOUNTED MONETIZED QALYS, HUMAN CAPITAL COSTS, FRICTION COSTS, HEALTHCARE COSTS
##############

undisc_welfarist_sum_iso3c <- sum_undiscmonqaly_iso3c %>%
  left_join(human_capital_sum_iso3c, by = "iso3c") %>%
  left_join(friction_costs_sum_iso3c, by = "iso3c") %>%
  left_join(hc_costs_total_iso3c, by = "iso3c") %>%
  group_by(iso3c) %>%  # Ensure grouping by iso3c
  reframe(
    undisc_welf_total_low = sum(undiscmonqalys_averted_sum_low, hc_costs_total_low, friction_costs_total_low, health_costs_total_low, na.rm = TRUE),
    undisc_welf_total_med = sum(undiscmonqalys_averted_sum_med, hc_costs_total_med, friction_costs_total_med, health_costs_total_med, na.rm = TRUE),
    undisc_welf_total_high = sum(undiscmonqalys_averted_sum_high, hc_costs_total_high, friction_costs_total_high, health_costs_total_high, na.rm = TRUE)
  ) %>%
  filter(if_any(c(undisc_welf_total_low, undisc_welf_total_med, undisc_welf_total_high), ~ . != 0))

# our results table which we can then save in the tables directory
undisc_welfarist_sum_iso3c
write.csv(undisc_welfarist_sum_iso3c, "analysis/tables/undisc_welfarist_sum_iso3c.csv")

### turn into map
# plot the data on the map
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# load the world map data from naturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

# insect world map data
head(world)

# rename to match the world data column
undisc_welfarist_sum_iso3c <- undisc_welfarist_sum_iso3c %>%
  rename(iso_a3 = iso3c)

# merge the data
world_undiscwelfarist <- world %>%
  left_join(undisc_welfarist_sum_iso3c, by = "iso_a3")

# plot the map
undisc_welfarist_sum_iso3c_plot <- ggplot(data = world_undiscwelfarist) +
  geom_sf(aes(fill = undisc_welf_total_med)) +  # Ensure this column exists in your data
  scale_fill_gradientn(colors = palette, na.value = "grey90", name = "Undiscounted Welfarist Benefits ($)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
  ) +
  labs(
    title = "Undiscounted Welfarist Benefits",
    fill = "Undiscounted Welfarist Benefits ($)"
  ) +
  coord_sf(expand = FALSE)

# quick look at the plot
undisc_welfarist_sum_iso3c_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "undisc_welfarist_sum_iso3c_plot", undisc_welfarist_sum_iso3c_plot)

##############
# DISCOUNTED WELFARIST: DISCOUNTED MONETIZED QALYS, HUMAN CAPITAL COSTS, FRICTION COSTS, HEALTHCARE COSTS
##############

disc_welfarist_sum_iso3c <- sum_discmonqaly_iso3c %>%
  left_join(human_capital_sum_iso3c, by = "iso3c") %>%
  left_join(friction_costs_sum_iso3c, by = "iso3c") %>%
  left_join(hc_costs_total_iso3c, by = "iso3c") %>%
  group_by(iso3c) %>%  # Ensure grouping by iso3c
  reframe(
    disc_welf_total_low = sum(discmonqalys_averted_sum_low, hc_costs_total_low, friction_costs_total_low, health_costs_total_low, na.rm = TRUE),
    disc_welf_total_med = sum(discmonqalys_averted_sum_med, hc_costs_total_med, friction_costs_total_med, health_costs_total_med, na.rm = TRUE),
    disc_welf_total_high = sum(discmonqalys_averted_sum_high, hc_costs_total_high, friction_costs_total_high, health_costs_total_high, na.rm = TRUE)
  ) %>% filter(if_any(c(disc_welf_total_low, disc_welf_total_med, disc_welf_total_high), ~ . != 0))


# our results table which we can then save in the tables directory
disc_welfarist_sum_iso3c
write.csv(disc_welfarist_sum_iso3c, "analysis/tables/undisc_welfarist_sum_iso3c.csv")

# rename to match the world data column
disc_welfarist_sum_iso3c <- disc_welfarist_sum_iso3c %>%
  rename(iso_a3 = iso3c)

# merge the data
world_discwelfarist <- world %>%
  left_join(disc_welfarist_sum_iso3c, by = "iso_a3")

# plot the map
disc_welfarist_sum_iso3c_plot <- ggplot(data = world_discwelfarist) +
  geom_sf(aes(fill = disc_welf_total_med)) +  # Ensure this column exists in your data
  scale_fill_gradientn(colors = palette, na.value = "grey90", name = "Discounted Welfarist Benefits ($)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
  ) +
  labs(
    title = "Discounted Welfarist Benefits",
    fill = "Discounted Welfarist Benefits ($)"
  ) +
  coord_sf(expand = FALSE)

# quick look at the plot
disc_welfarist_sum_iso3c_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "disc_welfarist_sum_iso3c_plot", disc_welfarist_sum_iso3c_plot)


##############
# UNDISCOUNTED EXTRA-WELFARIST: UNDISCOUNTED VSLYs
##############

sum_undiscvsly_iso3c <- vsly %>%
  group_by(iso3c, replicate) %>%
  summarise(vsly_undiscounted = sum(vsly_undiscounted, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(vsly_undiscounted,
           list(
             low = lf,
             med = mf,
             high = hf
           ))) %>%
  filter(if_any(c(vsly_undiscounted_low, vsly_undiscounted_med, vsly_undiscounted_high), ~ . != 0))

# our results table which we can then save in the tables directory
sum_undiscvsly_iso3c
write.csv(sum_undiscvsly_iso3c, "analysis/tables/ssum_undiscvsly_iso3c.csv")

# rename to match the world data column
sum_undiscvsly_iso3c <- sum_undiscvsly_iso3c %>%
  rename(iso_a3 = iso3c)

# merge the data
world_undiscvsly <- world %>%
  left_join(sum_undiscvsly_iso3c, by = "iso_a3")

# plot the map
sum_undiscvsly_iso3c_plot <- ggplot(data = world_undiscvsly) +
  geom_sf(aes(fill = vsly_undiscounted_med)) +  # Ensure this column exists in your data
  scale_fill_gradientn(colors = palette, na.value = "grey90", name = "Extra-Welfarist Benefits") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
  ) +
  labs(
    title = "Undiscounted Extra-Welfarist Benefits",
    fill = "Undiscounted Extra-Welfarist Benefits ($)"
  ) +
  coord_sf(expand = FALSE)

# quick look at the plot
sum_undiscvsly_iso3c_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "sum_undiscvsly_iso3c_plot", sum_undiscvsly_iso3c_plot)


##############
# DISCOUNTED EXTRA-WELFARIST: DISCOUNTED VSLYs
##############

sum_discvsly_iso3c <- vsly %>%
  group_by(iso3c, replicate) %>%
  summarise(vsly_discounted = sum(vsly_discounted, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(vsly_discounted,
           list(
             low = lf,
             med = mf,
             high = hf
           ))) %>%
  filter(if_any(c(vsly_discounted_low, vsly_discounted_med, vsly_discounted_high), ~ . != 0))


# our results table which we can then save in the tables directory
sum_discvsly_iso3c
write.csv(sum_discvsly_iso3c, "analysis/tables/sum_discvsly_iso3c.csv")

# rename to match the world data column
sum_discvsly_iso3c <- sum_discvsly_iso3c %>%
  rename(iso_a3 = iso3c)

# merge the data
world_discvsly <- world %>%
  left_join(sum_discvsly_iso3c, by = "iso_a3")

# plot the map
sum_discvsly_iso3c_plot <- ggplot(data = world_discvsly) +
  geom_sf(aes(fill = vsly_discounted_med)) +  # Ensure this column exists in your data
  scale_fill_gradientn(colors = palette, na.value = "grey90", name = "Extra-Welfarist Benefits") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
  ) +
  labs(
    title = "Discounted Extra-Welfarist Benefits",
    fill = "Discounted Extra-Welfarist Benefits ($)"
  ) +
  coord_sf(expand = FALSE)

# quick look at the plot
sum_discvsly_iso3c_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "sum_discvsly_iso3c_plot", sum_discvsly_iso3c_plot)

combined_maps <- (undisc_welfarist_sum_iso3c_plot + disc_welfarist_sum_iso3c_plot + sum_undiscvsly_iso3c_plot + sum_discvsly_iso3c_plot) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Print the combined maps
print(combined_maps)



install.packages("patchwork")
library(patchwork)

# Combine the plots into a 2x2 panel with a shared legend
combined_maps <- (undisc_welfarist_sum_iso3c_plot +
                    disc_welfarist_sum_iso3c_plot +
                    sum_undiscvsly_iso3c_plot +
                    sum_discvsly_iso3c_plot) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Print the combined maps
print(combined_maps)



##################

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(patchwork)

# Define your color palette
palette <- met.brewer("Archambault")

# Load and prepare the world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge with the world map for each dataset
undisc_welfarist_sum_iso3c <- undisc_welfarist_sum_iso3c %>% rename(iso_a3 = iso3c)
world_undiscwelfarist <- world %>% left_join(undisc_welfarist_sum_iso3c, by = "iso_a3")

disc_welfarist_sum_iso3c <- disc_welfarist_sum_iso3c %>% rename(iso_a3 = iso3c)
world_discwelfarist <- world %>% left_join(disc_welfarist_sum_iso3c, by = "iso_a3")

sum_undiscvsly_iso3c <- sum_undiscvsly_iso3c %>% rename(iso_a3 = iso3c)
world_undiscvsly <- world %>% left_join(sum_undiscvsly_iso3c, by = "iso_a3")

sum_discvsly_iso3c <- sum_discvsly_iso3c %>% rename(iso_a3 = iso3c)
world_discvsly <- world %>% left_join(sum_discvsly_iso3c, by = "iso_a3")

# Calculate global min and max for color scale
combined_data <- bind_rows(
  world_undiscwelfarist %>% select(undisc_welf_total_med) %>% rename(value = undisc_welf_total_med),
  world_discwelfarist %>% select(disc_welf_total_med) %>% rename(value = disc_welf_total_med),
  world_undiscvsly %>% select(vsly_undiscounted_med) %>% rename(value = vsly_undiscounted_med),
  world_discvsly %>% select(vsly_discounted_med) %>% rename(value = vsly_discounted_med)
)

global_min <- min(combined_data$value, na.rm = TRUE)
global_max <- max(combined_data$value, na.rm = TRUE)

# Create the individual plots with the same color scale
undisc_welfarist_sum_iso3c_plot <- ggplot(data = world_undiscwelfarist) +
  geom_sf(aes(fill = undisc_welf_total_med)) +
  scale_fill_gradientn(colors = palette, limits = c(global_min, global_max), na.value = "grey90", name = "Welfarist Benefits ($)") +
  theme_minimal() +
  labs(title = "Undiscounted Welfarist Benefits") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  )

disc_welfarist_sum_iso3c_plot <- ggplot(data = world_discwelfarist) +
  geom_sf(aes(fill = disc_welf_total_med)) +
  scale_fill_gradientn(colors = palette, limits = c(global_min, global_max), na.value = "grey90", name = "Welfarist Benefits ($)") +
  theme_minimal() +
  labs(title = "Discounted Welfarist Benefits") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  )

sum_undiscvsly_iso3c_plot <- ggplot(data = world_undiscvsly) +
  geom_sf(aes(fill = vsly_undiscounted_med)) +
  scale_fill_gradientn(colors = palette, limits = c(global_min, global_max), na.value = "grey90", name = "Extra-Welfarist Benefits ($)") +
  theme_minimal() +
  labs(title = "Undiscounted Extra-Welfarist Benefits") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  )

sum_discvsly_iso3c_plot <- ggplot(data = world_discvsly) +
  geom_sf(aes(fill = vsly_discounted_med)) +
  scale_fill_gradientn(colors = palette, limits = c(global_min, global_max), na.value = "grey90", name = "Extra-Welfarist Benefits ($)") +
  theme_minimal() +
  labs(title = "Discounted Extra-Welfarist Benefits") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  )

# Combine the plots into a 2x2 panel with a shared legend
combined_maps <- (undisc_welfarist_sum_iso3c_plot +
                    disc_welfarist_sum_iso3c_plot +
                    sum_undiscvsly_iso3c_plot +
                    sum_discvsly_iso3c_plot) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Print the combined maps
print(combined_maps)
