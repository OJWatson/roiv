hccosts_pp_gdppc_iso3c
undiscvsly_pp_gdppc_iso3c
discvsly_pp_gdppc_iso3c
undiscmonqaly_pp_gdppc_iso3c
discmonqaly_pp_gdppc_iso3c
friction_pp_gdppc_iso3c

##############
# UNDISCOUNTED EXTRAWELFARIST: UNDISCOUNTED MONETIZED QALYS, HUMAN CAPITAL COSTS, FRICTION COSTS, HEALTHCARE COSTS
##############

undisc_extrawelfarist_sum_iso3c <- sum_undiscmonqaly_iso3c %>%
  left_join(friction_sum_iso3c, by = "iso3c") %>%
  left_join(hc_costs_total_iso3c, by = "iso3c") %>%
  group_by(iso3c) %>%  # Ensure grouping by iso3c
  reframe(
    undisc_exwelf_total_low = sum(undiscmonqalys_averted_sum_low, friction_total_low, health_costs_total_low, na.rm = TRUE),
    undisc_exwelf_total_med = sum(undiscmonqalys_averted_sum_med, friction_total_med, health_costs_total_med, na.rm = TRUE),
    undisc_exwelf_total_high = sum(undiscmonqalys_averted_sum_high, friction_total_high, health_costs_total_high, na.rm = TRUE)
  ) %>%
  filter(if_any(c(undisc_exwelf_total_low, undisc_exwelf_total_med, undisc_exwelf_total_high), ~ . != 0))

# our results table which we can then save in the tables directory
undisc_extrawelfarist_sum_iso3c
write.csv(undisc_extrawelfarist_sum_iso3c, "analysis/tables/undisc_extrawelfarist_sum_iso3c.csv")

# get in terms of per person vaccinated as percentage of gdppc

undisc_exwelfarist_pp_gdppc_iso3c <- undisc_extrawelfarist_sum_iso3c %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by (iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)))%>%
  mutate(undisc_exwelf_low = (((undisc_exwelf_total_low / vaccines) / gdppc) * 100),
         undisc_exwelf_med = (((undisc_exwelf_total_med / vaccines) / gdppc) * 100),
         undisc_exwelf_high = (((undisc_exwelf_total_low / vaccines) / gdppc) * 100)) %>%
  select(iso3c, undisc_exwelf_low, undisc_exwelf_med, undisc_exwelf_high)

# our results table which we can then save in the tables directory
undisc_exwelfarist_pp_gdppc_iso3c
write.csv(undisc_exwelfarist_pp_gdppc_iso3c, "analysis/tables/undisc_exwelfarist_pp_gdppc_iso3c.csv")

##############
# DISCOUNTED EXTRAWELFARIST: DISCOUNTED MONETIZED QALYS, HUMAN CAPITAL COSTS, FRICTION COSTS, HEALTHCARE COSTS
##############


disc_exwelfarist_sum_iso3c <- sum_discmonqaly_iso3c %>%
  left_join(friction_sum_iso3c, by = "iso3c") %>%
  left_join(hc_costs_total_iso3c, by = "iso3c") %>%
  group_by(iso3c) %>%  # Ensure grouping by iso3c
  reframe(
    disc_exwelf_total_low = sum(discmonqalys_averted_sum_low, friction_total_low, health_costs_total_low, na.rm = TRUE),
    disc_exwelf_total_med = sum(discmonqalys_averted_sum_med, friction_total_med, health_costs_total_med, na.rm = TRUE),
    disc_exwelf_total_high = sum(discmonqalys_averted_sum_high, friction_total_high, health_costs_total_high, na.rm = TRUE)
  ) %>%
  filter(if_any(c(disc_exwelf_total_low, disc_exwelf_total_med, disc_exwelf_total_high), ~ . != 0))

# our results table which we can then save in the tables directory
disc_exwelfarist_sum_iso3c
write.csv(disc_exwelfarist_sum_iso3c, "analysis/tables/disc_exwelfarist_sum_iso3c.csv")


# get in terms of per person vaccinated as a percentage of gdppc
disc_exwelfarist_pp_gdppc_iso3c <- disc_exwelfarist_sum_iso3c %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by (iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)))%>%
  mutate(disc_exwelf_low = (((disc_exwelf_total_low / vaccines) / gdppc) * 100),
         disc_exwelf_med = (((disc_exwelf_total_med / vaccines) / gdppc) * 100),
         disc_exwelf_high = (((disc_exwelf_total_low / vaccines) / gdppc) * 100)) %>%
  select(iso3c, disc_exwelf_low, disc_exwelf_med, disc_exwelf_high)

# our results table which we can then save in the tables directory
disc_exwelfarist_pp_gdppc_iso3c
write.csv(disc_exwelfarist_pp_gdppc_iso3c, "analysis/tables/disc_exwelfarist_pp_gdppc_iso3c.csv")


##############
# UNDISCOUNTED WELFARIST
##############

undisc_welf_pp_pgdppc <- undiscvsly_pp_gdppc_iso3c

##############
# DISCOUNTED EXTRAWELFARIST
##############

disc_welf_pp_pgdppc <- discvsly_pp_gdppc_iso3c


################ two seperate maps###############

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(patchwork)
library(MetBrewer)  # Ensure this library is installed

# Define your color palettes
palette_welfarist <- met.brewer("Archambault")
palette_vsly <- met.brewer("Hokusai2")
palette_combined <- met.brewer("OKeeffe1")

# Load and prepare the world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Fix France
world <- world %>% mutate(iso_a3 = if_else(name_long == "France", "FRA", iso_a3))

# Merge with world map for each dataset
undisc_exwelfarist_pp_gdppc_iso3c <- undisc_exwelfarist_pp_gdppc_iso3c %>% rename(iso_a3 = iso3c)
world_undiscexwelfarist <- world %>% left_join(undisc_exwelfarist_pp_gdppc_iso3c, by = "iso_a3")

disc_exwelfarist_pp_gdppc_iso3c <- disc_exwelfarist_pp_gdppc_iso3c %>% rename(iso_a3 = iso3c)
world_discexwelfarist <- world %>% left_join(disc_exwelfarist_pp_gdppc_iso3c, by = "iso_a3")

undisc_welf_pp_pgdppc <- undisc_welf_pp_pgdppc %>% rename(iso_a3 = iso3c)
world_undiscwelf <- world %>% left_join(undisc_welf_pp_pgdppc, by = "iso_a3")

disc_welf_pp_pgdppc <- disc_welf_pp_pgdppc %>% rename(iso_a3 = iso3c)
world_discwelf <- world %>% left_join(disc_welf_pp_pgdppc, by = "iso_a3")

# Combine all data into one data frame
combined <- world %>%
  left_join(undisc_exwelfarist_pp_gdppc_iso3c, by = "iso_a3") %>%
  left_join(disc_exwelfarist_pp_gdppc_iso3c, by = "iso_a3", suffix = c("_undisc_exwelf", "_disc_exwelf")) %>%
  left_join(undisc_welf_pp_pgdppc, by = "iso_a3", suffix = c("", "_undisc_vsly")) %>%
  left_join(disc_welf_pp_pgdppc, by = "iso_a3", suffix = c("", "_disc_vsly"))


# Calculate min and max for color scales for Welfarist and Extra-Welfarist plots
min_exwelfarist <- min(c(
  combined$undisc_exwelf_med, combined$disc_exwelf_med
), na.rm = TRUE)
max_exwelfarist <- max(c(
  combined$undisc_exwelf_med, combined$disc_exwelf_med
), na.rm = TRUE)

min_welfarist <- min(c(
  combined$undiscvsly_pp_gdppc_med, combined$discvsly_pp_gdppc_med
), na.rm = TRUE)
max_welfarist <- max(c(
  combined$undiscvsly_pp_gdppc_med, combined$discvsly_pp_gdppc_med
), na.rm = TRUE)

# Create the Extra welfarist plots using a shared color scale and a single legend
undisc_exwelfarist_plot <- ggplot(data = combined) +
  geom_sf(aes(fill = undisc_exwelf_med)) +
  scale_fill_gradientn(colors = palette_vsly, limits = c(min_exwelfarist, max_exwelfarist), na.value = "grey90", name = "Benefits \n (per person vaccinated (% of GDPpc))") +
  theme_minimal() +
  labs(title = "Undiscounted Monetized Benefits") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)  # Smaller title
  )

disc_exwelfarist_plot <- ggplot(data = combined) +
  geom_sf(aes(fill = disc_exwelf_med)) +
  scale_fill_gradientn(colors = palette_vsly, limits = c(min_exwelfarist, max_exwelfarist), na.value = "grey90", name = "Benefits \n (per person vaccinated (% of GDPpc))") +
  theme_minimal() +
  labs(title = "Discounted Monetized Benefits") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)  # Smaller title
  )

# Create the Welfarist plots using a shared color scale and a single legend
undiscwelf_plot <- ggplot(data = combined) +
  geom_sf(aes(fill = undiscvsly_pp_gdppc_med)) +
  scale_fill_gradientn(colors = palette_vsly, limits = c(min_welfarist, max_welfarist), na.value = "grey90", name = "Benefits \n (per person vaccinated (% of GDPpc))") +
  theme_minimal() +
  labs(title = "Undiscounted Monetized Benefits") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)  # Smaller title
  )

discwelf_plot <- ggplot(data = combined) +
  geom_sf(aes(fill = discvsly_pp_gdppc_med)) +
  scale_fill_gradientn(colors = palette_vsly, limits = c(min_welfarist, max_welfarist), na.value = "grey90", name = "Benefits \n (per person vaccinated (% of GDPpc))") +
  theme_minimal() +
  labs(title = "Discounted Monetized Benefits") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)  # Smaller title
  )

# Combine the Extra Welfarist plots into a 1x2 panel with one legend at the bottom
exwelfarist_plot <- (undisc_exwelfarist_plot + disc_exwelfarist_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))  # Smaller panel title
  ) &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8, hjust = 0.5),  # Smaller legend title, centered
    legend.text = element_text(size = 6)
  )

# Combine the Welfarist plots into a 1x2 panel with one legend at the bottom
welfarist_plot <- (undiscwelf_plot + discwelf_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))  # Smaller panel title
  ) &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 8, hjust = 0.5),  # Smaller legend title, centered
    legend.text = element_text(size = 6)
  )

# Display the combined plots
print(exwelfarist_plot)
print(welfarist_plot)

# Save the figures to the plots directory using the save_figs function
# (assuming save_figs is a custom function from roiv package)
save_figs(name = "exwelfarist_plot", exwelfarist_plot)
save_figs(name = "welfarist_plot", welfarist_plot)
