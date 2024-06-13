library(tidyverse)
devtools::load_all()

setwd("~/documents/GitHub/roiv")

# read in our averted data that we create in the first script
res_all <- readRDS("analysis/data/derived/averted_dch.rds")

# read in GDP values for WTP Thresholds

gdp <- read_csv("analysis/data/raw/GDP_iso3c.csv")

# read in WTP threshold GDP percentages

gdp_precentages_for_wtp_thresholds <- read_csv("analysis/data/raw/WTP_thresholds.csv")

# read in income group classifications

income_groups <- read_csv("analysis/data/raw/worldbank_classifications.csv")

# match countries gdp with their income level

gdp <- left_join(gdp, income_groups, by = "iso3c")

# get WTP thresholds for each country

economic_classification_data <- gdp %>%
  left_join(gdp_precentages_for_wtp_thresholds, by = "income_group") %>%
  mutate(median_wtp_threshold = median * GDP_2021) %>%
  mutate(lower_wtp_threshold = lower_IQR * GDP_2021) %>%
  mutate(upper_wtp_threshold = upper_IQR * GDP_2021)

wtp_thresholds <- economic_classification_data %>%
  select(iso3c, median_wtp_threshold, lower_wtp_threshold, upper_wtp_threshold)

# read in infection utility data

utility_values <- read_csv("analysis/data/raw/utility_values.csv")

# meta-analysis for utility data
install.packages("meta")
library(meta)
help(meta)

# read in infection duration and turn the days into years

infection_durations <- read_csv("analysis/data/raw/infection_durations.csv")

infection_durations <- infection_durations %>%
  mutate(duration_years = duration / 365)

# extracting values needed

duration_years_nonhospitalized <- infection_durations %>% filter(infection == "nonhospitalized") %>% pull(duration_years)
utility_nonhospitalized <- utility_values %>% filter(infection_type == "nonhospitalized") %>% pull(utility)

duration_years_hospitalized <- infection_durations %>% filter(infection == "hospitalized") %>% pull(duration_years)
utility_hospitalized <- utility_values %>% filter(infection_type == "hospitalised") %>% pull(utility)

# verifying the above are numeric values

duration_years_nonhospitalized <- as.numeric(duration_years_nonhospitalized)
utility_nonhospitalized <- as.numeric(utility_nonhospitalized)

duration_years_hospitalized <- as.numeric(duration_years_hospitalized)
utility_hospitalized <- as.numeric(utility_hospitalized)


# QALYs for infections

nonhospitalized_QALYs <- res_all %>%
  filter(name == "infections") %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_nonhospitalisations_infections = sum(averted)) %>%
  left_join(wtp_thresholds, by = "iso3c") %>%
  mutate(averted_nonhospitalisations_QALYs = averted_nonhospitalisations_infections * duration_years_nonhospitalized * utility_nonhospitalized * median_wtp_threshold)

# QALYs for hospitalisations

hospitalized_QALYs <- res_all %>%
  filter(name == "hospitalisations") %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_hospitalisations = sum(averted)) %>%
  left_join(wtp_thresholds, by = "iso3c") %>%
  mutate(averted_hospitalisations_QALYs = averted_hospitalisations * duration_years_nonhospitalized * utility_nonhospitalized * median_wtp_threshold)

# total QALYs

total_QALYs_averted <- nonhospitalized_QALYs %>%
  select(iso3c, replicate, averted_nonhospitalisations_QALYs) %>%
  left_join(hospitalized_QALYs %>% select(iso3c, replicate, averted_hospitalisations_QALYs), by = "iso3c") %>%
  mutate(total_averted_QALYs = averted_nonhospitalisations_QALYs + averted_hospitalisations_QALYs) %>%
  left_join(economic_classification_data %>% select(iso3c, region, income_group), by = "iso3c")

# remove rows with missing values in total_averted_QALYs column
total_QALYs_averted <- total_QALYs_averted[complete.cases(total_QALYs_averted$total_averted_QALYs),]

# add helper functions for uncertainties
lf <- function(x){quantile(x, 0.025, na.rm=TRUE)}
mf <- function(x){quantile(x, 0.5, na.rm=TRUE)}
hf <- function(x){quantile(x, 0.975, na.rm=TRUE)}

# group by region and calculate the sum of total_averted_QALYs
total_QALYs_averted_by_region <- total_QALYs_averted %>%
  group_by(region, replicate.x, replicate.y) %>%
  summarise(total_averted_QALYs = sum(total_averted_QALYs)) %>%
  group_by(region) %>%
  summarise(
    across(total_averted_QALYs,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# save into table
total_QALYs_averted_by_region
write.csv(total_QALYs_averted_by_region, "analysis/tables/total_QALYs_averted_by_region.csv")

# basic plot of monetized QALYs by region
total_QALYs_averted_by_region_chart <- total_QALYs_averted_by_region %>%
  ggplot(aes(x = region, y = total_averted_QALYs/1e9, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +
  ylab("Monetized QALYs Saved (Billion $)") +
  xlab("World Bank Region") +
  scale_fill_viridis_d(name = "Income") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

# view plot
total_QALYs_averted_by_region_chart

# save plot
save_figs(name = "monetised_QALYs_averted_by_region", total_QALYs_averted_by_region_chart)

# plotting monetized QALYs averted by geographic region
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# merge averted QALY data with the world map data
QALY_world_data <- world %>%
  left_join(total_QALYs_averted, by = c("iso_a3" = "iso3c"))

# plot the data
monetized_QALYs_averted_plot <- ggplot(data = QALY_world_data) +
  geom_sf(aes(fill = total_averted_QALYs)) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal() +
  labs(title = "Monetized QALYs Averted per Country",
       fill = "Monetized QALYs Averted")

# quick look at the plot
monetized_QALYs_averted_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "monetised_QALYs_averted_by_country", monetized_QALYs_averted_plot)



















