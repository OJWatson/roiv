## goood
hc_costs <- res_full %>%
  filter(name == "hospitalisations")

# add in usa gdp deflator index 2019, 2021, 2022

usa_gdp_deflator2019 <- 107.3
usa_gdp_deflator2020 <- 108.7
usa_gdp_deflator2021 <- 113.6
usa_gdp_deflator2022 <- 121.6

# read in direct healthcare cost data from the data for lmics

lmic_hc_costs <- read_csv("analysis/data/raw/lmic_hc_costs.csv")

hc_costs_lmic <- merge(hc_costs, lmic_hc_costs, by = "iso3c", all = FALSE)

# convert costs into 2021 USD using GDP deflator index

hc_costs_lmic <- hc_costs_lmic %>%
  mutate(cost_pd2021 = cost_pd2019 *
           (usa_gdp_deflator2021/usa_gdp_deflator2019))

# calculate total costs averted by multiplying per day cost by hospitalisation
# duration by number of hospitalisations averted

hc_costs_lmic <- hc_costs_lmic %>%
  mutate(cost_total = cost_pd2021 * hospitalisations_duration * averted)

# do the name for non-european countries with missing healthcare data
# read in usa healthcare cost data for non-european high-income countries with missing data

usa_hc_costs <- read_csv("analysis/data/raw/usa_hc_costs.csv")

# convert the 2022 USD cost values to 2021 USD

usa_hc_costs <- usa_hc_costs %>%
  mutate(cost_2021 = cost_2022 * (usa_gdp_deflator2021/usa_gdp_deflator2022)) %>%
  mutate(upper_cost_2021 = upper_cost_2022 * (usa_gdp_deflator2021/usa_gdp_deflator2022)) %>%
  mutate(lower_cost_2021 = upper_cost_2022 * (usa_gdp_deflator2021/usa_gdp_deflator2022)) %>%
  mutate(hc_pgdppc = cost_2021/70219.47) %>% # USA GDP per capita (2021)
  mutate(hc_pgdppc_upper = upper_cost_2021/70219.47) %>%
  mutate(hc_pgdppc_lower = lower_cost_2021/70219.47)

# now apply these values and caluclate the healthcare costs for each high-income, non-european country
hc_costs_hic_noneurope <- hc_costs %>%
  filter(income_group == "H", region !="europe_central_asia") %>%
  left_join(usa_hc_costs, by = "iso3c") %>%
  mutate(hc_pgdppc = NA) %>%
  mutate(hc_pgdppc_upper = NA) %>%
  mutate(hc_pgdppc_lower = NA)

# use the US healthcare cost as a percentage of GDP per capita and apply it to other countries
hc_costs_hic_noneurope$hc_pgdppc[is.na(hc_costs_hic_noneurope$hc_pgdppc)] <-  usa_hc_costs$hc_pgdppc
hc_costs_hic_noneurope$hc_pgdppc_upper[is.na(hc_costs_hic_noneurope$hc_pgdppc_upper)] <- usa_hc_costs$hc_pgdppc_upper
hc_costs_hic_noneurope$hc_pgdppc_lower[is.na(hc_costs_hic_noneurope$hc_pgdppc_lower)] <- usa_hc_costs$hc_pgdppc_lower

# convert the costs to USD 2021

hc_costs_hic_noneurope <- hc_costs_hic_noneurope %>%
  mutate(cost_2021 = hc_pgdppc * gdppc) %>%
  mutate(upper_cost_2021 = hc_pgdppc_upper * gdppc) %>%
  mutate(lower_cost_2021 = hc_pgdppc_lower * gdppc)

# calculate the total cost

hc_costs_hic_noneurope <- hc_costs_hic_noneurope %>%
  mutate(cost_total = cost_2021 * hospitalisations_duration * averted) %>%
  mutate(upper_cost_total = upper_cost_2021 * hospitalisations_duration * averted) %>%
  mutate(lower_cost_total = lower_cost_2021 * hospitalisations_duration * averted)

# do the same for high income, european countries

# read in Spain healthcare cost data for european high-income countries with missing data
spain_hc_costs <- read_csv("analysis/data/raw/spain_hc_costs.csv")

spain_hc_costs <- spain_hc_costs %>%
  mutate(cost_2021 = mean_cost_usd2020 * (usa_gdp_deflator2021/usa_gdp_deflator2020)) %>%
  mutate(upper_cost_2021 = upper_cost_usd2020 * (usa_gdp_deflator2021/usa_gdp_deflator2020)) %>%
  mutate(lower_cost_2021 = upper_cost_usd2020 * (usa_gdp_deflator2021/usa_gdp_deflator2020)) %>%
  mutate(hc_pgdppc = cost_2021/30488.82) %>% # spain GDP per capita (2021)
  mutate(hc_pgdppc_upper = upper_cost_2021/30488.82) %>%
  mutate(hc_pgdppc_lower = lower_cost_2021/30488.82)

# now apply these values and caluclate the healthcare costs for each high-income, non-european country
hc_costs_hic_europe <- hc_costs %>%
  filter(income_group == "H", region == "europe_central_asia") %>%
  left_join(usa_hc_costs, by = "iso3c") %>%
  mutate(hc_pgdppc = NA) %>%
  mutate(hc_pgdppc_upper = NA) %>%
  mutate(hc_pgdppc_lower = NA)

# use the Spain healthcare cost as a percentage of GDP per capita and apply it to other countries
hc_costs_hic_europe$hc_pgdppc[is.na(hc_costs_hic_europe$hc_pgdppc)] <-  spain_hc_costs$hc_pgdppc
hc_costs_hic_europe$hc_pgdppc_upper[is.na(hc_costs_hic_europe$hc_pgdppc_upper)] <- spain_hc_costs$hc_pgdppc_upper
hc_costs_hic_europe$hc_pgdppc_lower[is.na(hc_costs_hic_europe$hc_pgdppc_lower)] <- spain_hc_costs$hc_pgdppc_lower

# convert the costs to USD 2021

hc_costs_hic_europe <- hc_costs_hic_europe %>%
  mutate(cost_2021 = hc_pgdppc * gdppc) %>%
  mutate(upper_cost_2021 = hc_pgdppc_upper * gdppc) %>%
  mutate(lower_cost_2021 = hc_pgdppc_lower * gdppc)

# calculate the total cost

hc_costs_hic_europe <- hc_costs_hic_europe %>%
  mutate(cost_total = cost_2021 * hospitalisations_duration * averted) %>%
  mutate(upper_cost_total = upper_cost_2021 * hospitalisations_duration * averted) %>%
  mutate(lower_cost_total = lower_cost_2021 * hospitalisations_duration * averted)

# now, need to group all the costs together

hc_costs_grouped <- bind_rows(hc_costs_hic_europe, hc_costs_hic_europe,
                              hc_costs_lmic)

# checking if they were grouped properly/data was assigned properly

num_rows_grouped <- nrow(hc_costs_grouped)

print(num_rows_grouped)

num_rows <- nrow(hc_costs)

print(num_rows)


## old ###

hc_costs <- res_full %>%
  filter(name == "hospitalisations")

# add in usa gdp deflator index 2019, 2021, 2022

usa_gdp_deflator2019 <- 107.3
usa_gdp_deflator2020 <- 108.7
usa_gdp_deflator2021 <- 113.6
usa_gdp_deflator2022 <- 121.6

# read in direct healthcare cost data from the data for lmics

lmic_hc_costs <- read_csv("analysis/data/raw/lmic_hc_costs.csv")

hc_costs_lmic <- merge(hc_costs, lmic_hc_costs, by = "iso3c", all = FALSE)

# convert costs into 2021 USD using GDP deflator index

hc_costs_lmic <- hc_costs_lmic %>%
  mutate(cost_pd2021 = cost_pd2019 *
           (usa_gdp_deflator2021/usa_gdp_deflator2019))

# calculate total costs averted by multiplying per day cost by hospitalisation
# duration by number of hospitalisations averted

hc_costs_lmic <- hc_costs_lmic %>%
  mutate(cost_total = cost_pd2021 * hospitalisations_duration * averted)

# do the name for non-european countries with missing healthcare data
# read in usa healthcare cost data for non-european high-income countries with missing data

usa_hc_costs <- read_csv("analysis/data/raw/usa_hc_costs.csv")

hc_costs_hic_noneurope <- hc_costs %>%
  filter(income_group == "H", region !="europe_central_asia") %>%
  left_join(usa_hc_costs, by = "iso3c")

# assign the USA values for the cost data

hc_costs_hic_noneurope$cost_2022[is.na(hc_costs_hic_noneurope$cost_2022)] <- 11275
hc_costs_hic_noneurope$upper_cost_2022[is.na(hc_costs_hic_noneurope$upper_cost_2022)] <- 11297
hc_costs_hic_noneurope$lower_cost_2022[is.na(hc_costs_hic_noneurope$lower_cost_2022)] <- 11252

# convert the costs to USD 2021

hc_costs_hic_noneurope <- hc_costs_hic_noneurope %>%
  mutate(cost_2021 = cost_2022 * (usa_gdp_deflator2021/usa_gdp_deflator2022)) %>%
  mutate(upper_cost_2021 = upper_cost_2022 * (usa_gdp_deflator2021/usa_gdp_deflator2022)) %>%
  mutate(lower_cost_2021 = upper_cost_2022 * (usa_gdp_deflator2021/usa_gdp_deflator2022))

# calculate the total cost

hc_costs_hic_noneurope <- hc_costs_hic_noneurope %>%
  mutate(cost_total = cost_2021 * hospitalisations_duration * averted) %>%
  mutate(upper_cost_total = upper_cost_2021 * hospitalisations_duration * averted) %>%
  mutate(lower_cost_total = lower_cost_2021 * hospitalisations_duration * averted)

# do the same for high income, european countries

# read in Spain healthcare cost data for european high-income countries with missing data

spain_hc_costs <- read_csv("analysis/data/raw/spain_hc_costs.csv")

hc_costs_hic_europe <- hc_costs %>%
  filter(income_group == "H", region =="europe_central_asia") %>%
  left_join(spain_hc_costs, by = "iso3c")

# assign the spain values for the cost data

hc_costs_hic_europe$mean_cost_usd2020[is.na(hc_costs_hic_europe$mean_cost_usd2020)] <- 6480.66
hc_costs_hic_europe$upper_cost_usd2020[is.na(hc_costs_hic_europe$upper_cost_usd2020)] <- 6521.63
hc_costs_hic_europe$lower_cost_usd2020[is.na(hc_costs_hic_europe$lower_cost_usd2020)] <- 6439.70

# convert the costs to USD 2021

hc_costs_hic_europe <- hc_costs_hic_europe %>%
  mutate(cost_2021 = mean_cost_usd2020 * (usa_gdp_deflator2021/usa_gdp_deflator2020)) %>%
  mutate(upper_cost_2021 = upper_cost_usd2020 * (usa_gdp_deflator2021/usa_gdp_deflator2020)) %>%
  mutate(lower_cost_2021 = upper_cost_usd2020 * (usa_gdp_deflator2021/usa_gdp_deflator2020))

# calculate the total cost

hc_costs_hic_europe <- hc_costs_hic_europe %>%
  mutate(cost_total = cost_2021 * hospitalisations_duration * averted) %>%
  mutate(upper_cost_total = upper_cost_2021 * hospitalisations_duration * averted) %>%
  mutate(lower_cost_total = lower_cost_2021 * hospitalisations_duration * averted)

# now, need to group all the costs together

hc_costs_grouped <- bind_rows(hc_costs_hic_europe, hc_costs_hic_europe,
                              hc_costs_lmic)

# checking if they were grouped properly/data was assigned properly

num_rows_grouped <- nrow(hc_costs_grouped)

print(num_rows_grouped)

num_rows <- nrow(hc_costs)

print(num_rows)
