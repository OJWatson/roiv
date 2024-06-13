# load any useful packages
library(tidyverse)

# load in any of our own R functions that we use often (these are written and
# documented in the "/R" directory as we try and use a similar structure to
# R packages as it's good for reproducibility - see for example:
# https://annakrystalli.me/rrresearchACCE20/creating-a-research-compendium-with-rrtools.html)
devtools::load_all()

# read in our averted data that we create in the first script
res_all <- readRDS("analysis/data/derived/averted_dch.rds")

# filtering it so it is just hospitalisations and infections
averted_deaths <- res_all %>%
  filter(name = "deaths")

# add in oecd wage data

oecd_wages <- read_csv("analysis/data/raw/oecd_wages.csv")

# converting 2022 wage data into 2021 USD

# read in GDP deflator values to convert wage data into 2021 USD, instead of 2022 USD
GDP_deflator <- read_csv("analysis/data/raw/GDP_deflator_worldbank.csv")

# select USA 2021 and USA 2022
USA_GDP_deflator2021 <- GDP_deflator %>% filter(iso3c == "USA") %>% pull("2021")
USA_GDP_deflator2022 <- GDP_deflator %>% filter(iso3c == "USA") %>% pull("2022")

# add column to convert wage data into 2021 USD, instead of 2022 USD

oecd_wages <- oecd_wages %>%
  mutate(wage_USD2021 = wage_USD2022 * (USA_GDP_deflator2021/USA_GDP_deflator2022))

# now, lets link wage data with infections and hospitalisations averted

friction_costs <- averted_deaths %>%
  left_join(oecd_wages, by = "iso3c")

# remove Unit column

friction_costs <- friction_costs %>% select(-Unit)

# add friction periods

# add column for friction costs averted

# get uncertainties

