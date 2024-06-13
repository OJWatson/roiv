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
averted_infections_hospitalisations <- res_all %>%
  filter(name %in% c("infections", "hospitalisations"))

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

human_capital <- averted_infections_hospitalisations %>%
  left_join(oecd_wages, by = "iso3c")

# remove Unit column

human_capital <- human_capital %>% select(-Unit)

# add value for duration of illness

days_absent <- c(14)

# add income levels to the data frame

human_capital <- human_capital %>%
  left_join(income_groups, by = "iso3c")

# add a column to calculate human capital costs averted

human_capital <- human_capital %>%
  group_by(iso3c, replicate) %>%
  mutate(huca_costs_averted = (wage_USD2021 / 365) * days_absent * averted)


# adding in our uncertainty functions

lf <- function(x){quantile(x, 0.025, na.rm=TRUE)}
mf <- function(x){quantile(x, 0.5, na.rm=TRUE)}
hf <- function(x){quantile(x, 0.975, na.rm=TRUE)}

# summarising by income group

huca_costs_averted <- human_capital %>%
  group_by(income_group, replicate) %>%
  mutate(huca_costs_averted = sum(huca_costs_averted, na.rm = TRUE)) %>%
  group_by(income_group) %>%
    summarise(
      across(huca_costs_averted,
              list(
                low = lf,
                med = mf,
                high = hf
                )))

# our results table which we can then save in the tables directory

huca_costs_averted
write.csv(huca_costs_averted, "analysis/tables/huca_costs_averted.csv")


