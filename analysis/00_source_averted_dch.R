setwd("~/documents/GitHub/roiv")
library(tidyverse)
library("MetBrewer")

# read in the files for the model fits
url <- "https://github.com/mrc-ide/covid-vaccine-impact-orderly/tree/by_age/data/excess_mortality/counterfactual_data"

# All the iso3cs looked at from the raw data file (don't edit files in here - treat as read only)
iso3cs <- readLines("analysis/data/raw/iso3cs.txt")

# function to download and read the files
read_rds <- function(x){

  tf <- tempfile()
  tf2 <- download.file(x, tf)
  readRDS(tf)

}

# Helper functions for summarising the files
res <- vector("list", length = length(iso3cs))
mn <- function(x){mean(x, na.rm = TRUE)}
lq <- function(x){quantile(x, 0.025, na.rm = TRUE)}
uq <- function(x){quantile(x, 0.975, na.rm = TRUE)}

res <- vector("list", length = length(iso3cs))

for(i in seq_along(iso3cs)) {

  message(i)
  bs <- read_rds(paste0("https://github.com/mrc-ide/covid-vaccine-impact-orderly/raw/by_age/data/excess_mortality/counterfactual_data/Baseline_",iso3cs[i],".Rds"))
  no <- read_rds(paste0("https://github.com/mrc-ide/covid-vaccine-impact-orderly/raw/by_age/data/excess_mortality/counterfactual_data/No%20Vaccines_",iso3cs[i],".Rds"))

  # calculate the averted cases and deaths between the counterfactual with
  # no vaccines (no) and the baseline (bs)
  averted <- left_join(
    bs %>%
      pivot_longer(infections:hospitalisations) %>%
    group_by(country, iso3c, age_group, replicate, name) %>%
      summarise(baseline = sum(value)),
    no %>%
      pivot_longer(infections:hospitalisations) %>%
      group_by(country, iso3c, age_group, replicate, name) %>%
      summarise(novaccine = sum(value))
  ) %>%
    mutate(averted = novaccine - baseline)

  res[[i]] <- averted

}

# group all the results and save to the derived data folder (i.e. results we have generated from our R scripts)
res_all <- do.call(rbind, res)
saveRDS(res_all, "analysis/data/derived/averted_dch.rds")

