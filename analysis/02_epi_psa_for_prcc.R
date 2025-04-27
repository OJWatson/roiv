
library(tidyverse)

# read in the files for the model fits
url <- "https://github.com/mrc-ide/covid-vaccine-impact-orderly/raw/refs/heads/main/data/excess_mortality/model_fits/"
res_all <- readRDS("analysis/data/derived/averted_dch.rds")


# All the iso3cs looked at from the raw data file (don't edit files in here - treat as read only)
iso3cs <- readLines("analysis/data/raw/iso3cs.txt")

# function to download and read the files
read_rds <- function(x){

  tf <- tempfile()
  tf2 <- download.file(x, tf)
  readRDS(tf)

}

res <- vector("list", length = length(iso3cs))
for(i in seq_along(iso3cs)) {

  message(i)
  fit <- read_rds(paste0(url,iso3cs[i],".Rds"))

  # Delta Immune Escape Impact
  vaccine_efficacy <- fit$replicate_parameters$ves

  # ratios
  irs <- res_all %>% filter(iso3c == iso3cs[i]) %>% group_by(replicate, iso3c) %>%
    summarise(infections = sum(baseline[name == "infections"]),
              hospitalisations = sum(baseline[name == "hospitalisations"]),
              deaths = sum(baseline[name == "deaths"]),
              ihr = hospitalisations/infections,
              ifr = deaths/infections)

  res[[i]] <- irs %>% left_join(
      data.frame(replicate = 1:100,
                 vaccine_efficacy = vaccine_efficacy)
    )

}

saveRDS(do.call(rbind, res), "analysis/data/derived/epi_psa.rds")
