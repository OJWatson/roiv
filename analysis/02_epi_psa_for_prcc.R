
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
  bs <- read_rds(paste0("https://github.com/mrc-ide/covid-vaccine-impact-orderly/raw/by_age/data/excess_mortality/counterfactual_data/Baseline_",iso3cs[i],".Rds"))
  no <- read_rds(paste0("https://github.com/mrc-ide/covid-vaccine-impact-orderly/raw/by_age/data/excess_mortality/counterfactual_data/No%20Vaccines_",iso3cs[i],".Rds"))


  # Delta Immune Escape Impact
  vaccine_efficacy <- fit$replicate_parameters$ves

  irs <- left_join(
    bs %>%
      filter(date >= as.Date(fit$interventions$date_vaccine_change[1])) %>%
      pivot_longer(infections:hospitalisations) %>%
      group_by(replicate, iso3c, name) %>%
      summarise(baseline = sum(value)),
    no %>%
      filter(date >= as.Date(fit$interventions$date_vaccine_change[1])) %>%
      pivot_longer(infections:hospitalisations) %>%
      group_by(replicate, iso3c, name) %>%
      summarise(novaccine = sum(value))
  ) %>%
    mutate(averted = novaccine - baseline) %>%
    select(replicate, iso3c, name, averted) %>%
    pivot_wider(names_from = name, values_from = averted) %>%
    mutate(ihr = hospitalisations/infections,
           ifr = deaths/infections)

  res[[i]] <- irs %>% left_join(
      data.frame(replicate = 1:100,
                 vaccine_efficacy = vaccine_efficacy)
    )

}

saveRDS(do.call(rbind, res), "analysis/data/derived/epi_psa.rds")
