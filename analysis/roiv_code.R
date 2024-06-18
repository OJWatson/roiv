setwd("~/documents/GitHub/roiv")
library(tidyverse)

# load in any of our own R functions that we use often (these are written and
# documented in the "/R" directory as we try and use a similar structure to
# R packages as it's good for reproducibility - see for example:
# https://annakrystalli.me/rrresearchACCE20/creating-a-research-compendium-with-rrtools.html)
devtools::load_all()

# *******************
# SOURCE_AVERTED_DCH
# *******************

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


# *******************************************************
# summary of life years, hospitalisations, and infections
# *******************************************************

# read in WHO life expectancy data from the data raw
# directory. This directory includes data that is sourced from
# elsewhere and should be treated as read only.

# https://apps.who.int/gho/data/node.imr.LIFE_0000000035?lang=en
lg <- read.csv("analysis/data/raw/lifeyearswho.csv")
lg <- lg %>%
  rename(iso3c = SpatialDimValueCode) %>%
  group_by(iso3c, Dim2) %>%
  summarise(ex = mean(Value))

# join these age groups and take the max
lg <- lg %>%
  mutate(
    Dim2 = replace(Dim2, Dim2 %in% c("<1 year", "1-4 years"), "0-4"),
    Dim2 = replace(Dim2, Dim2 %in% c("80-84 years", "85+ years"), "80+"),
  ) %>%
  group_by(iso3c, Dim2) %>%
  summarise(lg = max(ex))

# sort out the age labels to agree with the outputs from the modelling, e.g. 0-4
lg <- lg %>% mutate(Dim2 = factor(Dim2, levels = as.factor(Dim2)[c(1,10,2:9,11:17)])) %>%
  arrange(iso3c, Dim2)

lg$age_group <- c(levels(res_all$age_group %>% as.factor())[c(1,10,2:9,11:17)])[as.integer(lg$Dim2)]

# And join with our results table but with life expectancy (remaining years of life for this age group)
res_full <- left_join(res_all %>% ungroup, lg %>% ungroup %>% select(iso3c, age_group, lg)) %>%
  na.omit()

# now to add in population sizes used for the modelling
sqpop <- squire::population %>% select(iso3c, age_group, n) %>% rename(Ng = n)
sqpop$age_group <- c(levels(res_full$age_group %>% as.factor())[c(1,10,2:9,11:17)])[as.integer(sqpop$age_group)]
res_full <- left_join(res_full %>% ungroup, sqpop %>% ungroup, by = c("age_group", "iso3c"))

# calculate lghat - for discounting https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9889258/
res_full$y <- as.integer(factor(res_full$age_group, levels = levels(res_full$age_group %>% as.factor())[c(1,10,2:9,11:17)]))
res_full$lghat <- ((1)/((1+0.03)^res_full$y)) * res_full$lg
res_full <- group_by(res_full, iso3c, age_group) %>% mutate(lghat = sum(((1)/((1+0.03)^(1:round(unique(lg)))))))

# add world bank classifications for income group and region
res_full <- res_full %>%
  left_join(
    read_csv("analysis/data/raw/worldbank_classifications.csv"),
    by = "iso3c"
  )

# remove economy and lending category, and re-order columns
res_full <- res_full %>% select(country, iso3c, income_group, region, age_group, replicate, name, baseline, novaccine, averted, lg, Ng, y, lghat)

# adding new columns of total life years discounted and undiscounted life years for each age group
res_full <- res_full %>%
  mutate(lg_averted = averted * lg) %>%
  mutate(lghat_averted = averted * lghat)

# add uncertainty functions
lf <- function(x){quantile(x, 0.025, na.rm=TRUE)}
mf <- function(x){quantile(x, 0.5, na.rm=TRUE)}
hf <- function(x){quantile(x, 0.975, na.rm=TRUE)}

# create new data frame with lg_total each income group
lg_income <- res_full %>%
  filter(name == "deaths")%>%
  # here we group at iso3c, replicate, and age group
  group_by(iso3c, replicate, age_group) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  mutate(lg_total = sum(lg_averted)) %>%
  summarise(
  across(lg_total,
         list(
           low = lf,
           med = mf,
           high = hf
         )))


# our results table which we can then save in the tables directory
lg_income
write.csv(lg_income, "analysis/tables/lg_income.csv")

# create new data frame with lghat_total each income group
lghat_income <- res_full %>%
  filter(name == "deaths")%>%
  # here we group at iso3c, replicate, and age group
  group_by(iso3c, replicate, age_group) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  mutate(lghat_total = sum(lghat_averted)) %>%
  summarise(
    across(lghat_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
lghat_income
write.csv(lghat_income, "analysis/tables/lghat_income.csv")

# getting total hospitalisations per income group
summary_hospitalisations_income <- res_full %>%
  filter(name == "hospitalisations")%>%
  # here we group at iso3c and replicate, therefore summing over the age
  group_by(iso3c, replicate) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  mutate(hospitalisations_total = sum(averted)) %>%
  summarise(
    across(hospitalisations_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
summary_hospitalisations_income
write.csv(summary_hospitalisations_income, "analysis/tables/summary_hospitalisations_income.csv")

# infections averted for each income group
summary_infections_income_group <- res_full %>%
  filter(name == "infections")%>%
  # here we group at iso3c and replicate, therefore summing over the age
  group_by(iso3c, replicate) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  mutate(infections_total = sum(averted)) %>%
  summarise(
    across(infections_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
summary_infections_income_group
write.csv(summary_infections_income_group, "analysis/tables/summary_infections_income_group.csv")

# *****************
# VSLY calculations
# *****************

# read in GNI per capita and GDP data from World Bank

res_full <- res_full %>%
  left_join(
    read_csv("analysis/data/raw/GNIPC_2021.csv"),
    by = "iso3c"
    ) %>%
  left_join(
    read_csv("analysis/data/raw/GDP_iso3c.csv"),
    by = "iso3c"
  )

# read in USA VSL value
vsl_usa <- read_csv("analysis/data/raw/VSL_USA_2021.csv")
  # remove rows with all NA values
  vsl_usa <- vsl_usa %>%
    filter_all(any_vars(!is.na(.)))
  # remove columns with all NA values
  vsl_usa <- vsl_usa %>%
    select(which(colSums(is.na(.)) != nrow(vsl_usa)))

# extract USA VSL value (pulling value from data frame from column "mean" and row 1)
mean_vsl_usa <- vsl_usa$"mean"[1]

# extract USA GNIPC value
gnipc_usa <- res_full %>%
  filter(iso3c == "USA") %>%
  pull(gnipc) %>%
  first()


# make a new data frame by filtering only deaths from the main data frame to calculate vslys
vsly <- res_full %>%
  filter(name == "deaths")

# add a column to calculate VSL as part of the vsly calculation
vsly <- vsly %>%
  group_by(iso3c, replicate) %>%
  mutate(vsl = mean_vsl_usa*(gnipc/gnipc_usa)^1)

# add a column for vsly
vsly <- vsly %>%
  mutate(vsly_undiscounted = vsl * lg_averted) %>%
  mutate(vsly_discounted = vsl * lghat_averted)

# get vslys in terms of percent GDP
vsly <- vsly %>%
  mutate(vsly_percentgdp_undiscounted = (vsly_undiscounted/gdp)*100) %>%
  mutate(vsly_percentgdp_discounted = (vsly_discounted/gdp)*100)

# group by income group to get undiscounted VSLYs per income group in terms of mean percentage of GDP

vsly_income_undiscounted <- vsly %>%
  group_by(income_group, replicate) %>%
  mutate(vsly_percentgdp_undiscounted = mean(vsly_percentgdp_undiscounted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  summarise(
    across(vsly_percentgdp_undiscounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# drop rows with NA values in mean_hccosts_percentGDP
vsly_income_undiscounted <- vsly_income_undiscounted %>% drop_na()

# set the factor levels of income_group in the desired order
vsly_income_undiscountedp$income_group <- factor(vsly_income_undiscounted$income_group, levels = c("H", "UM", "LM", "L"))

# our results table which we can then save in the tables directory
vsly_income_undiscounted
write.csv(vsly_income_undiscounted, "analysis/tables/vsly_income_undiscounted.csv")


## Basic plot to compare these
vsly_income_undiscounted_plot <- vsly_income_undiscounted %>%
  ggplot(aes(x = income_group, y = vsly_percentgdp_undiscounted_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = vsly_percentgdp_undiscounted_low, ymax = vsly_percentgdp_undiscounted_high),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Value of Statistical Life Years Saved in Terms of Percentage of GDP") +
  xlab("World Bank Income Group") +
  scale_fill_viridis_d(name = "Income") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

# quick look at the plot
vsly_income_undiscounted_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "vvsly_income_undiscounted_plot",vsly_income_undiscounted_plot)

# group by income group to get discounted VSLYs per income group in terms of mean percentage of GDP
vsly_income_discounted <- vsly %>%
  group_by(income_group, replicate) %>%
  mutate(vsly_percentgdp_discounted = mean(vsly_percentgdp_discounted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  summarise(
    across(vsly_percentgdp_discounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# drop rows with NA values in mean_hccosts_percentGDP
vsly_income_discounted <- vsly_income_discounted %>% drop_na()

# set the factor levels of income_group in the desired order
vsly_income_discounted$income_group <- factor(vsly_income_discounted$income_group, levels = c("H", "UM", "LM", "L"))

# our results table which we can then save in the tables directory
vsly_income_discounted
write.csv(vsly_income_discounted, "analysis/tables/vsly_income_discounted.csv")

## Basic plot to compare these
vsly_income_discounted_plot <- vsly_income_discounted %>%
  ggplot(aes(x = income_group, y = vsly_percentgdp_discounted_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = vsly_percentgdp_discounted_low, ymax = vsly_percentgdp_discounted_high),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Value of Statistical Life Years Saved in Terms of Percentage of GDP") +
  xlab("World Bank Income Group") +
  scale_fill_viridis_d(name = "Income") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

# quick look at the plot
vsly_income_discounted_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "vvsly_income_discounted_plot",vsly_income_discounted_plot)

# ****************
# Monetized QALYs
# ****************



















