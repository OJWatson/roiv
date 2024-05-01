# load any useful packages
library(tidyverse)

# load in any of our own R functions that we use often (these are written and
# documented in the "/R" directory as we try and use a similar structure to
# R packages as it's good for reproducibility - see for example:
# https://annakrystalli.me/rrresearchACCE20/creating-a-research-compendium-with-rrtools.html)
devtools::load_all()

# read in our averted data
res_all <- readRDS("analysis/data/derived/averted_dch.rds")


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

# add in gni
gni <- read.csv("analysis/data/raw/API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_4151702.csv")

# helper function to get last n values
tail_func <- function(x, n){
  if(length(x) == 0) {
    return(NA)
  } else {
    tail(x, n)
  }
}

# source the last GNI value for each country
res_full <- left_join(res_full,
                      gni %>% rowwise() %>%
  mutate(gni = tail_func(na.omit(c_across(X1960:X2021)),1)) %>%
  rename(iso3c = Country.Code) %>%
  select(iso3c, gni)
)

# calculate a value for VSL based on gni * 130. Middle estimate sourced from
# from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9889258/

# Hailie - instead if you have sourced values of VSL for each country then
# read this in here and use those. The following is just included here to help
# provide code to show you how to easily filter and group and create correct
# uncertainty intervals etc.
res_full <- res_full %>%
  mutate(vsl = gni*130) %>%
  group_by(iso3c, replicate) %>%
  mutate(w_nglghat = sum(Ng * lghat) / sum(Ng)) %>%
  mutate(vly = vsl / w_nglghat)

# and now we can work out our total value of lives lost
# helper functions for uncertainties
lf <- function(x){quantile(x, 0.025, na.rm=TRUE)}
mf <- function(x){quantile(x, 0.5, na.rm=TRUE)}
hf <- function(x){quantile(x, 0.975, na.rm=TRUE)}

# for example to calculate economic value of lives saved (flives) by income group
# see page 24 in SI of https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9889258/ for this
res_full$income <- squire.page::get_income_group(res_full$iso3c)

# Because value of lives is just something to be calculated based on deaths we start by
# just filtering to this:
income_flives <- res_full %>%
  filter(name == "deaths") %>%
  # calculate the discounted number of years lost given the averted deaths
  mutate(Y = averted * lghat) %>%
  # now the financial value based on the VLY
  mutate(flives = vly * Y) %>%
  # here we group at income and replicate, therefore summing over the age
  group_by(income, replicate) %>%
  summarise(flives = sum(flives, na.rm = TRUE),
            Y = sum(Y, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income) %>%
  summarise(
    across(flives,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
income_flives
write.csv(income_flives, "analysis/tables/monetised_life_years_saved_by_income.csv")

## Basic plot to compare these
gg1 <- income_flives %>%
  ggplot(aes(x = income, y = flives_med/1e9, fill = income)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +
  ylab("Monetised Life Years Saved (Billion $)") +
  xlab("World Bank Income Group") +
  scale_fill_viridis_d(name = "Income") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

# quick look at the plot
gg1

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "monetised_life_years_saved_by_income", gg1)


