library(tidyverse)
devtools::load_all()

setwd("~/documents/GitHub/roiv")

# read in our averted data that we create in the first script
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

# ^ res_full now has deaths, hospitalisations, infections for each country, age group and replicate, as well as number of people in each group, remaining life years discounted (lghat) and undiscounted lg

# and now we can work out our total value of life years gained for each income group
# helper functions for uncertainties
lf <- function(x){quantile(x, 0.025, na.rm=TRUE)}
mf <- function(x){quantile(x, 0.5, na.rm=TRUE)}
hf <- function(x){quantile(x, 0.975, na.rm=TRUE)}

# add in income groups
income_groups <- read_csv("analysis/data/raw/worldbank_classifications.csv")

# now, lets get the number of undiscounted life years gained for each income group
summary_yg_by_income <- res_full%>%
  filter(name == "deaths")%>%
  # now we need to calculate the life years gained by multiplying the life expectancy by the number of individuals
  mutate(y_gained = lg * averted) %>%
  # here we group at iso3c and replicate, therefore summing over the age
  group_by(iso3c, replicate) %>%
  # add in income groups
  left_join(income_groups, by = "iso3c") %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  mutate(y_gained_total = sum(y_gained)) %>%
  summarise(
    across(y_gained_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
summary_yg_by_income
write.csv(summary_yg_by_income, "analysis/tables/summary_yg_by_income.csv")

# now, lets get the number of undiscounted life years gained for each country
summary_yg_country <- res_full%>%
  filter(name == "deaths")%>%
  # now we need to calculate the life years gained by multiplying the life expectancy by the number of individuals
  mutate(y_gained = lg * averted) %>%
  # here we group at iso3c and replicate, therefore summing over the age
  group_by(iso3c, replicate) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(iso3c) %>%
  mutate(y_gained_total = sum(y_gained)) %>%
  summarise(
    across(y_gained,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
summary_yg_country
write.csv(summary_yg_country, "analysis/tables/summary_yg_country.csv")

# ^ re-do for discounted life years

# now, lets do the same for hospitalisations averted for each income group
summary_hospitalisations_by_income <- res_full %>%
  filter(name == "hospitalisations")%>%
  # here we group at iso3c and replicate, therefore summing over the age
  group_by(iso3c, replicate) %>%
  # add in income groups
  left_join(income_groups, by = "iso3c") %>%
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
summary_hospitalisations_by_income
write.csv(summary_hospitalisations_by_income, "analysis/tables/summary_hospitalisations_by_income.csv")

# now, lets do the same for hospitalisations averted for each country
summary_hospitalisations_country <- res_full %>%
  filter(name == "hospitalisations")%>%
  # here we group at iso3c and replicate, therefore summing over the age
  group_by(iso3c, replicate) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(iso3c) %>%
  mutate(hospitalisations_total = sum(averted)) %>%
  summarise(
    across(hospitalisations_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
summary_hospitalisations_country
write.csv(summary_hospitalisations_country, "analysis/tables/summary_hospitalisations_country.csv")

# now, lets do the same for infections averted for each income group

summary_infections_by_income <- res_all %>%
  filter(name == "infections")%>%
  # here we group at iso3c and replicate, therefore summing over the age
  group_by(iso3c, replicate) %>%
  # add in income groups
  left_join(income_groups, by = "iso3c") %>%
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
summary_infections_by_income
write.csv(summary_infections_by_income, "analysis/tables/summary_infections_by_income.csv")

# now, lets do the same for infections averted for each country

summary_infections_country <- res_all %>%
  filter(name == "infections")%>%
  # here we group at iso3c and replicate, therefore summing over the age
  group_by(iso3c, replicate) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(iso3c) %>%
  mutate(infections_total = sum(averted)) %>%
  summarise(
    across(infections_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
summary_infections_country
write.csv(summary_infections_country, "analysis/tables/summary_infections_country.csv")



