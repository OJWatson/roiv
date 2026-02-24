setwd("~/documents/GitHub/roiv")
library(tidyverse)
library("MetBrewer")

# load in any of our own R functions that we use often (these are written and
# documented in the "/R" directory as we try and use a similar structure to
# R packages as it's good for reproducibility - see for example:
# https://annakrystalli.me/rrresearchACCE20/creating-a-research-compendium-with-rrtools.html)
devtools::load_all()

# *******************************************************
# summary of life years, hospitalisations, and infections
# *******************************************************

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

# add world bank classifications for region
res_full <- res_full %>%
  left_join(
    read_csv("analysis/data/raw/worldbank_classifications.csv"),
    by = "iso3c"
  )

# change to be income groups from 2021
devtools::install_github("mrc-ide/squire")
devtools::install_github("mrc-ide/nimue")
devtools::install_github("mrc-ide/squire.page")

res_full$income_group <- squire.page::get_income_group(res_full$iso3c)

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

# create new data frame with lg_total within each age group in each income group
lg_age_income <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at income group, replicate, and age group
  group_by(income_group, replicate, age_group) %>%
  # get the sum of life years averted
  summarise(lg_total = sum(lg_averted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group, age_group) %>%
  summarise(
  across(lg_total,
         list(
           low = lf,
           med = mf,
           high = hf
         )))

# our results table which we can then save in the tables directory
lg_age_income
write.csv(lg_age_income, "analysis/tables/lg_age_income.csv")

# re-do it so that it is just by income group
lg_income <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at income group, replicate, and age group
  group_by(income_group, replicate) %>%
  # get the sum of life years averted
  summarise(lg_total = sum(lg_averted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  summarise(
    across(lg_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
lg_income$income_group <- factor(lg_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
lg_income <- lg_income %>%
  arrange(income_group)


# our results table which we can then save in the tables directory
lg_income
write.csv(lg_income, "analysis/tables/lg_income.csv")

# get undiscounted life years for each iso3c
lg_iso3c <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at iso3c, replicate
  group_by(iso3c, replicate) %>%
  # get the sum of life years averted
  summarise(lg_total = sum(lg_averted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(iso3c) %>%
  summarise(
    across(lg_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
lg_iso3c
write.csv(lg_iso3c, "analysis/tables/lg_iso3c.csv")

# creating data frame to set up per-person vaccinated calculations

vaccine_iso3c <- read_csv("analysis/data/raw/vac_cov.csv") %>%
  left_join(squire::population %>% group_by(iso3c) %>% summarise(Ng = sum(n))) %>%
  mutate(vaccines = vac_cov * Ng) %>%
  mutate(income_group = squire.page:::get_income_group(iso3c))

# population-weighted mean undiscounted life years per person vaccinated in each iso3c
lg_pp_iso3c <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at income, replicate
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(lg_averted = sum(lg_averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(lg_pp = lg_averted/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(lg_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
lg_pp_iso3c
write.csv(lg_pp_iso3c, "analysis/tables/lg_pp_iso3c.csv")

# re-do for income group
lg_pp_income <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at income, replicate
  group_by(income_group, replicate) %>% # (step 1)
  summarise(lg_averted = sum(lg_averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(lg_pp = lg_averted/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(lg_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
lg_pp_income$income_group <- factor(lg_pp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
lg_pp_income <- lg_pp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
lg_pp_income
write.csv(lg_pp_income, "analysis/tables/lg_pp_income.csv")

# get sum of undiscounted life years
lg_sum <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at replicate
  group_by(replicate) %>%
  # get the sum of life years averted
  summarise(lg_total = sum(lg_averted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  summarise(
    across(lg_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
lg_sum
write.csv(lg_sum, "analysis/tables/lg_sum.csv")

# life years per-person globally

total_vaccine <- vaccine_iso3c %>%
  summarise(vaccines = sum(vaccines, na.rm = TRUE))

total_vaccines <- 2415645947

lg_pp_world <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at income, replicate
  group_by(replicate) %>% # (step 1)
  summarise(lg_averted = sum(lg_averted,na.rm=TRUE)) %>%
  mutate(lg_pp = lg_averted/total_vaccines) %>%  # (step 3)
  summarise(
    across(lg_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
lg_pp_world
write.csv(lg_pp_world, "analysis/tables/lg_pp_world.csv")

# create new data frame with lghat_total within each age group in each income group
lghat_age_income <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at income group, replicate, and age group
  group_by(income_group, replicate, age_group) %>%
  # get the sum of life years averted
  summarise(lghat_total = sum(lghat_averted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group, age_group) %>%
  summarise(
    across(lghat_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
lghat_age_income
write.csv(lghat_age_income, "analysis/tables/lghat_age_income.csv")

# re-do it so that it is just by income group
lghat_income <- res_full %>%
  filter(name == "deaths")%>%
  # here we group at iso3c, replicate, and age group
  group_by(income_group, replicate) %>%
  # get the sum of life years averted
  summarise(lghat_total = sum(lghat_averted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
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

# get undiscounted life years for each iso3c
lghat_iso3c <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at iso3c, replicate
  group_by(iso3c, replicate) %>%
  # get the sum of life years averted
  summarise(lghat_total = sum(lghat_averted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(iso3c) %>%
  summarise(
    across(lghat_total,
             list(
               low = lf,
               med = mf,
               high = hf
             )))

# our results table which we can then save in the tables directory
lghat_iso3c
write.csv(lghat_iso3c, "analysis/tables/lghat_iso3c.csv")

# population-weighted mean discounted life years per person vaccinated in each iso3c
lghat_pp_iso3c <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at income, replicate
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(lg_averted = sum(lghat_averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(lg_pp = lg_averted/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(lg_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
lghat_pp_iso3c
write.csv(lghat_pp_iso3c, "analysis/tables/lghat_pp_iso3c.csv")

# re-do for income group
lghat_pp_income <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at income, replicate
  group_by(income_group, replicate) %>% # (step 1)
  summarise(lg_averted = sum(lghat_averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(lg_pp = lg_averted/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(lg_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
lghat_pp_income
write.csv(lghat_pp_income, "analysis/tables/lghat_pp_income.csv")

# get sum of undiscounted life years
lghat_sum <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at replicate
  group_by(replicate) %>%
  # get the sum of life years averted
  summarise(lghat_total = sum(lghat_averted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  summarise(
    across(lghat_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
lghat_sum
write.csv(lghat_sum, "analysis/tables/lghat_sum.csv")

# discounted life years per person globally

lghat_pp_world <- res_full %>%
  filter(name == "deaths") %>%
  # here we group at income, replicate
  group_by(replicate) %>% # (step 1)
  summarise(lghat_averted = sum(lghat_averted,na.rm=TRUE)) %>%
  mutate(lghat_pp = lghat_averted/total_vaccines) %>%  # (step 3)
  summarise(
    across(lghat_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
lghat_pp_world
write.csv(lghat_pp_world, "analysis/tables/lghat_pp_world.csv")




### HOSPITALISATIONS

# getting total hospitalisations per income group
sum_hosp_income <- res_full %>%
  filter(name == "hospitalisations")%>%
  # here we group at income, replicate
  group_by(income_group, replicate) %>%
  summarise(hosp_averted = sum(averted,na.rm=TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(hosp_averted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_hosp_income
write.csv(sum_hosp_income, "analysis/tables/sum_hosp_income.csv")

# population-weighted mean hospitalisations averted per person vaccinated in each iso3c
hosp_pp_iso3c <- res_full %>%
  filter(name == "hospitalisations")%>%
  # here we group at income, replicate
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(hosp_averted = sum(averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(hosp_pp = hosp_averted/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(hosp_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hosp_pp_iso3c
write.csv(hosp_pp_iso3c, "analysis/tables/hosp_pp_iso3c.csv")

# re-do for income group
hosp_pp_income <- res_full %>%
  filter(name == "hospitalisations")%>%
  # here we group at income, replicate
  group_by(income_group, replicate) %>% # (step 1)
  summarise(hosp_averted = sum(averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(hosp_pp = hosp_averted/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(hosp_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hosp_pp_income
write.csv(hosp_pp_income, "analysis/tables/hosp_pp_income.csv")

# hospitalisation sum
sum_hosp <- res_full %>%
  filter(name == "hospitalisations")%>%
  group_by(replicate) %>%
  summarise(hosp_averted = sum(averted,na.rm=TRUE)) %>%
  summarise(
    across(hosp_averted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_hosp
write.csv(sum_hosp, "analysis/tables/sum_hosp.csv")

# hospitalisations per person world
hosp_pp_world <- res_full %>%
  filter(name == "hospitalisations") %>%
  # here we group at income, replicate
  group_by(replicate) %>% # (step 1)
  summarise(hosp_averted = sum(averted,na.rm=TRUE)) %>%
  mutate(hosp_pp = hosp_averted/total_vaccines) %>%  # (step 3)
  summarise(
    across(hosp_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hosp_pp_world
write.csv(hosp_pp_world, "analysis/tables/hosp_pp_world.csv")

## Basic plot to compare these
palette <- met.brewer("Archambault")
sum_hosp_income_plot <- sum_hosp_income %>%
  ggplot(aes(x = income_group, y = hosp_averted_med/1e6, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = hosp_averted_low/1e6, ymax = hosp_averted_low/1e6),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Mean Hospitalisations Averted (in Millions)") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position.inside = c(0.87,0.87))

sum_hosp_income_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "sum_hosp_income_plot",sum_hosp_income_plot)

# infections averted for each income group
sum_inf_income <- res_full %>%
  filter(name == "infections")%>%
  # here we group at iso3c and replicate, therefore summing over the age
  group_by(income_group, replicate) %>%
  # get the sum of hospitalisations averted
  summarise(infections_total = sum(averted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  summarise(
    across(infections_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_inf_income
write.csv(sum_inf_income, "analysis/tables/sum_inf_income.csv")

# infections averted sum
sum_inf <- res_full %>%
  filter(name == "infections")%>%
  # here we group at iso3c and replicate, therefore summing over the age
  group_by(replicate) %>%
  # get the sum of hospitalisations averted
  summarise(infections_total = sum(averted, na.rm = TRUE)) %>%
  summarise(
    across(infections_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_inf
write.csv(sum_inf, "analysis/tables/sum_inf.csv")

# infections per person world
inf_pp_world <- res_full %>%
  filter(name == "infections") %>%
  # here we group at income, replicate
  group_by(replicate) %>% # (step 1)
  summarise(inf_averted = sum(averted, na.rm=TRUE)) %>%
  mutate(inf_pp = inf_averted/total_vaccines) %>%  # (step 3)
  summarise(
    across(inf_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
inf_pp_world
write.csv(inf_pp_world, "analysis/tables/inf_pp_world.csv")


## Basic plot to compare these
sum_inf_income_plot <- sum_inf_income %>%
  ggplot(aes(x = income_group, y = infections_total_med/1e6, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = infections_total_low/1e6, ymax = infections_total_high/1e6),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Mean Infections Averted (in Millions)") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position.inside = c(0.87,0.87))

sum_inf_income_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "sum_inf_income_plot",sum_inf_income_plot)

# population-weighted mean hospitalisations averted per person vaccinated in each iso3c
inf_pp_iso3c <- res_full %>%
  filter(name == "infections") %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(inf_averted = sum(averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(inf_pp = inf_averted/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(inf_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
inf_pp_iso3c
write.csv(inf_pp_iso3c, "analysis/tables/inf_pp_iso3c.csv")

# re-do for income group
inf_pp_income <- res_full %>%
  filter(name == "infections") %>%
  group_by(income_group, replicate) %>% # (step 1)
  summarise(inf_averted = sum(averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(inf_pp = inf_averted/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(inf_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
inf_pp_income
write.csv(inf_pp_income, "analysis/tables/inf_pp_income")

# *****************
# VSLY calculations
# *****************

# loading and prepping data frame
# read in GNI per capita and GDP data from World Bank and gdp per capita

res_full <- res_full %>%
  left_join(
    read_csv("analysis/data/raw/GNIPC_2021.csv"),
    by = "iso3c"
    ) %>%
  left_join(
    read_csv("analysis/data/raw/GDP_iso3c.csv"),
    by = "iso3c"
  ) %>%
  left_join(
    read_csv("analysis/data/raw/gdppc_2021.csv"),
    by = "iso3c")


# extract USA GNIPC value
gnipc_usa <- res_full %>%
  filter(iso3c == "USA") %>%
  pull(gnipc) %>%
  first()

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

# make a new data frame by filtering only deaths from the main data frame to calculate vslys
# and add life expectancies
vsly <- res_full %>%
  filter(name == "deaths") %>%
  left_join(read_csv("analysis/data/raw/life_expectancies.csv"), by = "iso3c")

# add a column to calculate VSL and conduct the vsly calculation using age group population weightings
vsly <- vsly %>%
  group_by(iso3c, replicate) %>%
  mutate(vsl = mean_vsl_usa*(gnipc/gnipc_usa)^1) %>%
  mutate(w_nglg = sum(Ng*lg) / sum(Ng)) %>%
  mutate(w_nglghat = sum(Ng * lghat) / sum(Ng)) %>%
  mutate(vly = vsl / w_nglg) %>%
  mutate(vly_disc = vsl / w_nglghat)


# getting total monetary value of vsly per income group (population-weighted)
vsly_avertedtotals_income <- vsly %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_undisc_averted = sum((lg_averted*vly), na.rm = TRUE),
            vsly_disc_averted = sum((lghat_averted*vly_disc), na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(vsly_undisc_averted:vsly_disc_averted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
vsly_avertedtotals_income
write.csv(vsly_avertedtotals_income, "analysis/tables/vsly_avertedtotals_income.csv")

# getting total monetary value of vsly per iso3c (population-weighted)
vsly_avertedtotals_iso3c <- vsly %>%
  group_by(iso3c, replicate) %>%
  summarise(vsly_undisc_averted = sum((lg_averted*vly), na.rm = TRUE),
            vsly_disc_averted = sum((lghat_averted*vly_disc), na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(vsly_undisc_averted:vsly_disc_averted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
vsly_avertedtotals_iso3c
write.csv(vsly_avertedtotals_iso3c, "analysis/tables/vsly_avertedtotals_iso3c.csv")

# totals worldwide
vsly_avertedtotals <- vsly %>%
  group_by(replicate) %>%
  summarise(vsly_undisc_averted = sum((lg_averted*vly), na.rm = TRUE),
            vsly_disc_averted = sum((lghat_averted*vly_disc), na.rm = TRUE)) %>%
  summarise(
    across(vsly_undisc_averted:vsly_disc_averted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
vsly_avertedtotals
write.csv(vsly_avertedtotals, "analysis/tables/vsly_avertedtotals.csv")

# get population-weighted GDP per income group
gdp <- vaccine_iso3c %>%
  left_join(read_csv("analysis/data/raw/GDP_iso3c.csv"), by = "iso3c") %>%
  select(iso3c, income_group, gdp)


# get gdp sums per income group
gdp_income <- gdp %>%
  group_by(income_group) %>%
  summarise(gdp = sum(gdp, na.rm = TRUE))


# vsly in terms of percentage of gdp for income groups
# undiscounted
undiscvsly_pgdp_income <- vsly %>%
  mutate(undiscvsly = (lg_averted * vly)) %>%
  group_by(income_group, replicate) %>%
  summarise(undiscvsly_total = sum(undiscvsly, na.rm = TRUE)) %>%
  left_join(gdp %>% group_by(income_group) %>% summarise(gdp = sum(gdp, na.rm = TRUE))) %>% #
  mutate(undiscvsly_pgdp = (undiscvsly_total/gdp) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(undiscvsly_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscvsly_pgdp_income
write.csv(undiscvsly_pgdp_income, "analysis/tables/undiscvsly_pgdp_income.csv")

# discounted
discvsly_pgdp_income <- vsly %>%
  mutate(discvsly = (lghat_averted * vly_disc)) %>%
  group_by(income_group, replicate) %>%
  summarise(discvsly_total = sum(discvsly, na.rm = TRUE)) %>%
  left_join(gdp %>% group_by(income_group) %>% summarise(gdp = sum(gdp, na.rm = TRUE))) %>% #
  mutate(discvsly_pgdp = (discvsly_total/gdp) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(discvsly_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discvsly_pgdp_income
write.csv(discvsly_pgdp_income, "analysis/tables/discvsly_pgdp_income.csv")

# getting vsly in terms of percentage of gdp for each iso3c
# undiscounted
undiscvsly_pgdp_iso3c <- vsly %>%
  mutate(vsly_undisc_pgdp = (lg_averted*vly / gdp) * 100) %>%
  group_by(iso3c, replicate) %>%
  summarise(vsly_undisc_pgdp = sum(vsly_undisc_pgdp)) %>%
  group_by(iso3c) %>%
  summarise(
    across(vsly_undisc_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscvsly_pgdp_iso3c
write.csv(undiscvsly_pgdp_iso3c, "analysis/tables/undiscvsly_pgdp_iso3c.csv")

# discounted
discvsly_pgdp_iso3c <- vsly %>%
  mutate(vsly_disc_pgdp = (lghat_averted*vly_disc / gdp) * 100) %>%
  group_by(iso3c, replicate) %>%
  summarise(vsly_disc_pgdp = sum(vsly_disc_pgdp)) %>%
  group_by(iso3c) %>%
  summarise(
    across(vsly_disc_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discvsly_pgdp_iso3c
write.csv(discvsly_pgdp_iso3c, "analysis/tables/discvsly_pgdp_iso3c.csv")


# making a stacked bar graph to show difference between undiscounted and discounted vslys
# reformatting the data frames
discvsly_pgdp_income <- discvsly_pgdp_income %>%
  rename(low = discvsly_pgdp_low,
         med = discvsly_pgdp_med,
         high = discvsly_pgdp_high) %>%
  mutate(type = "Discounted")

undiscvsly_pgdp_income <- undiscvsly_pgdp_income %>%
  rename(low = undiscvsly_pgdp_low,
         med = undiscvsly_pgdp_med,
         high = undiscvsly_pgdp_high) %>%
  mutate(type = "Undiscounted")

# combine the data frames and reshape it
vsly_pgdp_combined <- bind_rows(discvsly_pgdp_income, undiscvsly_pgdp_income)

palette_vsly <- met.brewer("Hiroshige")

vsly_pgdp_plot <- vsly_pgdp_combined %>%
  ggplot(aes(x = income_group)) +
  geom_bar(aes(y = med, fill = type), stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  geom_errorbar(aes(group = type, ymin = low, ymax = high), width = 0.4, position = position_dodge(width = 0.6)) +
  ylab("Value of Statistical Life Years Gained \n (% of GDP)") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette_vsly, name = "Type") +
  theme_bw(base_family = "Helvetica") +
  theme(
    legend.position = c(0.87, 0.87),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)  # Adjust margins as needed
  ) +
  ggtitle("Comparison of Undiscounted and Discounted VSLYs Gained \n per Income Group")

ggsave("analysis/plots/vsly_pgdp_plot.png", plot = vsly_pgdp_plot, width = 10, height = 6, dpi = 300)

vsly_pgdp_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "vsly_pgdp_plot", vsly_pgdp_plot)

## get VSLYs in terms of pp vaccinated

# undiscounted vsly gained pp vaccinated per iso3c
undiscvsly_pp_iso3c <- vsly %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(undiscvsly_total = sum(lg_averted*vly,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(undiscvsly_pp = undiscvsly_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(undiscvsly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscvsly_pp_iso3c
write.csv(undiscvsly_pp_iso3c, "analysis/tables/undiscvsly_pp_iso3c.csv")

# undiscounted vsly gained pp vaccinated per income group
undiscvsly_pp_income <- vsly %>%
  group_by(income_group, replicate) %>% # (step 1)
  summarise(undiscvsly_total = sum(lg_averted*vly,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(undiscvsly_pp = undiscvsly_total/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(undiscvsly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscvsly_pp_income
write.csv(undiscvsly_pp_income, "analysis/tables/undiscvsly_pp_income.csv")

# discounted vsly gained pp vaccinated per iso3c
discvsly_pp_iso3c <- vsly %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(discvsly_total = sum(lghat_averted*vly_disc,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(discvsly_pp = discvsly_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(discvsly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discvsly_pp_iso3c
write.csv(discvsly_pp_iso3c, "analysis/tables/discvsly_pp_iso3c.csv")

# discounted vsly gained pp vaccinated per income group
discvsly_pp_income <- vsly %>%
  group_by(income_group, replicate) %>% # (step 1)
  summarise(discvsly_total = sum((lghat_averted*vly_disc),na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(discvsly_pp = discvsly_total/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(discvsly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discvsly_pp_income
write.csv(discvsly_pp_income, "analysis/tables/discvsly_pp_income.csv")

# totals worldwide per person vaccinated
vsly_pp_world <- vsly %>%
  group_by(replicate) %>%
  summarise(vsly_undisc_averted = sum((lg_averted*vly), na.rm = TRUE),
            vsly_disc_averted = sum((lghat_averted*vly_disc), na.rm = TRUE)) %>%
  mutate(vsly_undisc_pp = vsly_undisc_averted / total_vaccines,
         vsly_disc_pp = vsly_disc_averted / total_vaccines) %>%
  summarise(
    across(vsly_undisc_pp:vsly_disc_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
vsly_pp_world
write.csv(vsly_pp_world, "analysis/tables/vsly_pp_world.csv")

## in terms of per-person vaccinated as a percentage of GDP per capita

# make gdppc data frame

gdppc <- vaccine_iso3c %>%
  left_join(read_csv("analysis/data/raw/gdppc_2021.csv"), by = "iso3c") %>%
  select(iso3c, income_group, gdppc)

gdppc_income <- gdppc %>%
  group_by(income_group) %>%
  summarise(gdppc = mean(gdppc, na.rm = TRUE))

# undiscounted - income
undiscvsly_pp_gdppc_income <- vsly %>%
  group_by(income_group, replicate) %>%
  summarise(undiscvsly_total = sum(lg_averted * vly, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "income_group") %>%
  left_join(gdppc %>% group_by(income_group) %>%
              summarise(gdppc = mean(gdppc, na.rm = TRUE)),
            by = "income_group") %>%
  mutate(undiscvsly_pp_gdppc = ((undiscvsly_total / vaccines) / gdppc) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(undiscvsly_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscvsly_pp_gdppc_income
write.csv(undiscvsly_pp_gdppc_income, "analysis/tables/undiscvsly_pp_gdppc_income.csv")

# for each country
undiscvsly_pp_gdppc_iso3c <- vsly %>%
  filter(!is.na(vly)) %>%
  group_by(iso3c, replicate) %>%
  summarise(undiscvsly_total = sum(lg_averted * vly, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>%
              group_by(iso3c) %>%
              summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "iso3c") %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = sum(gdppc, na.rm = TRUE)), by = "iso3c") %>%
  mutate(undiscvsly_pp_gdppc = ((undiscvsly_total / vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(undiscvsly_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscvsly_pp_gdppc_iso3c
write.csv(undiscvsly_pp_gdppc_iso3c, "analysis/tables/undiscvsly_pp_gdppc_iso3c.csv")

# do the same for discounted
discvsly_pp_gdppc_income <- vsly %>%
  group_by(income_group, replicate) %>%
  summarise(discvsly_total = sum(lghat_averted * vly_disc, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "income_group") %>%
  left_join(gdppc %>% group_by(income_group) %>%
              summarise(gdppc = mean(gdppc, na.rm = TRUE)),
            by = "income_group") %>%
  mutate(discvsly_pp_gdppc = ((discvsly_total / vaccines) / gdppc) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(discvsly_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
discvsly_pp_gdppc_income
write.csv(discvsly_pp_gdppc_income, "analysis/tables/discvsly_pp_gdppc_income.csv")

# for each country
discvsly_pp_gdppc_iso3c <- vsly %>%
  filter(!is.na(vly)) %>%
  group_by(iso3c, replicate) %>%
  summarise(discvsly_total = sum(lghat_averted * vly_disc, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>%
      group_by(iso3c) %>%
      summarise(vaccines = sum(vaccines, na.rm = TRUE)),
    by = "iso3c") %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE))) %>%
  mutate(discvsly_pp_gdppc = ((discvsly_total / vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(discvsly_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discvsly_pp_gdppc_iso3c
write.csv(discvsly_pp_gdppc_iso3c, "analysis/tables/discvsly_pp_gdppc_iso3c.csv")

# totals worldwide per person vaccinated as percentage of GDP per capita
# get world gdp per capita
gdppc_world <- gdp %>%
  left_join(squire::population %>% group_by(iso3c) %>% summarise(Ng = sum(n)))

total_gdp <- sum(gdppc_world$gdp, na.rm = TRUE)
total_population <- sum(gdppc_world$Ng, na.rm = TRUE)
world_gdppc <- total_gdp / total_population

# now get per person vaccinated as a % of gdp per capita
vsly_pp_gdppc_world <- vsly %>%
  group_by(replicate) %>%
  summarise(vsly_undisc_averted = sum((lg_averted*vly), na.rm = TRUE),
            vsly_disc_averted = sum((lghat_averted*vly_disc), na.rm = TRUE)) %>%
  mutate(vsly_undisc = ((vsly_undisc_averted / total_vaccines) / world_gdppc) * 100,
         vsly_disc = ((vsly_disc_averted / total_vaccines) / world_gdppc) * 100) %>%
  summarise(
    across(vsly_undisc:vsly_disc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
vsly_pp_gdppc_world
write.csv(vsly_pp_gdppc_world, "analysis/tables/vsly_pp_gdppc_world.csv")

# ****************
# Monetized QALYs
# ****************

# add percentages of GDP for WTP thresholds to res_full
res_full <- res_full %>%
  left_join(
  read_csv("analysis/data/raw/WTP_thresholds.csv") %>%
    set_names(c("income_group", "wtp_median", "wtp_lower_IQR", "wtp_upper_IQR")),
  by = "income_group")

# calculate wtp thresholds using the GDP percentages
res_full <- res_full %>%
  mutate(median_wtp_threshold = wtp_median * gdppc) %>%
  mutate(lower_wtp_threshold = wtp_lower_IQR * gdppc) %>%
  mutate(upper_wtp_threshold = wtp_upper_IQR * gdppc)

# read in qaly loss data
res_full <- res_full %>%
  left_join(
  read_csv("analysis/data/raw/qaly_losses.csv"),
  by = "name")

# read in duration data (in days)

infections_duration <- 5
hospitalisations_duration <- 12

# calculating number of QALYs averted for infections for each iso3c

inf_qaly_iso3c <- res_full %>%
  filter(name == "infections") %>%
  mutate(averted_inf_qalys = averted
         * -(qaly_loss)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_inf_qalys = sum(averted_inf_qalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_inf_qalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
inf_qaly_iso3c
write.csv(inf_qaly_iso3c, "analysis/tables/inf_qaly_iso3c.csv")

# calculating monetized QALYs averted for infections for each iso3c
inf_monqaly_iso3c <- res_full %>%
  filter(name == "infections") %>%
  mutate(averted_inf_qalys = averted
         * -(qaly_loss)) %>%
  mutate(averted_inf_monqalys = (averted_inf_qalys * median_wtp_threshold)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_inf_monqalys = sum(averted_inf_monqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_inf_monqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
inf_monqaly_iso3c
write.csv(inf_monqaly_iso3c, "analysis/tables/inf_monqaly_iso3c.csv")

# calculating number of QALYs averted for infections for each income group

inf_qaly_income <- res_full %>%
  filter(name == "infections") %>%
  mutate(averted_inf_qalys = averted *
           -(qaly_loss)) %>%
  group_by(income_group, replicate) %>%
  summarise(averted_inf_qalys = sum(averted_inf_qalys, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_inf_qalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
inf_qaly_income
write.csv(inf_qaly_income, "analysis/tables/inf_qaly_income.csv")

# calculating monetized QALYs averted for infections for each income group
inf_monqaly_income <- res_full %>%
  filter(name == "infections") %>%
  mutate(averted_inf_qalys = averted
         * -(qaly_loss)) %>%
  mutate(averted_inf_monqalys = averted_inf_qalys * median_wtp_threshold) %>%
  group_by(income_group, replicate) %>%
  summarise(averted_inf_monqalys = sum(averted_inf_monqalys, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_inf_monqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
inf_monqaly_income
write.csv(inf_monqaly_income, "analysis/tables/inf_monqaly_income.csv")

# caclulating number of QALYs averted for hospitalisations for each iso3c
hosp_qaly_iso3c <- res_full %>%
  filter(name == "infections") %>%
  mutate(averted_hosp_qalys = averted
         * -(qaly_loss)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_hosp_qalys = sum(averted_hosp_qalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_hosp_qalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hosp_qaly_iso3c
write.csv(hosp_qaly_iso3c, "analysis/tables/hosp_qaly_iso3c")

# calculating monetized QALYs averted for hospitalisations for each iso3c
hosp_monqaly_iso3c <- res_full %>%
  filter(name == "infections") %>%
  mutate(averted_hosp_qalys = averted
         * -(qaly_loss)) %>%
  mutate(averted_hosp_monqalys = averted_hosp_qalys * median_wtp_threshold) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_hosp_monqalys = sum(averted_hosp_monqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_hosp_monqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hosp_monqaly_iso3c
write.csv(hosp_monqaly_iso3c, "analysis/tables/hosp_monqaly_iso3c.csv")

# caclulating number of QALYs averted for hospitalisations for each income group
hosp_qaly_income <- res_full %>%
  filter(name == "infections") %>%
  mutate(averted_hosp_qalys = averted
         * -(qaly_loss)) %>%
  group_by(income_group, replicate) %>%
  summarise(averted_hosp_qalys = sum(averted_hosp_qalys, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_hosp_qalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hosp_qaly_income
write.csv(hosp_qaly_income, "analysis/tables/hosp_qaly_income.csv")

# calculating monetized QALYs averted for hospitalisations for each income group
hosp_monqaly_income <- res_full %>%
  filter(name == "infections") %>%
  mutate(averted_hosp_qalys = averted
         * -(qaly_loss)) %>%
  mutate(averted_hosp_monqalys = averted_hosp_qalys * median_wtp_threshold) %>%
  group_by(income_group, replicate) %>%
  summarise(averted_hosp_monqalys = sum(averted_hosp_monqalys, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_hosp_monqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hosp_monqaly_income
write.csv(hosp_monqaly_income, "analysis/tables/hosp_monqaly_income.csv")

# undiscounted death qalys
# calculating number of QALYs averted for deaths for each iso3c
deaths_undiscqaly_iso3c <- res_full %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_undiscqalys = ((averted * -qaly_loss) + lg_averted)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_deaths_undiscqalys = sum(averted_deaths_undiscqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_deaths_undiscqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_undiscqaly_iso3c
write.csv(deaths_undiscqaly_iso3c, "analysis/tables/deaths_undiscqaly_iso3c.csv")

# calculating monetized QALYs averted for deaths for each iso3c
deaths_undiscmonqaly_iso3c <- res_full %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_undiscqalys = ((averted * -qaly_loss) + lg_averted)) %>%
  mutate(averted_deaths_undiscmonqalys = (averted_deaths_undiscqalys * median_wtp_threshold)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_deaths_undiscmonqalys = sum(averted_deaths_undiscmonqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_deaths_undiscmonqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_undiscmonqaly_iso3c
write.csv(deaths_undiscmonqaly_iso3c, "analysis/tables/deaths_undiscmonqaly_iso3c.csv")

# calculating number of QALYs averted for deaths for each income group
deaths_undiscqaly_income <- res_full %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_undiscqalys = ((averted * -qaly_loss) + lg_averted)) %>%
  group_by(income_group, replicate) %>%
  summarise(averted_deaths_undiscqalys = sum(averted_deaths_undiscqalys, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_deaths_undiscqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_undiscqaly_income
write.csv(deaths_undiscqaly_income, "analysis/tables/deaths_undiscqaly_income.csv")

# calculating undiscounted monetized QALYs averted for deaths for each income group
deaths_undiscmonqaly_income <- res_full %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_undiscqalys = ((averted * -qaly_loss) + lg_averted)) %>%
  mutate(averted_deaths_undiscmonqalys = (averted_deaths_undiscqalys * median_wtp_threshold)) %>%
  group_by(income_group, replicate) %>%
  summarise(averted_deaths_undiscmonqalys = sum(averted_deaths_undiscmonqalys, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_deaths_undiscmonqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_undiscmonqaly_income
write.csv(deaths_undiscmonqaly_income, "analysis/tables/deaths_undiscmonqaly_income.csv")

# sum of all undiscounted qalys for infections, hospitalisations, deaths
sum_undiscqaly_iso3c <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted))
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(undiscqalys_averted_sum = sum(qalys_averted, na.rm=TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(undiscqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_undiscqaly_iso3c
write.csv(sum_undiscqaly_iso3c, "analysis/tables/sum_undiscqaly_iso3c.csv")

# get qaly's per-person vaccinated
undiscqaly_pp_iso3c <- res_full %>%
  mutate(undiscqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted))
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(undiscqalys_averted = sum(undiscqalys_averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(undiscqaly_pp = undiscqalys_averted / vaccines) %>%
  group_by(iso3c) %>%
  summarise(
    across(undiscqaly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscqaly_pp_iso3c
write.csv(undiscqaly_pp_iso3c, "analysis/tables/undiscqaly_pp_iso3c.csv")

# sum of all monetized undiscounted qalys for infections, hospitalisations, deaths
sum_undiscmonqaly_iso3c <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(undiscmonqalys_averted_sum = sum(undiscmonqalys_averted, na.rm=TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(undiscmonqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_undiscmonqaly_iso3c
write.csv(sum_undiscmonqaly_iso3c, "analysis/tables/sum_undiscmonqaly_iso3c.csv")


# sum of qalys for infections, hospitalisations, deaths per income group
sum_undiscqaly_income <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted))
  )) %>%
  group_by(income_group, replicate) %>%
  summarise(undiscqalys_averted_sum = sum(qalys_averted, na.rm=TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(undiscqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
sum_undiscqaly_income$income_group <- factor(sum_undiscqaly_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_undiscqaly_income <- sum_undiscqaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_undiscqaly_income
write.csv(sum_undiscqaly_income, "analysis/tables/sum_undiscqaly_income.csv")

# sum world wide
sum_undiscqaly <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted))
  )) %>%
  group_by(replicate) %>%
  summarise(undiscqalys_averted_sum = sum(qalys_averted, na.rm=TRUE)) %>%
  summarise(
    across(undiscqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_undiscqaly
write.csv(sum_undiscqaly, "analysis/tables/sum_undiscqaly.csv")

# undiscounted qalys per person worldwide
sum_undiscqaly_pp <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted))
  )) %>%
  group_by(replicate) %>%
  summarise(undiscqalys_averted_sum = sum(qalys_averted, na.rm=TRUE)) %>%
  mutate(undiscqalys_pp = undiscqalys_averted_sum / total_vaccines) %>%
  summarise(
    across(undiscqalys_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_undiscqaly_pp
write.csv(sum_undiscqaly_pp, "analysis/tables/sum_undiscqaly_pp.csv")


# get qaly's per-person vaccinated per income group
undiscqaly_pp_income <- res_full %>%
  mutate(undiscqaly_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted))
  )) %>%
  group_by(income_group, replicate) %>%
  summarise(undiscqaly_averted = sum(undiscqaly_averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(undiscqaly_pp = undiscqaly_averted / vaccines) %>%
  group_by(income_group) %>%
  summarise(
    across(undiscqaly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
undiscqaly_pp_income$income_group <- factor(undiscqaly_pp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
undiscqaly_pp_income <- undiscqaly_pp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
undiscqaly_pp_income
write.csv(undiscqaly_pp_income, "analysis/tables/undiscqaly_pp_income.csv")

# get total monetized qalys per income group
sum_undiscmonqaly_income <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(income_group, replicate) %>%
  summarise(undiscmonqalys_averted_sum = sum(undiscmonqalys_averted, na.rm=TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(undiscmonqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


sum_undiscmonqaly_income <- sum_undiscmonqaly_income %>%
  drop_na()

# set the factor levels of income_group in the desired order
sum_undiscmonqaly_income$income_group <- factor(sum_undiscmonqaly_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_undiscmonqaly_income <- sum_undiscmonqaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_undiscmonqaly_income
write.csv(sum_undiscmonqaly_income, "analysis/tables/sum_undiscmonqaly_income.csv")


# get mean undiscounted monetized qalys in percentage of gdp per income group
undiscmonqaly_pgdp_income <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(income_group, replicate) %>%
  summarise(undiscmonqaly_total = sum(undiscmonqalys_averted, na.rm = TRUE)) %>%
  left_join(gdp %>% group_by(income_group) %>% summarise(gdp = sum(gdp, na.rm = TRUE))) %>%
  mutate(undiscmonqalyspgdp = (undiscmonqaly_total/gdp) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(undiscmonqalyspgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

undiscmonqaly_pgdp_income


# set the factor levels of income_group in the desired order
undiscmonqaly_pgdp_income$income_group <- factor(undiscmonqaly_pgdp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
undiscmonqaly_pgdp_income <- undiscmonqaly_pgdp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
undiscmonqaly_pgdp_income
write.csv(undiscmonqaly_pgdp_income, "analysis/tables/undiscmonqaly_pgdp_income.csv")

# get mean monetized undiscounted QALYS as proportion of GDP per iso3c
undiscmonqaly_pgdp_iso3c <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  mutate(undiscmonqalys_pgdp = (undiscmonqalys_averted / gdp) * 100) %>%
  group_by(iso3c, replicate) %>%
  summarise(undiscmonqalys_pgdp = sum(undiscmonqalys_pgdp)) %>%
  group_by(iso3c) %>%
  summarise(
    across(undiscmonqalys_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
undiscmonqaly_pgdp_iso3c
write.csv(undiscmonqaly_pgdp_iso3c, "analysis/tables/undiscmonqaly_pgdp_iso3c.csv")

# discounted death qalys
# calculating number of QALYs averted for deaths for each iso3c

deaths_discqaly_iso3c <- res_full %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_discqalys = ((averted * -qaly_loss) + lghat_averted)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_deaths_discqalys = sum(averted_deaths_discqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_deaths_discqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_discqaly_iso3c
write.csv(deaths_discqaly_iso3c, "analysis/tables/deaths_discqaly_iso3c.csv")

# calculating monetized QALYs averted for deaths for each iso3c
deaths_discmonqaly_iso3c <- res_full %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_discqalys = ((averted * -qaly_loss) + lghat_averted)) %>%
  mutate(averted_deaths_discmonqalys = (averted_deaths_discqalys * median_wtp_threshold)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_deaths_discmonqalys = sum(averted_deaths_discmonqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_deaths_discmonqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_discmonqaly_iso3c
write.csv(deaths_discmonqaly_iso3c, "analysis/tables/deaths_discmonqaly_iso3c.csv")

# calculating number of QALYs averted for deaths for each income group
deaths_discqaly_income <- res_full %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_discqalys = ((averted * -qaly_loss) + lghat_averted)) %>%
  group_by(income_group, replicate) %>%
  summarise(averted_deaths_discqalys = sum(averted_deaths_discqalys)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_deaths_discqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
deaths_discqaly_income$income_group <- factor(deaths_discqaly_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
deaths_discqaly_income <- deaths_discqaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
deaths_discqaly_income
write.csv(deaths_discqaly_income, "analysis/tables/deaths_discqaly_income.csv")

# calculating monetized QALYs averted for deaths for each income group
deaths_discmonqaly_income <- res_full %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_discqalys = ((averted * -qaly_loss) + lghat_averted)) %>%
  mutate(averted_deaths_discmonqalys = (averted_deaths_discqalys * median_wtp_threshold)) %>%
  group_by(income_group, replicate) %>%
  summarise(averted_deaths_discmonqalys = sum(averted_deaths_discmonqalys, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_deaths_discmonqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
deaths_discmonqaly_income$income_group <- factor(deaths_discmonqaly_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
deaths_discmonqaly_income <- deaths_discmonqaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
deaths_discmonqaly_income
write.csv(deaths_discmonqaly_income, "analysis/tables/deaths_discmonqaly_income.csv")

# sum of all discounted qalys for infections, hospitalisations, deaths
sum_discqaly_iso3c <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted))
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(discqalys_averted_sum = sum(qalys_averted, na.rm=TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(discqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_discqaly_iso3c
write.csv(sum_discqaly_iso3c, "analysis/tables/sum_discqaly_iso3c.csv")

# get discounted qaly's per-person vaccinated per iso3c
discqaly_pp_iso3c <- res_full %>%
  mutate(discqaly_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted))
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(disqaly_averted = sum(discqaly_averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  mutate(discqaly_pp = disqaly_averted / vaccines) %>%
  group_by(iso3c) %>%
  summarise(
    across(discqaly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
discqaly_pp_iso3c
write.csv(discqaly_pp_iso3c, "analysis/tables/discqaly_pp_iso3c.csv")

# sum of all monetized discounted qalys for infections, hospitalisations, deaths
sum_discmonqaly_iso3c <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(discmonqalys_averted_sum = sum(discmonqalys_averted, na.rm=TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(discmonqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
sum_discmonqaly_iso3c
write.csv(sum_discmonqaly_iso3c, "analysis/tables/sum_discmonqaly_iso3c.csv")

# sum of qalys for infections, hospitalisations, deaths per income group
sum_discqaly_income <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted))
  )) %>%
  group_by(income_group, replicate) %>%
  summarise(discqalys_averted_sum = sum(qalys_averted, na.rm=TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(discqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


sum_discqaly_income <- sum_discqaly_income %>%
  drop_na()

# set the factor levels of income_group in the desired order
sum_discqaly_income$income_group <- factor(sum_discqaly_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_discqaly_income <- sum_discqaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_discqaly_income
write.csv(sum_discqaly_income, "analysis/tables/sum_discqaly_income.csv")

# get discounted qaly's per-person vaccinated per income group
discqaly_pp_income <- res_full %>%
  mutate(discqaly_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted))
  )) %>%
  group_by(income_group, replicate) %>%
  summarise(discqaly_averted = sum(discqaly_averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  mutate(discqaly_pp = discqaly_averted / vaccines) %>%
  group_by(income_group) %>%
  summarise(
    across(discqaly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
discqaly_pp_income$income_group <- factor(discqaly_pp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
discqaly_pp_income <- discqaly_pp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
discqaly_pp_income
write.csv(discqaly_pp_income, "analysis/tables/discqaly_pp_income.csv")

# get total monetized qalys per income group
sum_discmonqaly_income <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(income_group, replicate) %>%
  summarise(discmonqalys_averted_sum = sum(discmonqalys_averted, na.rm=TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(discmonqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


sum_discmonqaly_income <- sum_discmonqaly_income %>%
  drop_na()

# set the factor levels of income_group in the desired order
sum_discmonqaly_income$income_group <- factor(sum_discmonqaly_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_discmonqaly_income <- sum_discmonqaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_discmonqaly_income
write.csv(sum_discmonqaly_income, "analysis/tables/sum_discmonqaly_income.csv")

# get mean monetized qalys in proportion of gdp per income group
discmonqaly_pgdp_income <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(income_group, replicate) %>%
  summarise(discmonqaly_total = sum(discmonqalys_averted, na.rm = TRUE)) %>%
  left_join(gdp %>% group_by(income_group) %>% summarise(gdp = sum(gdp, na.rm = TRUE))) %>%
  mutate(discmonqalys_pgdp = (discmonqaly_total/gdp) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(discmonqalys_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
discmonqaly_pgdp_income$income_group <- factor(discmonqaly_pgdp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
discmonqaly_pgdp_income <- discmonqaly_pgdp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
discmonqaly_pgdp_income
write.csv(discmonqaly_pgdp_income, "analysis/tables/discmonqaly_pgdp_income.csv")


# get mean monetized qalys in proportion of gdp per iso3c
discmonqaly_pgdp_iso3c <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold)
  )) %>%
  mutate(discmonqalys_pgdp = (discmonqalys_averted / gdp) * 100) %>%
  group_by(iso3c, replicate) %>%
  summarise(discmonqalys_pgdp = sum(discmonqalys_pgdp)) %>%
  group_by(iso3c) %>%
  summarise(
    across(discmonqalys_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discmonqaly_pgdp_iso3c
write.csv(discmonqaly_pgdp_iso3c, "analysis/tables/discmonqaly_pgdp_iso3c.csv")

# get in terms of per person vaccinated

# undiscounted monetized qalys gained pp vaccinated per iso3c
undiscmonqaly_pp_iso3c <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold))) %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(monqaly_total = sum(undiscmonqalys_averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(monqaly_pp = monqaly_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(monqaly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscmonqaly_pp_iso3c
write.csv(undiscmonqaly_pp_iso3c, "analysis/tables/undiscmonqaly_pp_iso3c.csv")

# undiscounted monetized qalys gained pp vaccinated per income group
undiscmonqaly_pp_income <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold))) %>%
  group_by(income_group, replicate) %>% # (step 1)
  summarise(undiscmonqalys_total = sum(undiscmonqalys_averted, na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(undiscmonqalys_pp = undiscmonqalys_total/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(undiscmonqalys_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
undiscmonqaly_pp_income$income_group <- factor(undiscmonqaly_pp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
undiscmonqaly_pp_income <- undiscmonqaly_pp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
undiscmonqaly_pp_income
write.csv(undiscmonqaly_pp_income, "analysis/tables/undiscmonqaly_pp_income.csv")

# discounted monetized qalys gained pp vaccinated per iso3c
discmonqaly_pp_iso3c <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold))) %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(discmonqaly_total = sum(discmonqalys_averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(discmonqaly_pp = discmonqaly_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(discmonqaly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discmonqaly_pp_iso3c
write.csv(discmonqaly_pp_iso3c, "analysis/tables/discmonqaly_pp_iso3c.csv")

# discounted monetized qalys gained pp vaccinated per income group
discmonqaly_pp_income <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold))) %>%
  group_by(income_group, replicate) %>% # (step 1)
  summarise(discmonqalys_total = sum(discmonqalys_averted, na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(discmonqalys_pp = discmonqalys_total/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(discmonqalys_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
discmonqaly_pp_income$income_group <- factor(discmonqaly_pp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
discmonqaly_pp_income <- discmonqaly_pp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
discmonqaly_pp_income
write.csv(discmonqaly_pp_income, "analysis/tables/discmonqaly_pp_income.csv")

# now as per-person vaccinated as a percentage of GDP per capita

# undiscounted - income
undiscmonqaly_pp_gdppc_income <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold))) %>%
  group_by(income_group, replicate) %>%
  summarise(undiscmonqalys_total = sum(undiscmonqalys_averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "income_group") %>%
  left_join(gdppc %>% group_by(income_group) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)), by = "income_group") %>%
  mutate(undiscmonqalys_pp_gdppc = ((undiscmonqalys_total / vaccines) / gdppc) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(undiscmonqalys_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
undiscmonqaly_pp_gdppc_income$income_group <- factor(undiscmonqaly_pp_gdppc_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
undiscmonqaly_pp_gdppc_income <- undiscmonqaly_pp_gdppc_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
undiscmonqaly_pp_gdppc_income
write.csv(undiscmonqaly_pp_gdppc_income, "analysis/tables/undiscmonqaly_pp_gdppc_income.csv")

# for each country
undiscmonqaly_pp_gdppc_iso3c <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold))) %>%
  group_by(iso3c, replicate) %>%
  summarise(undiscmonqalys_total = sum(undiscmonqalys_averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE))) %>%
  mutate(undiscmonqalys_pp_gdppc = ((undiscmonqalys_total / vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(undiscmonqalys_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscmonqaly_pp_gdppc_iso3c
write.csv(undiscmonqaly_pp_gdppc_iso3c, "undiscmonqaly_pp_gdppc_iso3c.csv")

# do the same for discounted
discmonqaly_pp_gdppc_income <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold))) %>%
  group_by(income_group, replicate) %>%
  summarise(discmonqalys_total = sum(discmonqalys_averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "income_group") %>%
  left_join(gdppc %>% group_by(income_group) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)), by = "income_group") %>%
  mutate(
    discmonqalys_pp_gdppc = ((discmonqalys_total / vaccines) / gdppc) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(discmonqalys_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
discmonqaly_pp_gdppc_income$income_group <- factor(discmonqaly_pp_gdppc_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
discmonqaly_pp_gdppc_income <- discmonqaly_pp_gdppc_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
discmonqaly_pp_gdppc_income
write.csv(discmonqaly_pp_gdppc_income, "analysis/tables/discmonqaly_pp_gdppc_income.csv")

# for each country
discmonqaly_pp_gdppc_iso3c <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold))) %>%
  group_by(iso3c, replicate) %>%
  summarise(
    discmonqalys_total = sum(discmonqalys_averted, na.rm = TRUE),
    gdppc = sum(gdppc, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>%
              group_by(iso3c) %>%
              summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "iso3c") %>%
  mutate(
    discmonqalys_pp_gdppc = ((discmonqalys_total / vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(discmonqalys_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discmonqaly_pp_gdppc_iso3c
write.csv(discmonqaly_pp_gdppc_iso3c, "discmonqaly_pp_gdppc_iso3c.csv")


# sum world wide
sum_discqaly <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted))
  )) %>%
  group_by(replicate) %>%
  summarise(discqalys_averted_sum = sum(qalys_averted, na.rm=TRUE)) %>%
  summarise(
    across(discqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_discqaly
write.csv(sum_discqaly, "analysis/tables/sum_discqaly.csv")

# discounted qalys per person worldwide
sum_discqaly_pp <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted))
  )) %>%
  group_by(replicate) %>%
  summarise(discqalys_averted_sum = sum(qalys_averted, na.rm=TRUE)) %>%
  mutate(discqalys_pp = discqalys_averted_sum / total_vaccines) %>%
  summarise(
    across(discqalys_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_discqaly_pp
write.csv(sum_discqaly_pp, "analysis/tables/sum_discqaly_pp.csv")

#### doing monetized qalys for worldwide
# undiscounted
# pgdp
undiscmonqaly_pgdp_world <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(replicate) %>%
  summarise(undiscmonqalys_averted_sum = sum(undiscmonqalys_averted, na.rm=TRUE)) %>%
  mutate(undiscmonqalys_pgdp = (undiscmonqalys_averted_sum / total_gdp) * 100) %>%
  summarise(
    across(undiscmonqalys_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscmonqaly_pgdp_world
write.csv(undiscmonqaly_pgdp_world, "analysis/tables/undiscmonqaly_pgdp_world.csv")

# pp
undiscmonqaly_pp_world <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(replicate) %>%
  summarise(undiscmonqalys_averted_sum = sum(undiscmonqalys_averted, na.rm=TRUE)) %>%
  mutate(undiscmonqalys_pp = (undiscmonqalys_averted_sum / total_vaccines)) %>%
  summarise(
    across(undiscmonqalys_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscmonqaly_pp_world
write.csv(undiscmonqaly_pp_world, "analysis/tables/undiscmonqaly_pp_world.csv")

# pp_gdppc
undiscmonqaly_pp_gdppc_world <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(replicate) %>%
  summarise(undiscmonqalys_averted_sum = sum(undiscmonqalys_averted, na.rm=TRUE)) %>%
  mutate(undiscmonqalys_pp_gdppc = ((undiscmonqalys_averted_sum / total_vaccines) / world_gdppc * 100)) %>%
  summarise(
    across(undiscmonqalys_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscmonqaly_pp_gdppc_world
write.csv(undiscmonqaly_pp_gdppc_world, "analysis/tables/undiscmonqaly_pp_gdppc_world.csv")

# discounted
# pgdp
discmonqaly_pgdp_world <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(replicate) %>%
  summarise(discmonqalys_averted_sum = sum(discmonqalys_averted, na.rm=TRUE)) %>%
  mutate(discmonqalys_pgdp = (discmonqalys_averted_sum / total_gdp) * 100) %>%
  summarise(
    across(discmonqalys_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discmonqaly_pgdp_world
write.csv(discmonqaly_pgdp_world, "analysis/tables/discmonqaly_pgdp_world.csv")

# pp
discmonqaly_pp_world <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(replicate) %>%
  summarise(discmonqalys_averted_sum = sum(discmonqalys_averted, na.rm=TRUE)) %>%
  mutate(discmonqalys_pp = (discmonqalys_averted_sum / total_vaccines)) %>%
  summarise(
    across(discmonqalys_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discmonqaly_pp_world
write.csv(discmonqaly_pp_world, "analysis/tables/discmonqaly_pp_world.csv")

# pp_gdppc
discmonqaly_pp_gdppc_world <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(replicate) %>%
  summarise(discmonqalys_averted_sum = sum(discmonqalys_averted, na.rm=TRUE)) %>%
  mutate(discmonqalys_pp_gdppc = ((discmonqalys_averted_sum / total_vaccines) / world_gdppc * 100)) %>%
  summarise(
    across(discmonqalys_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discmonqaly_pp_gdppc_world
write.csv(discmonqaly_pp_gdppc_world, "analysis/tables/discmonqaly_pp_gdppc_world.csv")

# ********************
# Human Capital Costs
# ********************

# calculating human capital costs
human_capital <- res_full %>%
  filter(name %in% c("infections", "hospitalisations")) %>%
  mutate(hc_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted
  ))

# generating table just for human capital costs averted by infections
human_capital_inf <- human_capital %>%
  filter(name == "infections") %>%
  group_by(income_group, replicate) %>%
  summarise(hc_costs_total = sum(hc_costs, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(across(hc_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# set the factor levels of income_group in the desired order
human_capital_inf$income_group <- factor(human_capital_inf$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
human_capital_inf <- human_capital_inf %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
human_capital_inf
write.csv(human_capital_inf, "analysis/tables/human_capital_inf.csv")


# generating table just for human capital costs averted by hospitalisations
human_capital_hosp <- human_capital %>%
  filter(name == "hospitalisations") %>%
  group_by(income_group, replicate) %>%
  summarise(hc_costs_total = sum(hc_costs, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(across(hc_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))


# set the factor levels of income_group in the desired order
human_capital_hosp$income_group <- factor(human_capital_hosp$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
human_capital_hosp <- human_capital_hosp %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
human_capital_hosp
write.csv(human_capital_hosp, "analysis/tables/human_capital_hosp.csv")


# generating table for the sum of human capital costs averted
human_capital_sum_income <- human_capital %>%
  group_by(income_group, replicate) %>%
  summarise(humcap_total = sum(hc_costs, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(across(humcap_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))


# set the factor levels of income_group in the desired order
human_capital_sum_income$income_group <- factor(human_capital_sum_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
human_capital_sum_income <- human_capital_sum_income %>%
  arrange(income_group)


# our results table which we can then save in the tables directory
human_capital_sum_income
write.csv(human_capital_sum_income, "analysis/tables/human_capital_sum_income.csv")

## Basic plot to compare human capital costs between income group
human_capital_sum_incomeplot <- human_capital_sum_income %>%
  ggplot(aes(x = income_group, y = humcap_total_med/1e9, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = humcap_total_low/1e9, ymax = humcap_total_high/1e9),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Human Capital Costs Averted (in Billions ($))") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.87,0.87))

# quick look at the plot
human_capital_sum_incomeplot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "human_capital_sum_incomeplot", human_capital_sum_incomeplot)

# generating table of human capital costs averted as a percentage of gdp per income group
human_capital_pgdp <- human_capital %>%
  group_by(income_group, replicate) %>%
  summarise(humcap_total = sum(hc_costs, na.rm = TRUE)) %>%
  left_join(gdp %>% group_by(income_group) %>% summarise(gdp = sum(gdp, na.rm = TRUE)), by = "income_group") %>%
  mutate(humcap_pgdp = (humcap_total / gdp) * 100) %>%
  group_by(income_group) %>%
  summarise(across(humcap_pgdp,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# set the factor levels of income_group in the desired order
human_capital_pgdp$income_group <- factor(human_capital_pgdp$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
human_capital_pgdp <- human_capital_pgdp %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
human_capital_pgdp
write.csv(human_capital_pgdp, "analysis/tables/human_capital_pgdp.csv")

## Basic plot to compare human capital costs between income group as percentage of gdp
human_capital_pgdp_plot <- human_capital_pgdp %>%
  ggplot(aes(x = income_group, y = humcap_pgdp_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = humcap_pgdp_low, ymax = humcap_pgdp_high),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Human Capital Costs Averted as a Percentage of GDP") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.87,0.87))

# quick look at the plot
human_capital_pgdp_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "human_capital_pgdp_plot", human_capital_pgdp_plot)

#### RE-DO FOR ISO3C

# generating table just for human capital costs averted by infections
human_capital_inf_iso3c <- human_capital %>%
  filter(name == "infections") %>%
  group_by(iso3c, replicate) %>%
  summarise(hc_costs_total = sum(hc_costs, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(across(hc_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
human_capital_inf_iso3c
write.csv(human_capital_inf_iso3c, "analysis/tables/human_capital_inf_iso3c.csv")


# generating table just for human capital costs averted by hospitalisations
human_capital_hosp_iso3c <- human_capital %>%
  filter(name == "hospitalisations") %>%
  group_by(iso3c, replicate) %>%
  summarise(hc_costs_total = sum(hc_costs, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(across(hc_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))


# our results table which we can then save in the tables directory
human_capital_hosp_iso3c
write.csv(human_capital_hosp_iso3c, "analysis/tables/human_capital_hosp_iso3c.csv")


# sum of infections and hospitalisations
human_capital_sum_iso3c <- res_full %>%
  filter(name %in% c("infections", "hospitalisations")) %>%
  mutate(hc_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(hc_costs_total = sum(hc_costs, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(across(hc_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
human_capital_sum_iso3c
write.csv(human_capital_sum_iso3c, "analysis/tables/human_capital_sum_iso3c.csv")

# get human capital costs in terms of percentage of gdp for each iso3c
human_capital_iso3c_pgdp <- res_full %>%
  filter(name %in% c("infections", "hospitalisations")) %>%
  mutate(hc_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted
  )) %>%
  mutate(humcap_pgdp = (hc_costs / gdp) * 100) %>%
  group_by(iso3c, replicate) %>%
  summarise(humcap_pgdp = sum(humcap_pgdp)) %>%
  group_by(iso3c) %>%
  summarise(across(humcap_pgdp,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
human_capital_iso3c_pgdp
write.csv(human_capital_iso3c_pgdp, "analysis/tables/human_capital_iso3c_pgdp.csv")

## in terms of per person vaccinated

# human capital costs averted pp vaccinated per iso3c
human_capital_pp_iso3c <- res_full %>%
  filter(name %in% c("infections", "hospitalisations")) %>%
  mutate(hc_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted
  )) %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(hc_costs_total = sum(hc_costs,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(hc_costs_pp = hc_costs_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(hc_costs_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
human_capital_pp_iso3c
write.csv(human_capital_pp_iso3c, "analysis/tables/human_capital_pp_iso3c.csv")

# human capital costs averted pp vaccinated per income group
human_capital_pp_income <- res_full %>%
  filter(name %in% c("infections", "hospitalisations")) %>%
  mutate(hc_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted
  )) %>%
  group_by(income_group, replicate) %>% # (step 1)
  summarise(hc_costs_total = sum(hc_costs,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(hc_costs_pp = hc_costs_total/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(hc_costs_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# set the factor levels of income_group in the desired order
human_capital_pp_income$income_group <- factor(human_capital_pp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
human_capital_pp_income <- human_capital_pp_income%>%
  arrange(income_group)

# our results table which we can then save in the tables directory
human_capital_pp_income
write.csv(human_capital_pp_income, "analysis/tables/human_capital_pp_income.csv")

# express as per person vaccinated as a percentage of GDP per capita

human_capital_pp_gdppc_income <- res_full %>%
  filter(name %in% c("infections", "hospitalisations")) %>%
  mutate(hc_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted
  )) %>%
  group_by(income_group, replicate) %>%
  summarise(hccosts_total = sum(hc_costs, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "income_group") %>%
  left_join(gdppc %>% group_by(income_group) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)), by = "income_group") %>%
  mutate(hccosts_pp_gdppc = ((hccosts_total / vaccines) / gdppc) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(hccosts_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
human_capital_pp_gdppc_income$income_group <- factor(human_capital_pp_gdppc_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
human_capital_pp_gdppc_income <- human_capital_pp_gdppc_income%>%
  arrange(income_group)

# our results table which we can then save in the tables directory
human_capital_pp_gdppc_income
write.csv(human_capital_pp_gdppc_income, "analysis/tables/human_capital_pp_gdppc_income.csv")

# re-do for each iso3c
human_capital_pp_gdppc_iso3c <- res_full %>%
  filter(name %in% c("infections", "hospitalisations")) %>%
  mutate(hc_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(hccosts_total = sum(hc_costs, na.rm = TRUE)) %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = sum(gdppc)), by = "iso3c") %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "iso3c") %>%
  mutate(hccosts_pp_gdppc = ((hccosts_total / vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(hccosts_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
human_capital_pp_gdppc_iso3c
write.csv(human_capital_pp_gdppc_iso3c, "analysis/tables/human_capital_pp_gdppc_iso3c.csv")

# redoing calculations for world
# sum
human_capital_sum <- res_full %>%
  filter(name %in% c("infections", "hospitalisations")) %>%
  mutate(hc_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted
  )) %>%
  group_by(replicate) %>%
  summarise(hc_costs_total = sum(hc_costs, na.rm = TRUE)) %>%
  summarise(across(hc_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
human_capital_sum
write.csv(human_capital_sum, "analysis/tables/human_capital_sum.csv")

# pgdp
humcap_pgdp_world <- res_full %>%
  filter(name %in% c("infections", "hospitalisations")) %>%
  mutate(hc_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted
  )) %>%
  group_by(replicate) %>%
  summarise(hc_costs_sum = sum(hc_costs, na.rm=TRUE)) %>%
  mutate(hc_pgdp = (hc_costs_sum / total_gdp) * 100) %>%
  summarise(
    across(hc_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
humcap_pgdp_world
write.csv(humcap_pgdp_world, "analysis/tables/humcap_pgdp_world.csv")

# pp
humcap_pp_world <- res_full %>%
  filter(name %in% c("infections", "hospitalisations")) %>%
  mutate(hc_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted
  )) %>%
  group_by(replicate) %>%
  summarise(hc_costs_sum = sum(hc_costs, na.rm=TRUE)) %>%
  mutate(hc_pp = (hc_costs_sum / total_vaccines)) %>%
  summarise(
    across(hc_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
humcap_pp_world
write.csv(humcap_pp_world, "analysis/tables/humcap_pp_world.csv")

# pp_gdppc
humcap_pp_gdppc <- res_full %>%
  filter(name %in% c("infections", "hospitalisations")) %>%
  mutate(hc_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted
  )) %>%
  group_by(replicate) %>%
  summarise(hc_costs_sum = sum(hc_costs, na.rm=TRUE)) %>%
  mutate(hc_costs_pp_gdppc = ((hc_costs_sum / total_vaccines) / world_gdppc * 100)) %>%
  summarise(
    across(hc_costs_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
humcap_pp_gdppc
write.csv(humcap_pp_gdppc, "analysis/tables/humcap_pp_gdppc.csv")

# ***************
# Friction Costs
# ***************

# create new data frame
friction_costs <- res_full %>%
  filter(name == "deaths")

# add in oecd friction periods

friction_costs <- friction_costs %>%
  left_join(
    read_csv("analysis/data/raw/oecd_friction_periods.csv"),
    by = "iso3c")

# assign friction period of 3 months to LMICs

lmic_income <- (3*30.417)

friction_costs$friction_period[friction_costs$income_group == "LIC"] <- lmic_income
friction_costs$friction_period[friction_costs$income_group == "LMIC"] <- lmic_income

# assign average friction period value to European countries

friction_costs$friction_period[is.na(friction_costs$friction_period) & friction_costs$region == "europe_central_asia"] <- 60.6
friction_costs$friction_sd[is.na(friction_costs$friction_sd) & friction_costs$region == "europe_central_asia"] <- 14.8

# assign average friction period value to non-European countries

friction_costs$friction_period[is.na(friction_costs$friction_period) & friction_costs$region != "europe_central_asia"] <- 61.0
friction_costs$friction_sd[is.na(friction_costs$friction_sd) & friction_costs$region != "europe_central_asia"] <- 9.4

# calculate friction costs averted

friction_costs <- friction_costs %>%
  mutate(friction_period = as.numeric(friction_period)) %>%
  mutate(friction_costs = (gdppc/365.25) * friction_period * averted)


# sum friction costs per income group

friction_costs_sum <- friction_costs %>%
  group_by(income_group, replicate) %>%
  summarise(friction_costs_total = sum(friction_costs, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(across(friction_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# set the factor levels of income_group in the desired order
friction_costs_sum$income_group <- factor(friction_costs_sum$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
friction_costs_sum <- friction_costs_sum %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
friction_costs_sum
write.csv(friction_costs_sum, "analysis/tables/friction_costs_sum.csv")

## Basic plot to compare human capital costs between income group
friction_costs_plot <- friction_costs_sum %>%
  ggplot(aes(x = income_group, y = friction_costs_total_med/1e9, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = friction_costs_total_low/1e9, ymax = friction_costs_total_high/1e9),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Friction Costs Averted (in Billions ($))") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.87,0.87))

# quick look at the plot
friction_costs_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "friction_costs_plot", friction_costs_plot)

# sum friction costs per income group as percentage of gdp

friction_costs_pgdp <- friction_costs %>%
  group_by(income_group, replicate) %>%
  summarise(friction_costs_total = sum(friction_costs, na.rm = TRUE)) %>%
  left_join(gdp %>% group_by(income_group) %>% summarise(gdp = sum(gdp, na.rm = TRUE))) %>%
  mutate(friction_costs_pgdp = (friction_costs_total / gdp) * 100) %>%
  group_by(income_group) %>%
  summarise(across(friction_costs_pgdp,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# set the factor levels of income_group in the desired order
friction_costs_pgdp$income_group <- factor(friction_costs_pgdp$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
friction_costs_pgdp <- friction_costs_pgdp %>%
  arrange(income_group)


# our results table which we can then save in the tables directory
friction_costs_pgdp
write.csv(friction_costs_pgdp, "analysis/tables/friction_costs_pgdp.csv")


## Basic plot to compare friction costs between income group as percentage of gdp
friction_costs_pgdp_plot <- friction_costs_pgdp %>%
  ggplot(aes(x = income_group, y = friction_costs_pgdp_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = friction_costs_pgdp_low, ymax = friction_costs_pgdp_high),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Friction Costs Averted as a Percentage of GDP") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.87,0.87))

# quick look at the plot
friction_costs_pgdp_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "friction_costs_pgdp_plot", friction_costs_pgdp_plot)


# calculating friction costs for each iso3c
friction_costs_sum_iso3c <- friction_costs %>%
  group_by(iso3c, replicate) %>%
  summarise(friction_costs_total = sum(friction_costs, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(across(friction_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
friction_costs_sum_iso3c
write.csv(friction_costs_sum_iso3c, "analysis/tables/friction_costs_sum_iso3c.csv")


# sum friction costs per iso3c as percentage of gdp
friction_costs_pgdp_iso3c <- friction_costs %>%
  group_by(iso3c, replicate) %>%
  summarise(friction_costs_total= sum(friction_costs)) %>%
  left_join(gdp %>% group_by(iso3c) %>% summarise(gdp = sum(gdp, na.rm = TRUE)), by = "iso3c") %>%
  mutate(friction_costs_pgdp = (friction_costs_total / gdp) * 100) %>%
  group_by(iso3c) %>%
  summarise(across(friction_costs_pgdp,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
friction_costs_pgdp_iso3c
write.csv(friction_costs_pgdp_iso3c, "analysis/tables/friction_costs_pgdp_iso3c.csv")

## in terms of per person vaccinated

# frctions costs averted pp vaccinated per iso3c
friction_costs_pp_iso3c <- friction_costs %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(friction_costs_total = sum(friction_costs,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(friction_costs_pp = friction_costs_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(friction_costs_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
friction_costs_pp_iso3c
write.csv(friction_costs_pp_iso3c, "analysis/tables/friction_costs_pp_iso3c.csv")

# frctions costs averted pp vaccinated per income group
friction_costs_pp_income <- friction_costs %>%
  group_by(income_group, replicate) %>% # (step 1)
  summarise(friction_costs_total = sum(friction_costs,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(friction_costs_pp = friction_costs_total/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(friction_costs_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
friction_costs_pp_income$income_group <- factor(friction_costs_pp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
friction_costs_pp_income <- friction_costs_pp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
friction_costs_pp_income
write.csv(friction_costs_pp_income, "analysis/tables/friction_costs_pp_income.csv")

# express as per person vaccinated as a percentage of GDP per capita

friction_pp_gdppc_income <- friction_costs %>%
  group_by(income_group, replicate) %>%
  summarise(friction_total = sum(friction_costs, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "income_group") %>%
  left_join(gdppc %>% group_by(income_group) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE))) %>%
  mutate(friction_pp_gdppc = ((friction_total / vaccines) / gdppc) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(friction_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
friction_pp_gdppc_income$income_group <- factor(friction_pp_gdppc_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
friction_pp_gdppc_income <- friction_pp_gdppc_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
friction_pp_gdppc_income
write.csv(friction_pp_gdppc_income, "analysis/tables/friction_pp_gdppc_income.csv")

# re-do for each iso3c
friction_pp_gdppc_iso3c <- friction_costs %>%
  group_by(iso3c, replicate) %>%
  summarise(friction_total = sum(friction_costs, na.rm = TRUE)) %>%
  left_join(read_csv("analysis/data/raw/gdppc_2021.csv"), by = "iso3c") %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "iso3c") %>%
  mutate(friction_pp_gdppc = ((friction_total / vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(friction_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
friction_pp_gdppc_iso3c
write.csv(friction_pp_gdppc_iso3c, "analysis/tables/friction_pp_gdppc_iso3c.csv")

# redoing calculations for world
# sum
friction_sum <- friction_costs %>%
  group_by(replicate) %>%
  summarise(friction_total = sum(friction_costs, na.rm = TRUE)) %>%
  summarise(across(friction_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
friction_sum
write.csv(friction_sum, "analysis/tables/friction_sum.csv")

# pgdp
friction_pgdp <- friction_costs %>%
  group_by(replicate) %>%
  summarise(friction_total = sum(friction_costs, na.rm = TRUE)) %>%
  mutate(friction_pgdp = (friction_total / total_gdp) * 100) %>%
  summarise(
    across(friction_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
friction_pgdp
write.csv(friction_pgdp, "analysis/tables/friction_pgdp.csv")

# pp
friction_pp <- friction_costs %>%
  group_by(replicate) %>%
  summarise(friction_total = sum(friction_costs, na.rm = TRUE)) %>%
  mutate(friction_pp = (friction_total / total_vaccines)) %>%
  summarise(
    across(friction_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
friction_pp
write.csv(friction_pp, "analysis/tables/friction_pp.csv")

# pp_gdppc
friction_pp_gdppc <- friction_costs %>%
  group_by(replicate) %>%
  summarise(friction_total = sum(friction_costs, na.rm = TRUE)) %>%
  mutate(friction_pp_gdppc = ((friction_total / total_vaccines) / world_gdppc * 100)) %>%
  summarise(
    across(friction_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
friction_pp_gdppc
write.csv(friction_pp_gdppc, "analysis/tables/friction_pp_gdppc.csv")


# ********************
# Healthcare Costs
# ********************

hc_costs <- res_full %>%
  filter(name == "hospitalisations")

# add in usa gdp deflator index 2010, 2019, 2021, 2022

usa_gdp_deflator2010 <- 92.1
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

# read in high income country hc costs

hic_hccosts <- read_csv("analysis/data/raw/hic_hccosts.csv")

# convert into usd 2021

usa_gdp_deflator2010 <- 92.1

hic_hccosts <- hic_hccosts %>%
  mutate(mean_hccostpd_usd2021 = mean_hccostpd_id * (usa_gdp_deflator2021/usa_gdp_deflator2010)) %>%
  mutate(upper_hccostspd_usd2021 = upper_hccostspd_id * (usa_gdp_deflator2021/usa_gdp_deflator2010)) %>%
  mutate(lower_hccostspd_usd2021 = lower_hccostspd_id * (usa_gdp_deflator2021/usa_gdp_deflator2010))

# convert to total costs by multiplying by hospitalisation duration

hic_hccosts <- hic_hccosts %>%
  mutate(mean_hccosts = mean_hccostpd_usd2021 * hospitalisations_duration) %>%
  mutate(upper_hccosts = upper_hccostspd_usd2021 * hospitalisations_duration) %>%
  mutate(lower_hccosts = lower_hccostspd_usd2021 * hospitalisations_duration)

# merge the total costs with res_full
# filter res_full to just be hospitalisations and high income countries
hic_hosp <- res_full %>%
  filter(name == "hospitalisations", income_group == "HIC")

# make data frame that is just the 2021 costs
hic_hccosts_2021 <- hic_hccosts %>%
  select(iso3c, mean_hccosts, upper_hccosts, lower_hccosts)

# merge the two new data frames
hic_hccosts_2021 <- hic_hosp %>%
  left_join(hic_hccosts_2021, by = "iso3c")

# calculate total costs based on number of hospitalisations averted
hic_hccosts_2021 <- hic_hccosts_2021 %>%
  mutate(cost_total = mean_hccosts * averted) %>%
  mutate(uppercost_total = upper_hccosts * averted) %>%
  mutate(lowercost_total = lower_hccosts * averted)

# bind with lmic costs

hc_costs_grouped <- bind_rows(hic_hccosts_2021, hc_costs_lmic)

# get healthcare costs as a % of GDP
# sum costs averted for each income group

hc_costs_total <- hc_costs_grouped  %>%
  group_by(income_group, replicate) %>%
  summarise(hc_costs_total = sum(cost_total)) %>%
  group_by(income_group) %>%
  summarise(across(hc_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

#set the factor levels of income_group in the desired order
hc_costs_total$income_group <- factor(hc_costs_total$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
hc_costs_total <- hc_costs_total %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
hc_costs_total
write.csv(hc_costs_total, "analysis/tables/hc_costs_total.csv")

# get mean % of GDP for each income group
hccosts_pgdp <- hc_costs_grouped %>%
  group_by(income_group, replicate) %>%
  summarise(hccosts_total = sum(cost_total, na.rm = TRUE)) %>%
  left_join(gdp %>% group_by(income_group) %>% summarise(gdp = sum(gdp, na.rm = TRUE))) %>%
  mutate(hccosts_pgdp = (hccosts_total / gdp) * 100) %>%
  group_by(income_group) %>%
  summarise(across(hccosts_pgdp,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))


# set the factor levels of income_group in the desired order
hccosts_pgdp$income_group <- factor(hccosts_pgdp$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
hccosts_pgdp <- hccosts_pgdp %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
hccosts_pgdp
write.csv(hccosts_pgdp, "analysis/tables/hccosts_pgdp.csv")

# create bar graph to express healthcare costs averted per income group as a percentage of gdp
healthcarecosts_pgdp_plot <- hccosts_pgdp %>%
  ggplot(aes(x = income_group, y = hccosts_pgdp_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = hccosts_pgdp_low, ymax = hccosts_pgdp_high),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Healthcare Costs Averted (% GDP)") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.87,0.87))

# quick look at the chart
healthcarecosts_pgdp_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "healthcarecosts_pgdp_plot", healthcarecosts_pgdp_plot, plot_dir = "/Users/halliebenjamin/Documents/GitHub/roiv/analysis/plots")


# sum costs averted for each iso3c
hc_costs_total_iso3c <- hc_costs_grouped  %>%
  group_by(iso3c, replicate) %>%
  summarise(health_costs_total = sum(cost_total)) %>%
  group_by(iso3c) %>%
  summarise(across(health_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))


# our results table which we can then save in the tables directory
hc_costs_total_iso3c
write.csv(hc_costs_total_iso3c, "analysis/tables/hc_costs_total_iso3c.csv")

# get mean % of GDP for each iso3c
hccosts_pgdp_iso3c <- hc_costs_grouped %>%
  group_by(iso3c, replicate) %>%
  summarise(hccosts_total = sum(cost_total), na.rm = TRUE) %>%
  left_join(gdp %>% group_by(iso3c) %>% summarise(gdp = sum(gdp, na.rm = TRUE)), by = "iso3c") %>%
  mutate(hccosts_pgdp = (hccosts_total/gdp) * 100) %>%
  group_by(iso3c) %>%
  summarise(across(hccosts_pgdp,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
hccosts_pgdp_iso3c
write.csv(hccosts_pgdp_iso3c, "analysis/tables/hccosts_pgdp_iso3c.csv")

# healthcare costs averted pp vaccinated per iso3c
hccosts_pp_iso3c <- hc_costs_grouped %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(hccosts_total = sum(cost_total,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(hccosts_pp = hccosts_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(hccosts_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hccosts_pp_iso3c
write.csv(friction_costs_pp_iso3c, "analysis/tables/hccosts_pp_iso3c.csv")

# healthcare costs averted pp vaccinated per iso3c
hccosts_pp_income <- hc_costs_grouped %>%
  group_by(income_group, replicate) %>% # (step 1)
  summarise(hccosts_total = sum(cost_total,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(hccosts_pp = hccosts_total/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(hccosts_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
hccosts_pp_income$income_group <- factor(hccosts_pp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
hccosts_pp_income <- hccosts_pp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
hccosts_pp_income
write.csv(hccosts_pp_income, "analysis/tables/hccosts_pp_income.csv")


# get as per person vaccinated as a percentage of gdppc
# per iso3c
hccosts_pp_gdppc_iso3c <- hc_costs_grouped %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(hccosts_total = sum(cost_total,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)), by = "iso3c") %>%
  mutate(hccosts_pp_gdppc = ((hccosts_total/vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(hccosts_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hccosts_pp_gdppc_iso3c
write.csv(hccosts_pp_gdppc_iso3c, "analysis/tables/hccosts_pp_gdppc_iso3c.csv")

# per income group
hccosts_pp_gdppc_income <- hc_costs_grouped %>%
  group_by(income_group, replicate) %>% # (step 1)
  summarise(hccosts_total = sum(cost_total,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by(income_group) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)), by = "income_group") %>%
  mutate(hccosts_pp_gdppc = ((hccosts_total/vaccines) / gdppc) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(hccosts_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
hccosts_pp_gdppc_income$income_group <- factor(hccosts_pp_gdppc_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
hccosts_pp_gdppc_income <- hccosts_pp_gdppc_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
hccosts_pp_gdppc_income
write.csv(hccosts_pp_gdppc_income, "analysis/tables/hccosts_pp_gdppc_income.csv")


# redoing calculations for world
# sum
healthcare_sum <- hc_costs_grouped %>%
  group_by(replicate) %>%
  summarise(health_total = sum(cost_total, na.rm = TRUE)) %>%
  summarise(across(health_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
healthcare_sum
write.csv(healthcare_sum, "analysis/tables/healthcare_sum.csv")

# pgdp
healthcare_pgdp <- hc_costs_grouped %>%
  group_by(replicate) %>%
  summarise(health_total = sum(cost_total, na.rm = TRUE)) %>%
  mutate(health_pgdp = (health_total / total_gdp) * 100) %>%
  summarise(
    across(health_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
healthcare_pgdp
write.csv(healthcare_pgdp, "analysis/tables/healthcare_pgdp.csv")

# pp
healthcare_pp <- hc_costs_grouped %>%
  group_by(replicate) %>%
  summarise(health_total = sum(cost_total, na.rm = TRUE)) %>%
  mutate(health_pp = (health_total / total_vaccines)) %>%
  summarise(
    across(health_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
healthcare_pp
write.csv(healthcare_pp, "analysis/tables/healthcare_pp.csv")

# pp_gdppc
healthcare_pp_gdppc <- hc_costs_grouped %>%
  group_by(replicate) %>%
  summarise(health_total = sum(cost_total, na.rm = TRUE)) %>%
  mutate(health_pp_gdppc = ((health_total / total_vaccines) / world_gdppc * 100)) %>%
  summarise(
    across(health_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
healthcare_pp_gdppc
write.csv(healthcare_pp_gdppc, "analysis/tables/healthcare_pp_gdppc.csv")


# *************************
# Total Costs into Vaccines
# *************************

# delivery costs
del_cost_iso3c <- res_full %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(del_cost = vaccines * 3.70) %>%
  group_by(iso3c, replicate) %>%
  summarise(del_cost_total = sum(del_cost, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(across(del_cost_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))



# our results table which we can then save in the tables directory
del_cost_iso3c
write.csv(del_cost_iso3c, "analysis/tables/del_cost_iso3c.csv")

del_cost_income <- res_full %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(del_cost = vaccines * 3.70) %>%
  group_by(income_group, replicate) %>%
  summarise(del_cost_total = sum(del_cost, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(across(del_cost_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# set the factor levels of income_group in the desired order
del_cost_income$income_group <- factor(del_cost_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
del_cost_income <- del_cost_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
del_cost_income
write.csv(del_cost_income, "analysis/tables/del_cost_income.csv")

# get in-terms of pgdp
del_cost_pgdp_income <- res_full %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(del_cost = vaccines * 3.70) %>%
  mutate(del_cost_pgdp = (del_cost / gdp) * 100) %>%
  group_by(income_group, replicate) %>%
  summarise(del_cost_pgdp = sum((del_cost_pgdp * Ng) / sum(Ng, na.rm = TRUE), na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(across(del_cost_pgdp,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# set the factor levels of income_group in the desired order
del_cost_pgdp_income$income_group <- factor(del_cost_pgdp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
del_cost_pgdp_income <- del_cost_pgdp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
del_cost_pgdp_income
write.csv(del_cost_pgdp_income, "analysis/tables/del_cost_pgdp_income.csv")

# get sum of delivery costs across all income groups
del_cost <- vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE)) %>%
  mutate(del_cost = vaccines * 3.70) %>%
  summarise(del_cost = sum(del_cost))


# our results table which we can then save in the tables directory
del_cost
write.csv(del_cost, "analysis/tables/del_cost.csv")

del_cost <- 8937890003
dev_funding <- 14407549755.12 * (usa_gdp_deflator2021/usa_gdp_deflator2020)
apa <-  45442990000 * (usa_gdp_deflator2021/usa_gdp_deflator2020)
corporate <- 11000000000 * (1/0.845)
manu <- 380000000 * (usa_gdp_deflator2021/usa_gdp_deflator2020)


# ********************
# Return on Investment
# ********************

# welfarist - monetized QALYs, covid-19 healthcare costs, human capital costs, productivity losses

# sum of undiscounted monetized qalys

sum_undiscmonqaly <- res_full %>%
  mutate(monqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(replicate) %>%
  summarise(monqalys_averted_sum = sum(monqalys_averted, na.rm=TRUE)) %>%
  summarise(
    across(monqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# save results
sum_undiscmonqaly
write.csv(sum_undiscmonqaly, "analysis/tables/sum_undiscmonqaly.csv")

# sum of discounted monetized qalys
sum_discmonqaly <- res_full %>%
  mutate(monqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(replicate) %>%
  summarise(monqalys_averted_sum = sum(monqalys_averted, na.rm=TRUE)) %>%
  summarise(
    across(monqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# save results
sum_discmonqaly
write.csv(sum_discmonqaly, "analysis/tables/sum_discmonqaly.csv")

# sum of human capital costs
sum_humancapital <- human_capital %>%
  group_by(replicate) %>%
  summarise(humcap_total = sum(hc_costs, na.rm = TRUE)) %>%
  summarise(
    across(humcap_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# save results
sum_humancapital
write.csv(sum_humancapital, "analysis/tables/sum_humancapital.csv")

# sum of friction costs
sum_frictioncosts <- friction_costs %>%
  group_by(replicate) %>%
  summarise(friction_costs_total = sum(friction_costs, na.rm = TRUE)) %>%
  summarise(across(friction_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# save results
sum_frictioncosts
write.csv(sum_frictioncosts, "analysis/tables/sum_frictioncosts.csv")


# sum healthcare costs
sum_hc_costs <- hc_costs_grouped  %>%
  group_by(replicate) %>%
  summarise(hc_costs_total = sum(cost_total, na.rm = TRUE)) %>%
  summarise(across(hc_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# save results
sum_hc_costs
write.csv(sum_hc_costs, "analysis/tables/sum_hc_costs.csv")


# combine the summaries - undiscounted
welfarist_undiscsum <- bind_cols(sum_undiscmonqaly, sum_humancapital, sum_frictioncosts, sum_hc_costs) %>%
  summarise(
    total_low = monqalys_averted_sum_low + humcap_total_low + friction_costs_total_low + hc_costs_total_low,
    total_med = monqalys_averted_sum_med + humcap_total_med + friction_costs_total_med + hc_costs_total_med,
    total_high = monqalys_averted_sum_high + humcap_total_high + friction_costs_total_high + hc_costs_total_high
  )

# save results
welfarist_undiscsum
write.csv(welfarist_undiscsum, "analysis/tables/welfarist_undiscsum.csv")

# combine the summaries - discounted
welfarist_discsum <- bind_cols(sum_discmonqaly, sum_humancapital, sum_frictioncosts, sum_hc_costs) %>%
  summarise(
    total_low = monqalys_averted_sum_low + humcap_total_low + friction_costs_total_low + hc_costs_total_low,
    total_med = monqalys_averted_sum_med + humcap_total_med + friction_costs_total_med + hc_costs_total_med,
    total_high = monqalys_averted_sum_high + humcap_total_high + friction_costs_total_high + hc_costs_total_high
  )

# save results
welfarist_discsum
write.csv(welfarist_discsum, "analysis/tables/welfarist_discsum.csv")

# calculate undiscounted welfarist roi

roi_undiscwelfarist <- welfarist_undiscsum %>%
  mutate(roi_low = ((total_low - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_med = ((total_med - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_high = ((total_high - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_undiscwelfarist
write.csv(roi_undiscwelfarist, "analysis/tables/roi_undiscwelfarist.csv")

# calculate discounted welfarist roi

roi_discwelfarist <- welfarist_discsum %>%
  mutate(roi_low = ((total_low - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_med = ((total_med - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_high = ((total_high - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_discwelfarist
write.csv(roi_discwelfarist, "analysis/tables/roi_discwelfarist.csv")

# extra welfarist - VSLYs
# sum undiscounted and discounted vsly

undisc_extrawelfarist_sum <- vsly %>%
  group_by(replicate) %>%
  summarise(total = sum(lg_averted*vly, na.rm = TRUE)) %>%
  summarise(
    across(total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# save results
undisc_extrawelfarist_sum
write.csv(undisc_extrawelfarist_sum, "analysis/tables/undisc_extrawelfarist_sum.csv")

disc_extrawelfarist_sum <- vsly %>%
  group_by(replicate) %>%
  summarise(total = sum(lghat_averted*vly_disc, na.rm = TRUE)) %>%
  summarise(
    across(total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# save results
disc_extrawelfarist_sum
write.csv(disc_extrawelfarist_sum, "analysis/tables/disc_extrawelfarist_sum.csv")

# calculate undiscounted extrawelfarist roi
roi_undiscextrawelfarist <- undisc_extrawelfarist_sum %>%
  mutate(roi_low = ((total_low - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_med = ((total_med - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_high = ((total_high - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_undiscextrawelfarist
write.csv(roi_undiscextrawelfarist, "analysis/tables/roi_undiscextrawelfarist.csv")


# calculate discounted extrawelfarist roi
roi_discextrawelfarist <- disc_extrawelfarist_sum %>%
  mutate(roi_low = ((total_low - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_med = ((total_med - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_high = ((total_high - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_discextrawelfarist
write.csv(roi_discextrawelfarist, "analysis/tables/roi_discextrawelfarist.csv")









