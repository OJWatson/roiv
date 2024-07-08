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

# set income_group to be a factor and re-order them
lg_age_income$income_group <- factor(lg_age_income$income_group, levels = c("H", "UM", "LM", "L"))
lg_age_income <- lg_age_income %>%
  arrange(income_group, age_group)

# our results table which we can then save in the tables directory
lg_age_income
# OJ: Changed the name of file being saved as it was not the object you created aboove
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

# re-order income groups
income_order <- c("H", "UM", "LM", "L")

lg_income <- lg_income %>%
  mutate(income_group = factor(income_group, levels = income_order)) %>%  # Convert income_group to factor with specified order
  arrange(income_group)

# our results table which we can then save in the tables directory
lg_income
write.csv(lg_income, "analysis/tables/lg_income.csv")

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

# set income_group to be a factor and re-order them
lghat_age_income$income_group <- factor(lghat_age_income$income_group, levels = c("H", "UM", "LM", "L"))
lghat_age_income <- lghat_age_income %>%
  arrange(income_group, age_group)

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

# set income_group to be a factor and re-order them
lghat_income$income_group <- factor(lghat_income$income_group, levels = c("H", "UM", "LM", "L"))
lghat_income <- lghat_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
lghat_income
write.csv(lghat_income, "analysis/tables/lghat_income.csv")

# make a table of discounted and undiscounted life years in one both by age group and income group, and then just by income group

# life years by income group and age group
lg_lghat_age_income <- lg_age_income %>%
  left_join(lghat_age_income)

  # our results table which we can then save in the tables directory
  lg_lghat_age_income
  write.csv(lg_lghat_age_income, "analysis/tables/lg_lghat_age_income")

# life years by income group
lg_lghat_income <- lg_income %>%
  left_join(lghat_income, by = "income_group")

  # our results table which we can then save in the tables directory
  lg_lghat_income
  write.csv(lg_lghat_income, "analysis/tables/lg_lghat_income")

### HOSPITALISATIONS

# getting total hospitalisations per income group
sum_hosp_income <- res_full %>%
  filter(name == "hospitalisations")%>%
  # here we group at iso3c and replicate, therefore summing over the age
  group_by(income_group, replicate) %>%
  # get the sum of hospitalisations averted
  summarise(hospitalisations_total = sum(averted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  summarise(
    across(hospitalisations_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# re-ordering income groups
sum_hosp_income$income_group <- factor(sum_hosp_income$income_group, levels = c("H", "UM", "LM", "L"))
sum_hosp_income <- sum_hosp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_hosp_income
write.csv(sum_hosp_income, "analysis/tables/sum_hosp_income.csv")

## Basic plot to compare these
palette <- met.brewer("Archambault")
sum_hosp_income_plot <- sum_hosp_income %>%
  ggplot(aes(x = income_group, y = hospitalisations_total_med/1e6, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = hospitalisations_total_low/1e6, ymax = hospitalisations_total_high/1e6),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Median Hospitalisations Averted (in Millions)") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.87,0.87))

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

#re-ordering income groups

sum_inf_income$income_group <- factor(sum_inf_income$income_group, levels = c("H", "UM", "LM", "L"))
sum_inf_income <- sum_inf_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_inf_income
write.csv(sum_inf_income, "analysis/tables/sum_inf_income.csv")

## Basic plot to compare these
sum_inf_income_plot <- sum_inf_income %>%
  ggplot(aes(x = income_group, y = infections_total_med/1e6, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = infections_total_low/1e6, ymax = infections_total_high/1e6),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Median Infections Averted (in Millions)") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.87,0.87))

sum_inf_income_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "sum_inf_income_plot",sum_inf_income_plot)


# *****************
# VSLY calculations
# *****************

# loading and prepping data frame
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

# OJ: You refer to gnipc and gdp later but that does not exist
res_full <- res_full %>% mutate(gnipc = gnipc_usa, gdp = GDP_2021)

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
vsly_pdgp <- vsly %>%
  mutate(vsly_pgdp_undiscounted = (vsly_undiscounted/gdp)*100) %>%
  mutate(vsly_pgdp_discounted = (vsly_discounted/gdp)*100)

# group by income group to get undiscounted VSLYs per income group in terms of mean percentage of GDP

vsly_income_undiscounted_pgdp <- vsly_pdgp %>%
  group_by(income_group, replicate) %>%
  mutate(vsly_pgdp_undiscounted = mean(vsly_pgdp_undiscounted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  summarise(
    across(vsly_pgdp_undiscounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# drop rows with NA values in mean_hccosts_percentGDP
vsly_income_undiscounted_pgdp <- vsly_income_undiscounted_pgdp %>% drop_na()

# set the factor levels of income_group in the desired order
vsly_income_undiscounted_pgdp$income_group <- factor(vsly_income_undiscounted_pgdp$income_group, levels = c("H", "UM", "LM", "L"))
vsly_income_undiscounted_pgdp <- vsly_income_undiscounted_pgdp %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
vsly_income_undiscounted_pgdp
write.csv(vsly_income_undiscounted_pgdp, "analysis/tables/vsly_income_undiscounted_pgdp.csv")


## Basic plot to compare these
vsly_income_undiscounted_pgdp_plot <- vsly_income_undiscounted_pgdp %>%
  ggplot(aes(x = income_group, y = vsly_pgdp_undiscounted_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = vsly_pgdp_undiscounted_low, ymax = vsly_pgdp_undiscounted_high),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Undiscounted Value of Statistical Life Years Saved (% of GDP)") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.87,0.87))

# quick look at the plot
vsly_income_undiscounted_pgdp_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "vvsly_income_undiscounted_pgdp_plot",vsly_income_undiscounted_pgdp_plot)

# group by income group to get discounted VSLYs per income group in terms of mean percentage of GDP
vsly_income_discounted_pgdp <- vsly_pdgp %>%
  group_by(income_group, replicate) %>%
  mutate(vsly_pgdp_discounted = mean(vsly_pgdp_discounted, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  summarise(
    across(vsly_pgdp_discounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# drop rows with NA values in mean_hccosts_percentGDP
vsly_income_discounted_pgdp <- vsly_income_discounted_pgdp %>% drop_na()

# set the factor levels of income_group in the desired order
vsly_income_discounted_pgdp$income_group <- factor(vsly_income_discounted_pgdp$income_group, levels = c("H", "UM", "LM", "L"))
vsly_income_discounted_pgdp <- vsly_income_discounted_pgdp %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
vsly_income_discounted_pgdp
write.csv(vsly_income_discounted_pgdp, "analysis/tables/vsly_income_discounted_pgdp.csv")

## Basic plot to compare these
vsly_income_discounted_pgdp_plot <- vsly_income_discounted_pgdp %>%
  ggplot(aes(x = income_group, y = vsly_pgdp_discounted_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = vsly_pgdp_discounted_low, ymax = vsly_pgdp_discounted_high),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Discounted Value of Statistical Life Years Saved (% of GDP)") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.87,0.87))

# quick look at the plot
vsly_income_discounted_pgdp_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "vsly_income_discounted_pgdp_plot",vsly_income_discounted_pgdp_plot)

# making a stacked bar graph to show difference between undiscounted and discounted vslys

# reformatting the data frames
vsly_discounted_pgdp <- vsly_income_discounted_pgdp %>%
  rename(low = vsly_pgdp_discounted_low,
         med = vsly_pgdp_discounted_med,
         high = vsly_pgdp_discounted_high) %>%
  mutate(type = "Discounted")

vsly_undiscounted_pgdp <- vsly_income_undiscounted_pgdp %>%
  rename(low = vsly_pgdp_undiscounted_low,
         med = vsly_pgdp_undiscounted_med,
         high = vsly_pgdp_undiscounted_high) %>%
  mutate(type = "Undiscounted")

# combine the data frames and reshape it
vsly_pgdp_combined <- bind_rows(vsly_discounted_pgdp, vsly_undiscounted_pgdp)

# set the factor levels of income_group in the desired order
vsly_pgdp_combined$income_group <- factor(vsly_pgdp_combined$income_group, levels = c("H", "UM", "LM", "L"))
vsly_pgdp_combined <- vsly_pgdp_combined %>%
  arrange(income_group)

vsly_pgdp_plot <- vsly_pgdp_combined %>%
  ggplot(aes(x = income_group)) +
  geom_bar(aes(y = med, fill = type), stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  geom_errorbar(aes(group = type, ymin = low, ymax = high), width = 0.4, position = position_dodge(width = 0.6)) +
  ylab("Discounted and Undiscounted Value of Statistical Life Years Saved \n (% of GDP)") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Type") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.87, 0.87))

vsly_pgdp_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "vsly_pgdp_plot", vsly_pgdp_plot)

# getting total monetary value of vsly (not in percent of gdp)

vsly_undiscounted <- vsly %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_undiscounted = sum(vsly_undiscounted, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(vsly_undiscounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

vsly_undiscounted <- vsly_undiscounted %>%
  drop_na()

# set the factor levels of income_group in the desired order
vsly_undiscounted$income_group <- factor(vsly_undiscounted$income_group, levels = c("H", "UM", "LM", "L"))
vsly_undiscounted <- vsly_undiscounted %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
vsly_undiscounted
write.csv(vsly_undiscounted, "analysis/tables/vsly_undiscounted.csv")

# do the same for discounted vsly

vsly_discounted <- vsly %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_discounted = sum(vsly_discounted, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(vsly_discounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

vsly_discounted <- vsly_discounted %>%
  drop_na()

# set the factor levels of income_group in the desired order
vsly_discounted$income_group <- factor(vsly_discounted$income_group, levels = c("H", "UM", "LM", "L"))
vsly_discounted <- vsly_discounted %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
vsly_discounted
write.csv(vsly_discounted, "analysis/tables/vsly_discounted.csv")


# ****************
# Monetized QALYs
# ****************

# add percentages of GDP for WTP thresholds to res_full

res_full <- res_full %>%
  left_join(
  read_csv("analysis/data/raw/WTP_thresholds.csv") %>%
    # OJ: these were named differently to below so i had to change the names here
    set_names(c("income_group", "wtp_median", "wtp_lower_IQR", "wtp_upper_IQR")),
  by = "income_group")

# calculate wtp thresholds using the GDP percentages
res_full <- res_full %>%
  mutate(median_wtp_threshold = wtp_median * gdp) %>%
  mutate(lower_wtp_threshold = wtp_lower_IQR * gdp) %>%
  mutate(upper_wtp_threshold = wtp_upper_IQR * gdp)

# read in qaly loss data
# OJ: I can't run this as this file does not exist in what you pushed
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
  group_by(iso3c, replicate) %>%
  mutate(averted_inf_qalys = averted * infections_duration
         * -(qaly_loss/365.25)) %>%
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
  group_by(iso3c, replicate) %>%
  mutate(averted_inf_qalys = averted * infections_duration
         * -(qaly_loss/365.25)) %>%
  mutate(averted_inf_monqalys = averted_inf_qalys * median_wtp_threshold)%>%
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
  group_by(income_group, replicate) %>%
  mutate(averted_inf_qalys = averted * infections_duration
         * -(qaly_loss/365.25)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_inf_qalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
inf_qaly_income$income_group <- factor(inf_qaly_income$income_group, levels = c("H", "UM", "LM", "L"))
inf_qaly_income <- inf_qaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
inf_qaly_income
write.csv(inf_qaly_income, "analysis/tables/inf_qaly_income.csv")

# calculating monetized QALYs averted for infections for each income group

inf_monqaly_income <- res_full %>%
  filter(name == "infections") %>%
  group_by(income_group, replicate) %>%
  mutate(averted_inf_qalys = averted * infections_duration
         * -(qaly_loss/365.25)) %>%
  mutate(averted_inf_monqalys = averted_inf_qalys * median_wtp_threshold)%>%
  group_by(income_group) %>%
  summarise(
    across(averted_inf_monqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
inf_monqaly_income$income_group <- factor(inf_monqaly_income$income_group, levels = c("H", "UM", "LM", "L"))
inf_monqaly_income <- inf_monqaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
inf_monqaly_income
write.csv(inf_monqaly_income, "analysis/tables/inf_monqaly_income.csv")

# caclulating number of QALYs averted for hospitalisations for each iso3c

hosp_qaly_iso3c <- res_full %>%
  filter(name == "infections") %>%
  group_by(iso3c, replicate) %>%
  mutate(averted_hosp_qalys = averted * hospitalisations_duration
         * -(qaly_loss/365.25)) %>%
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
  group_by(iso3c, replicate) %>%
  mutate(averted_hosp_qalys = averted * infections_duration
         * -(qaly_loss/365.25)) %>%
  mutate(averted_hosp_monqalys = averted_hosp_qalys * median_wtp_threshold) %>%
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
write.csv(hosp_monqaly_iso3c, "analysis/tables/hosp_monqaly_iso3c")

# caclulating number of QALYs averted for hospitalisations for each income group

hosp_qaly_income <- res_full %>%
  filter(name == "infections") %>%
  group_by(income_group, replicate) %>%
  mutate(averted_hosp_qalys = averted * hospitalisations_duration
         * -(qaly_loss/365.25)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_hosp_qalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
hosp_qaly_income$income_group <- factor(hosp_qaly_income$income_group, levels = c("H", "UM", "LM", "L"))
hosp_qaly_income <- hosp_qaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
hosp_qaly_income
write.csv(hosp_qaly_income, "analysis/tables/hosp_qaly_income")

# calculating monetized QALYs averted for hospitalisations for each income group

hosp_monqaly_income <- res_full %>%
  filter(name == "infections") %>%
  group_by(income_group, replicate) %>%
  mutate(averted_hosp_qalys = averted * hospitalisations_duration
         * -(qaly_loss/365.25)) %>%
  mutate(averted_hosp_monqalys = averted_hosp_qalys * median_wtp_threshold) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_hosp_monqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
hosp_monqaly_income$income_group <- factor(hosp_monqaly_income$income_group, levels = c("H", "UM", "LM", "L"))
hosp_monqaly_income <- hosp_monqaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
hosp_monqaly_income
write.csv(hosp_monqaly_income, "analysis/tables/hosp_monqaly_income")

# calculating number of QALYs averted for deaths for each iso3c

deaths_qaly_iso3c <- res_full %>%
  filter(name == "deaths") %>%
  group_by(iso3c, replicate) %>%
  mutate(averted_deaths_qalys = ((averted * -qaly_loss) + lg_averted)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_deaths_qalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_qaly_iso3c
write.csv(deaths_qaly_iso3c, "analysis/tables/deaths_qaly_iso3c")

# calculating monetized QALYs averted for deaths for each iso3c

deaths_monqaly_iso3c <- res_full %>%
  filter(name == "deaths") %>%
  group_by(iso3c, replicate) %>%
  mutate(averted_deaths_qalys = ((averted * -qaly_loss) + lg_averted)) %>%
  mutate(averted_deaths_monqalys = (averted_deaths_qalys * median_wtp_threshold)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_deaths_monqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_monqaly_iso3c
write.csv(deaths_monqaly_iso3c, "analysis/tables/deaths_monqaly_iso3c")

# calculating number of QALYs averted for deaths for each income group
deaths_qaly_income <- res_full %>%
  filter(name == "deaths") %>%
  group_by(income_group, replicate) %>%
  mutate(averted_deaths_qalys = ((averted * -qaly_loss) + lg_averted)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_deaths_qalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
deaths_qaly_income$income_group <- factor(deaths_qaly_income$income_group, levels = c("H", "UM", "LM", "L"))
deaths_qaly_income <- deaths_qaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
deaths_qaly_income
write.csv(deaths_qaly_income, "analysis/tables/deaths_qaly_income")

# calculating monetized QALYs averted for deaths for each income group

deaths_monqaly_income <- res_full %>%
  filter(name == "deaths") %>%
  group_by(income_group, replicate) %>%
  mutate(averted_deaths_qalys = ((averted * -qaly_loss) + lg_averted)) %>%
  mutate(averted_deaths_monqalys = (averted_deaths_qalys * median_wtp_threshold)) %>%
  group_by(income_group) %>%
  summarise(
    across(averted_deaths_monqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
deaths_monqaly_income$income_group <- factor(deaths_monqaly_income$income_group, levels = c("H", "UM", "LM", "L"))
deaths_monqaly_income <- deaths_monqaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
deaths_monqaly_income
write.csv(deaths_monqaly_income, "analysis/tables/deaths_monqaly_income")

# sum of all qalys for infections, hospitalisations, deaths
sum_qaly_iso3c <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted * infections_duration
    * -(qaly_loss/365.25),
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(qaly_loss/365.25),
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted))
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(qalys_averted_sum = sum(qalys_averted, na.rm=TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(qalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_qaly_iso3c
write.csv(sum_qaly_iso3c, "analysis/tables/sum_qaly_iso3c.csv")

# sum of all monetized qalys for infections, hospitalisations, deaths
sum_monqaly_iso3c <- res_full %>%
  mutate(monqalys_averted = case_when(
    name == "infections" ~ averted * infections_duration
                           * -(qaly_loss/365.25) * median_wtp_threshold,
    name == "hospitalisations" ~ averted * hospitalisations_duration
                                * -(qaly_loss/365.25) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(monqalys_averted_sum = sum(monqalys_averted, na.rm=TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(monqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_monqaly_iso3c
write.csv(sum_monqaly_iso3c, "analysis/tables/sum_monqaly_iso3c.csv")

# sum of qalys for infections, hospitalisations, deaths per income group
sum_qaly_income <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted * infections_duration
    * -(qaly_loss/365.25),
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(qaly_loss/365.25),
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted))
  )) %>%
  group_by(income_group, replicate) %>%
  summarise(qalys_averted_sum = sum(qalys_averted, na.rm=TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(qalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


sum_qaly_income <- sum_qaly_income %>%
  drop_na()

# set the factor levels of income_group in the desired order
sum_qaly_income$income_group <- factor(sum_qaly_income$income_group, levels = c("H", "UM", "LM", "L"))
sum_qaly_income <- sum_qaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_qaly_income
write.csv(sum_qaly_income, "analysis/tables/sum_qaly_income.csv")

# get total monetized qalys per income group
sum_monqaly_income <- res_full %>%
  mutate(monqalys_averted = case_when(
    name == "infections" ~ averted * infections_duration
    * -(qaly_loss/365.25) * median_wtp_threshold,
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(qaly_loss/365.25) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  group_by(income_group, replicate) %>%
  summarise(monqalys_averted_sum = sum(monqalys_averted, na.rm=TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(monqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


sum_monqaly_income <- sum_monqaly_income %>%
  drop_na()

# set the factor levels of income_group in the desired order
sum_monqaly_income$income_group <- factor(sum_monqaly_income$income_group, levels = c("H", "UM", "LM", "L"))
sum_monqaly_income <- sum_monqaly_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_monqaly_income
write.csv(sum_monqaly_income, "analysis/tables/sum_monqaly_income.csv")

# get mean monetized qalys in proportion of gdp per income group
monqaly_pgdp_income <- res_full %>%
  mutate(monqalys_averted = case_when(
    name == "infections" ~ averted * infections_duration
    * -(qaly_loss/365.25) * median_wtp_threshold,
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(qaly_loss/365.25) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  mutate(monqalys_pgdp = monqalys_averted/gdp) %>%
  group_by(income_group, replicate) %>%
  summarise(mean_monqalys_pgdp = mean(monqalys_pgdp, na.rm=TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(mean_monqalys_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# set the factor levels of income_group in the desired order
monqaly_pgdp_income$income_group <- factor(monqaly_pgdp_income$income_group, levels = c("H", "UM", "LM", "L"))
monqaly_pgdp_income <- monqaly_pgdp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
monqaly_pgdp_income
write.csv(monqaly_pgdp_income, "analysis/tables/monqaly_pgdp_income.csv")

## Basic plot to compare monetized qalys as a proportion of GDP per income group
monqaly_pgdp_income_plot <- monqaly_pgdp_income %>%
  ggplot(aes(x = income_group, y = mean_monqalys_pgdp_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_monqalys_pgdp_low, ymax = mean_monqalys_pgdp_high),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Monetized QALYs Gained as a Proportion of GDP") +
  xlab("World Bank Income Group") +
  scale_fill_manual(values = palette, name = "Income Group") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

# quick look at the plot
monqaly_pgdp_income_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "monqaly_pgdp_income_plot", monqaly_pgdp_income_plot)

# plot of monetized QALYs as proportion of GDP averted on map
# get mean monetized QALYS as proportion of GDP per iso3c
monqaly_pgdp_iso3c <- res_full %>%
  mutate(monqalys_averted = case_when(
    name == "infections" ~ averted * infections_duration
    * -(qaly_loss/365.25) * median_wtp_threshold,
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(qaly_loss/365.25) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  mutate(monqalys_pgdp = monqalys_averted/gdp) %>%
  group_by(iso3c, replicate) %>%
  summarise(med_monqalys_pgdp = median(monqalys_pgdp, na.rm=TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(mean_monqalys_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
monqaly_pgdp_iso3c
write.csv(monqaly_pgdp_iso3c, "analysis/tables/monqaly_pgdp_iso3c.csv")

# plot the data on the map
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# load the world map data from naturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

# insect world map data
head(world)

# rename to match the world data column
monqaly_pgdp_iso3c <- monqaly_pgdp_iso3c %>%
  rename(iso_a3 = iso3c)

# merge the data
world <- world %>%
  left_join(monqaly_pgdp_iso3c, by = "iso_a3")

# plot the map
monqaly_pgdp_iso3c_plot <- ggplot(data = world) +
  geom_sf(aes(fill = mean_monqalys_pgdp_med)) +  # Ensure this column exists in your data
  scale_fill_gradientn(colors = palette, na.value = "grey90", name = "Monetized QALYs (Proportion of GDP)") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
  ) +
  labs(
    title = "Monetized QALYs as a Proportion of GDP by Country",
    fill = "Monetized QALYs (Proportion of GDP)"
  )

# quick look at the plot
monqaly_pgdp_iso3c_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "monqaly_pgdp_iso3c_plot", monqaly_pgdp_iso3c_plot)


# ********************
# Human Capital Costs
# ********************

# add in gdp per capita (gdppc) data
res_full <- res_full %>%
  left_join(
    read_csv("analysis/data/raw/gdppc_2021.csv"),
    by = "iso3c")

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

human_capital_inf <- human_capital_inf %>%
  drop_na()

# set the factor levels of income_group in the desired order
human_capital_inf$income_group <- factor(human_capital_inf$income_group, levels = c("H", "UM", "LM", "L"))
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


human_capital_hosp <- human_capital_hosp %>%
  drop_na()

# set the factor levels of income_group in the desired order
human_capital_hosp$income_group <- factor(human_capital_hosp$income_group, levels = c("H", "UM", "LM", "L"))
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

human_capital_sum_income <- human_capital_sum_income %>%
  drop_na()

# set the factor levels of income_group in the desired order
human_capital_sum_income$income_group <- factor(human_capital_sum_income$income_group, levels = c("H", "UM", "LM", "L"))
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
  mutate(humcap_pgdp = (hc_costs/gdp)*100) %>%
  group_by(income_group, replicate) %>%
  summarise(mean_humcap_pgdp = mean(humcap_pgdp, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(across(mean_humcap_pgdp,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# set the factor levels of income_group in the desired order
human_capital_pgdp$income_group <- factor(human_capital_pgdp$income_group, levels = c("H", "UM", "LM", "L"))
human_capital_pgdp <- human_capital_pgdp %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
human_capital_pgdp
write.csv(human_capital_pgdp, "analysis/tables/human_capital_pgdp.csv")

## Basic plot to compare human capital costs between income group as percentage of gdp
human_capital_pgdp_plot <- human_capital_pgdp %>%
  ggplot(aes(x = income_group, y = mean_humcap_pgdp_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_humcap_pgdp_low, ymax = mean_humcap_pgdp_high),
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

friction_costs$friction_period[friction_costs$income_group == "L"] <- lmic_income
friction_costs$friction_period[friction_costs$income_group == "LM"] <- lmic_income

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

friction_costs <- friction_costs %>%
  drop_na()

# sum friction costs per income group

friction_costs_sum <- friction_costs %>%
  group_by(income_group, replicate) %>%
  summarise(friction_costs_total = sum(friction_costs)) %>%
  group_by(income_group) %>%
  summarise(across(friction_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# set the factor levels of income_group in the desired order
friction_costs_sum$income_group <- factor(friction_costs_sum$income_group, levels = c("H", "UM", "LM", "L"))
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
  mutate(friction_costs_pgdp = (friction_costs/gdp)*100) %>%
  group_by(income_group, replicate) %>%
  summarise(mean_friction_costs_pgdp = mean(friction_costs_pgdp)) %>%
  group_by(income_group) %>%
  summarise(across(mean_friction_costs_pgdp,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# set the factor levels of income_group in the desired order
friction_costs_pgdp$income_group <- factor(friction_costs_pgdp$income_group, levels = c("H", "UM", "LM", "L"))
friction_costs_pgdp <- friction_costs_pgdp %>%
  arrange(income_group)


# our results table which we can then save in the tables directory
friction_costs_pgdp
write.csv(friction_costs_pgdp, "analysis/tables/friction_costs_pgdp.csv")


## Basic plot to compare human capital costs between income group as percentage of gdp
friction_costs_pgdp_plot <- friction_costs_pgdp %>%
  ggplot(aes(x = income_group, y = mean_friction_costs_pgdp_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_friction_costs_pgdp_low, ymax = mean_friction_costs_pgdp_high),
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

# ********************
# Healthcare Costs
# ********************

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

# get healthcare costs as a % of GDP

hc_costs_grouped <- hc_costs_grouped %>%
  mutate(hccosts_pgdp = (cost_total / gdp)*100)

# sum costs averted for each income group

hc_costs_total <- hc_costs_grouped  %>%
  group_by(income_group, replicate) %>%
  summarise(hc_costs_total = sum(cost_total, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(across(hc_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# set the factor levels of income_group in the desired order
hc_costs_total$income_group <- factor(hc_costs_total$income_group, levels = c("H", "UM", "LM", "L"))
hc_costs_total <- hc_costs_total %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
hc_costs_total
write.csv(hc_costs_total, "analysis/tables/hc_costs_total .csv")

# get mean % of GDP for each income group
hccosts_pgdp <- hc_costs_grouped %>%
  group_by(income_group, replicate) %>%
  summarise(mean_hccosts_pgdp = mean(hccosts_pgdp, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(across(mean_hccosts_pgdp,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# set the factor levels of income_group in the desired order
hccosts_pgdp$income_group <- factor(hccosts_pgdp$income_group, levels = c("H", "UM", "LM", "L"))
hccosts_pgdp <- hccosts_pgdp %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
hccosts_pgdp
write.csv(hccosts_pgdp, "analysis/tables/hccosts_pgdp.csv")


# create bar graph to express healthcare costs averted per income group as a percentage of gdp
healthcarecosts_pgdp_plot <- hccosts_pgdp %>%
  ggplot(aes(x = income_group, y = mean_hccosts_pgdp_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_hccosts_pgdp_low, ymax = mean_hccosts_pgdp_high),
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


# *************************
# Total Costs into Vaccines
# *************************

dev_funding <- 10011000000

# ********************
# Return on Investment
# ********************

# welfarist - monetized QALYs, covid-19 healthcare costs, human capital costs, productivity losses

# sum of monetized qalys

sum_monqaly <- res_full %>%
  mutate(monqalys_averted = case_when(
    name == "infections" ~ averted * infections_duration
    * -(qaly_loss/365.25) * median_wtp_threshold,
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(qaly_loss/365.25) * median_wtp_threshold,
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
sum_monqaly
write.csv(sum_monqaly, "analysis/tables/sum_monqaly.csv")

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
  summarise(friction_costs_total = sum(friction_costs)) %>%
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


# combine the summaries

welfarist_sum <- bind_cols(sum_monqaly, sum_humancapital, sum_frictioncosts, sum_hc_costs) %>%
  summarise(
    total_low = monqalys_averted_sum_low + humcap_total_low + friction_costs_total_low + hc_costs_total_low,
    total_med = monqalys_averted_sum_med + humcap_total_med + friction_costs_total_med + hc_costs_total_med,
    total_high = monqalys_averted_sum_high + humcap_total_high + friction_costs_total_high + hc_costs_total_high
  )

# save results
welfarist_sum
write.csv(welfarist_sum, "analysis/tables/welfarist_sum.csv")

# do ROI with median

roi_welfarist <- (welfarist_sum$total_med - dev_funding)/dev_funding

# extra welfarist - VSLYs

# sum of vslys (undiscounted)

sum_vsly_undiscounted <- vsly %>%
  group_by(replicate) %>%
  summarise(vsly_undiscounted = sum(vsly_undiscounted, na.rm = TRUE)) %>%
  summarise(
    across(vsly_undiscounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

sum_vsly_undiscounted <- sum_vsly_undiscounted %>%
  drop_na()


# our results table which we can then save in the tables directory
sum_vsly_undiscounted
write.csv(sum_vsly_undiscounted, "analysis/tables/sum_vsly_undiscounted.csv")

# roi calculation

undisc_exwelf_roi <- (sum_vsly_undiscounted$vsly_undiscounted_med - dev_funding)/dev_funding

# do the same for discounted vsly

sum_vsly_discounted <- vsly %>%
  group_by(replicate) %>%
  summarise(vsly_discounted = sum(vsly_discounted, na.rm = TRUE)) %>%
  summarise(
    across(vsly_discounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

sum_vsly_discounted <- sum_vsly_discounted %>%
  drop_na()

# our results table which we can then save in the tables directory
sum_vsly_discounted
write.csv(sum_vsly_discounted, "analysis/tables/sum_vsly_discounted.csv")

# roi calculation
disc_exwelf_roi <- (sum_vsly_discounted$vsly_discounted_med - dev_funding)/dev_funding















