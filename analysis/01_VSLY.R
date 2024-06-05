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

# read in GNI per capita data from World Bank

gnipc_2021 <- read_csv("analysis/data/raw/GNIPC_2021.csv", col_types = cols(
  `Country Name` = col_character(),
  iso3c = col_character(),
  GNIPC_2021 = col_double()
))

# read in income group classification from World Bank

income_groups <- read_csv("analysis/data/raw/worldbank_classifications.csv")

# add GNIPC value for each country and income level

res_full <- res_full %>%
  left_join(gnipc_2021, by = "iso3c") %>%
  left_join(income_groups, by = "iso3c")


# read in USA VSL value

vsl_usa <- read_csv("analysis/data/raw/VSL_USA_2021.csv")
  # remove rows with all NA values
  vsl_usa <- vsl_usa %>%
  filter_all(any_vars(!is.na(.)))
  # remove columns with all NA values
  vsl_usa <- vsl_usa %>%
  select(which(colSums(is.na(.)) != nrow(vsl_usa)))

# extract USA GNIPC value
gnipc_usa <- gnipc_2021 %>%
  filter(iso3c == "USA") %>%
  pull(GNIPC_2021)

#extract USA VSL value (pulling value from data frame from column "mean" and row 1)

mean_vsl_usa <- vsl_usa$"mean"[1]

# add column for VSL

res_full <- res_full %>%
  group_by(iso3c, replicate) %>%
  mutate(vsl = mean_vsl_usa*(GNIPC_2021/gnipc_usa)^1)

# and now we can work out our total value of lives lost
# helper functions for uncertainties
lf <- function(x){quantile(x, 0.025, na.rm=TRUE)}
mf <- function(x){quantile(x, 0.5, na.rm=TRUE)}
hf <- function(x){quantile(x, 0.975, na.rm=TRUE)}

# Because vsly is just something to be calculated based on deaths we start by
# just filtering to this:
vsly_income_group <- res_full %>%
  filter(name == "deaths") %>%
  # calculate the discounted number of years lost given the averted deaths
  mutate(Y = averted * lghat) %>%
  # now the financial value based on the VLY
  mutate(vsly = vsl * Y) %>%
  # here we group at income and replicate, therefore summing over the age
  group_by(income_group, replicate) %>%
  summarise(vsly = sum(vsly, na.rm = TRUE),
            Y = sum(Y, na.rm = TRUE)) %>%
  # now we just group by income, so providing intervals over our replicates
  group_by(income_group) %>%
  summarise(
    across(vsly,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# drop rows with NA values in mean_hccosts_percentGDP
vsly_income_group <- vsly_income_group %>% drop_na()

# set the factor levels of income_group in the desired order
vsly_income_group$income_group <- factor(vsly_income_group$income_group, levels = c("H", "UM", "LM", "L"))


# our results table which we can then save in the tables directory
vsly_income_group
write.csv(vsly_income_group, "analysis/tables/value_of_statistical_life_years_saved_by_income.csv")

## Basic plot to compare these
vsly_income_group_plot <- vsly_income_group %>%
  ggplot(aes(x = income_group, y = vsly_med/1e9, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +
  ylab("Value of Statistical Life Years Saved (Billion $)") +
  xlab("World Bank Income Group") +
  scale_fill_viridis_d(name = "Income") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

# quick look at the plot
vsly_income_group_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "value_of_statistical_life_years_saved_by_income", gg1)








