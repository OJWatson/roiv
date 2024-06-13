library(tidyverse)
devtools::load_all()

setwd("~/documents/GitHub/roiv")

# read in our averted data that we create in the first script
res_all <- readRDS("analysis/data/derived/averted_dch.rds")

# filter by hospitalisations and sum hospitals per country

averted_hospitalisations <- res_all %>%
  filter(name == "hospitalisations") %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_hospitalisations = sum(averted))

# read in direct healthcare cost data from the data raw
# directory. This directory includes data that is sourced from
# elsewhere and should be treated as read only.

hc_costs <- read_csv("analysis/data/raw/healthcarecosts(practice).csv")

# create data frame with total hospitalisations averted per country and the cost per hospitalisation per country
averted_hc_costs <- left_join(averted_hospitalisations, hc_costs, by = "iso3c")

# read in GDP data

GDP_iso3c <- read_csv("analysis/data/raw/GDP_iso3c.csv")

# add GDP of each country to data frame

averted_hc_costs <- left_join(averted_hc_costs, GDP_iso3c, by = "iso3c")

# multiply hospitalisations by costs to get costs averted per country

averted_hc_costs <- averted_hc_costs %>%
  mutate(total_costs = averted_hospitalisations * cost)

# get healthcare costs as a % of GDP

averted_hc_costs <- averted_hc_costs %>%
  mutate(total_costs_percentageGDP = (total_costs / GDP_2021)*100)

# sum costs averted for each income group

averted_hccosts_incomegroup <- averted_hc_costs  %>%
  group_by(income_group, replicate) %>%
  summarise(averted_total_costs = sum(total_costs, na.rm = TRUE))

# get mean % of GDP for each income group
mean_percentGDP_incomegroup <- averted_hc_costs %>%
  group_by(income_group, replicate) %>%
  summarise(mean_hccosts_percentGDP = mean(total_costs_percentageGDP, na.rm = TRUE))

# join costs averted per income group, mean GDP for each income group, and mean percentage of GDP for each income group
averted_hccosts_incomegroup <- averted_hccosts_incomegroup %>%
  left_join(mean_percentGDP_incomegroup, by = "income_group")

# set the factor levels of income_group in the desired order
averted_hccosts_incomegroup$income_group <- factor(averted_hccosts_incomegroup$income_group, levels = c("H", "UM", "LM", "L"))

# remove rows with NA values in mean_hccosts_percentGDP
averted_hccosts_incomegroup <- averted_hccosts_incomegroup %>% drop_na(mean_hccosts_percentGDP)

# our results table which we can then save in the tables directory
averted_hccosts_incomegroup
write.csv(averted_hccosts_incomegroup, "analysis/tables/averted_hccosts_incomegroup.csv")

# create bar graph
gg_healthcarecosts <- ggplot(averted_hccosts_incomegroup, aes(x = income_group, y = mean_hccosts_percentGDP)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Healthcare Costs Averted by Income Group as a Proportion of GDP",
       x = "Income Group",
       y = "Total Healthcare Costs Averted as a Proportion of GDP")

# quick look at the chart

gg_healthcarecosts

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "averted_hccosts_incomegroup", gg_healthcarecosts, plot_dir = "/Users/halliebenjamin/Documents/GitHub/roiv/analysis/plots")





