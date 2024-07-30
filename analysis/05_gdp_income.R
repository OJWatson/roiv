# make data frame with iso3c, income group and gdp
gdp_iso3c_list <- read_csv("analysis/data/raw/iso3c_list.csv") %>%
  left_join(
    read_csv("analysis/data/raw/GDP_iso3c.csv"),
    by = "iso3c")

gdp_iso3c_list$income_group <- squire.page::get_income_group(gdp_iso3c_list$iso3c)

# determine gdp per income group

gdp_income <- gdp_iso3c_list %>%
  group_by(income_group) %>%
  summarise(gdp_total = sum(gdp, na.rm = TRUE))

gdp_income$income_group <- factor(gdp_income$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
gdp_income <- gdp_income %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
gdp_income
write.csv(gdp_income, "analysis/tables/gdp_income.csv")
