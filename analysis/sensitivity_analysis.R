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
vsly_pdgp <- vsly %>%
  mutate(vsly_percentgdp_undiscounted = (vsly_undiscounted/gdp)*100) %>%
  mutate(vsly_percentgdp_discounted = (vsly_discounted/gdp)*100)

# group by income group to get undiscounted VSLYs per income group in terms of mean percentage of GDP

vsly_income_undiscounted_pgdp <- vsly_pdgp %>%
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
vsly_income_undiscounted_pgdp_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "vvsly_income_undiscounted_pgdp_plot",vsly_income_undiscounted_pgdp_plot)

# group by income group to get discounted VSLYs per income group in terms of mean percentage of GDP
vsly_income_discounted_pgdp <- vsly_pdgp %>%
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
vsly_income_discounted_pgdp <- vsly_income_discounted_pgdp %>% drop_na()

# set the factor levels of income_group in the desired order
vsly_income_discounted_pgdp$income_group <- factor(vsly_income_discounted_pgdp$income_group, levels = c("H", "UM", "LM", "L"))

# our results table which we can then save in the tables directory
vsly_income_discounted_pgdp
write.csv(vsly_income_discounted_pgdp, "analysis/tables/vsly_income_discounted_pgdp.csv")

## Basic plot to compare these
vsly_income_discounted_pgdp_plot <- vsly_income_discounted_pgdp %>%
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
vsly_income_discounted_pgdp_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "vsly_income_discounted_pgdp_plot",vsly_income_discounted_pgdp_plot)

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
    read_csv("analysis/data/raw/WTP_thresholds.csv"),
    by = "income_group")

# calculate wtp thresholds using the GDP percentages
res_full <- res_full %>%
  mutate(median_wtp_threshold = wtp_median * gdp) %>%
  mutate(lower_wtp_threshold = wtp_lower_IQR * gdp) %>%
  mutate(upper_wtp_threshold = wtp_upper_IQR * gdp)

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
write.csv(inf_qaly_iso3c, "analysis/tables/inf_monqaly_iso3c.csv")

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

hosp_qaly_iso3c <- res_full %>%
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
write.csv(hosp_monqaly_iso3c, "analysis/tables/hosp_qaly_iso3c")

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
write.csv(hosp_qaly_income, "analysis/tables/hosp_qaly_income")

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
  ylab("Monetized QALYs Gained as a Proportion of GDP)") +
  xlab("World Bank Income Group") +
  scale_fill_viridis_d(name = "Income") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

# quick look at the plot
monqaly_pgdp_income_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "sum_monqaly_income_plot", sum_monqaly_income_plot)


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
human_capital_sum <- human_capital %>%
  group_by(income_group, replicate) %>%
  summarise(hc_costs_total = sum(hc_costs, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(across(hc_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

human_capital_sum <- human_capital_sum %>%
  drop_na()

# set the factor levels of income_group in the desired order
human_capital_sum$income_group <- factor(human_capital_sum$income_group, levels = c("H", "UM", "LM", "L"))
human_capital_sum <- human_capital_sum %>%
  arrange(income_group)


# our results table which we can then save in the tables directory
human_capital_sum
write.csv(human_capital_sum, "analysis/tables/human_capital_sum.csv")

## Basic plot to compare human capital costs between income group
human_capital_sum_plot <- human_capital_sum %>%
  ggplot(aes(x = income_group, y = hc_costs_total_med/1e9, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = hc_costs_total_low/1e9, ymax = hc_costs_total_high/1e9),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Human Capital Costs Averted (in Billions ($))") +
  xlab("World Bank Income Group") +
  scale_fill_viridis_d(name = "Income") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

# quick look at the plot
human_capital_sum_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "human_capital_sum_plot", human_capital_sum_plot)

# generating table of human capital costs averted as a percentage of gdp per income group
human_capital_pgdp <- human_capital %>%
  mutate(hc_costs_pgdp = (hc_costs/gdp)*100) %>%
  group_by(income_group, replicate) %>%
  summarise(mean_hc_costs_pgdp = mean(hc_costs_pgdp, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(across(mean_hc_costs_pgdp,
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
  ggplot(aes(x = income_group, y = mean_hc_costs_pgdp_med, fill = income_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_hc_costs_pgdp_low, ymax = mean_hc_costs_pgdp_high),
                width = 0.2, position = position_dodge(0.9)) +
  ylab("Human Capital Costs Averted as a Percentage of GDP") +
  xlab("World Bank Income Group") +
  scale_fill_viridis_d(name = "Income") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

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
  scale_fill_viridis_d(name = "Income") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

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
  scale_fill_viridis_d(name = "Income") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

# quick look at the plot
friction_costs_pgdp_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "friction_costs_pgdp_plot", friction_costs_pgdp_plot)

# ********************
# Healthcare Costs -- REVIEW
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

hc_costs_hic_noneurope <- hc_costs %>%
  filter(income_group == "H", region !="europe_central_asia") %>%
  left_join(usa_hc_costs, by = "iso3c")

# assign the USA values for the cost data

hc_costs_hic_noneurope$cost_2022[is.na(hc_costs_hic_noneurope$cost_2022)] <- 11275
hc_costs_hic_noneurope$upper_cost_2022[is.na(hc_costs_hic_noneurope$upper_cost_2022)] <- 11297
hc_costs_hic_noneurope$lower_cost_2022[is.na(hc_costs_hic_noneurope$lower_cost_2022)] <- 11252

# convert the costs to USD 2021

hc_costs_hic_noneurope <- hc_costs_hic_noneurope %>%
  mutate(cost_2021 = cost_2022 * (usa_gdp_deflator2021/usa_gdp_deflator2022)) %>%
  mutate(upper_cost_2021 = upper_cost_2022 * (usa_gdp_deflator2021/usa_gdp_deflator2022)) %>%
  mutate(lower_cost_2021 = upper_cost_2022 * (usa_gdp_deflator2021/usa_gdp_deflator2022))

# calculate the total cost

hc_costs_hic_noneurope <- hc_costs_hic_noneurope %>%
  mutate(cost_total = cost_2021 * hospitalisations_duration * averted) %>%
  mutate(upper_cost_total = upper_cost_2021 * hospitalisations_duration * averted) %>%
  mutate(lower_cost_total = lower_cost_2021 * hospitalisations_duration * averted)

# do the same for high income, european countries

# read in Spain healthcare cost data for european high-income countries with missing data

spain_hc_costs <- read_csv("analysis/data/raw/spain_hc_costs.csv")

hc_costs_hic_europe <- hc_costs %>%
  filter(income_group == "H", region =="europe_central_asia") %>%
  left_join(spain_hc_costs, by = "iso3c")

# assign the spain values for the cost data

hc_costs_hic_europe$mean_cost_usd2020[is.na(hc_costs_hic_europe$mean_cost_usd2020)] <- 6480.66
hc_costs_hic_europe$upper_cost_usd2020[is.na(hc_costs_hic_europe$upper_cost_usd2020)] <- 6521.63
hc_costs_hic_europe$lower_cost_usd2020[is.na(hc_costs_hic_europe$lower_cost_usd2020)] <- 6439.70

# convert the costs to USD 2021

hc_costs_hic_europe <- hc_costs_hic_europe %>%
  mutate(cost_2021 = mean_cost_usd2020 * (usa_gdp_deflator2021/usa_gdp_deflator2020)) %>%
  mutate(upper_cost_2021 = upper_cost_usd2020 * (usa_gdp_deflator2021/usa_gdp_deflator2020)) %>%
  mutate(lower_cost_2021 = upper_cost_usd2020 * (usa_gdp_deflator2021/usa_gdp_deflator2020))

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
  ylab("Healthcare Costs Averted as a Percentage of GDP") +
  xlab("World Bank Income Group") +
  scale_fill_viridis_d(name = "Income") +
  theme_bw(base_family = "Helvetica") +
  theme(legend.position = c(0.9,0.85))

# quick look at the chart
healthcarecosts_pgdp_plot

# save the figure to the plots directory using the function
# save_figs, which is a function in this roiv package (see utils-plot.R)
save_figs(name = "healthcarecosts_pgdp_plot", healthcarecosts_pgdp_plot, plot_dir = "/Users/halliebenjamin/Documents/GitHub/roiv/analysis/plots")
