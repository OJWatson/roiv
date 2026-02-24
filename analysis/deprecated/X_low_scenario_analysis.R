## LOWER ESTIMATES

# *****************
# VSLY calculations
# *****************

# extract low USA VSL value (pulling value from data frame from column "low" and row 1)
low_vsl_usa <- vsl_usa$"low"[1]

vsly_low <- vsly %>%
  group_by(iso3c, replicate) %>%
  mutate(vsl_low = low_vsl_usa*(gnipc/gnipc_usa)^1) %>%
  mutate(w_nglg = sum(Ng*lg) / sum(Ng)) %>%
  mutate(w_nglghat = sum(Ng * lghat) / sum(Ng)) %>%
  mutate(vly_low = vsl_low / w_nglg) %>%
  mutate(vly_disc_low = vsl_low / w_nglghat)

# getting total monetary value of vsly (not in percent of gdp)
vsly_undisc_low <- vsly_low %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_undisc_low = sum(lg_averted * vly_low, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(vsly_undisc_low,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
vsly_undisc_low
write.csv(vsly_undisc_low, "analysis/tables/vsly_undisc_low.csv")

# getting total monetary value of vsly (not in percent of gdp)
vsly_disc_low <- vsly_low %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_disc_low = sum(lghat_averted * vly_disc_low, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(vsly_disc_low,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
vsly_disc_low
write.csv(vsly_disc_low, "analysis/tables/vsly_disc_low.csv")


# ****************
# Monetized QALYs
# ****************

# convert IQR from WTP threshold percentages into 95% CI

qaly_sa <- res_full %>%
  mutate(lower_wtpCI = case_when(
    income_group == "HIC" ~ 0.606882906,
    income_group == "UMIC" ~ 0.51478086,
    income_group == "LMIC" ~ 0.299997037,
    income_group == "LIC" ~ 0.199435387
  )) %>%
  mutate(upper_wtpCI = case_when(
    income_group == "HIC" ~ 0.757215692,
    income_group == "UMIC" ~ 0.649052609,
    income_group == "LMIC" ~ 0.403800542,
    income_group == "LIC" ~ 0.284264021
  ))


# calculate wtp thresholds using the GDP percentages
qaly_sa_low <- qaly_sa %>%
  mutate(lower_wtp_threshold = lower_wtpCI * gdppc)

# calculating the sum of QALYs averted for infections, hospitalisations and deaths
#     for each iso3c using the UPPER qaly loss value
# using upper qaly loss value because it is smaller in magnitude than the lower
#     qaly loss estimate and therefore will produce a lower qaly gain

# sum of all undiscounted qalys for infections, hospitalisations, deaths
sum_undiscqaly_iso3c_low <- qaly %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(upper_qaly_loss),
    name == "hospitalisations" ~ averted
    * -(upper_qaly_loss),
    name == "deaths" ~ (((averted * -upper_qaly_loss) + lg_averted))
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
sum_undiscqaly_iso3c_low
write.csv(sum_undiscqaly_iso3c_low, "analysis/tables/sum_undiscqaly_iso3c_low.csv")


# sum of qalys for infections, hospitalisations, deaths per income group
sum_undiscqaly_income_low <- qaly %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(upper_qaly_loss),
    name == "hospitalisations" ~ averted
    * -(upper_qaly_loss),
    name == "deaths" ~ (((averted * -upper_qaly_loss) + lg_averted))
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
sum_undiscqaly_income_low$income_group <- factor(sum_undiscqaly_income_low$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_undiscqaly_income_low <- sum_undiscqaly_income_low %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_undiscqaly_income_low
write.csv(sum_undiscqaly_income_low, "analysis/tables/sum_undiscqaly_income_low.csv")

# get total monetized qalys per iso3c
sum_undiscmonqaly_iso3c_low <- qaly %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "deaths" ~ (((averted * -upper_qaly_loss) + lg_averted)
                        * lower_wtp_threshold)
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
sum_undiscmonqaly_iso3c_low
write.csv(sum_undiscmonqaly_iso3c_low, "analysis/tables/sum_undiscmonqaly_iso3c_low.csv")

# get total monetized qalys per income group
sum_undiscmonqaly_income_low <- qaly %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "deaths" ~ (((averted * -upper_qaly_loss) + lg_averted)
                        * lower_wtp_threshold)
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


sum_undiscmonqaly_income_low <- sum_undiscmonqaly_income_low %>%
  drop_na()

# set the factor levels of income_group in the desired order
sum_undiscmonqaly_income_low$income_group <- factor(sum_undiscmonqaly_income_low$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_undiscmonqaly_income_low <- sum_undiscmonqaly_income_low %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_undiscmonqaly_income_low
write.csv(sum_undiscmonqaly_income_low, "analysis/tables/sum_undiscmonqaly_income_low.csv")

# do the same for discounted qalys and monetized qalys
# sum of all discounted qalys for infections, hospitalisations, deaths
sum_discqaly_iso3c_low <- qaly %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(upper_qaly_loss),
    name == "hospitalisations" ~ averted
    * -(upper_qaly_loss),
    name == "deaths" ~ (((averted * -upper_qaly_loss) + lghat_averted))
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
sum_discqaly_iso3c_low
write.csv(sum_discqaly_iso3c_low, "analysis/tables/sum_discqaly_iso3c_low.csv")


# sum of all monetized discounted qalys for infections, hospitalisations, deaths
sum_discmonqaly_iso3c_low <- qaly %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "deaths" ~ (((averted * -upper_qaly_loss) + lghat_averted)
                        * lower_wtp_threshold)
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
sum_discmonqaly_iso3c_low
write.csv(sum_discmonqaly_iso3c_low, "analysis/tables/sum_discmonqaly_iso3c_low.csv")

# sum of qalys for infections, hospitalisations, deaths per income group
sum_discqaly_income_low <- qaly %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(upper_qaly_loss),
    name == "hospitalisations" ~ averted
    * -(upper_qaly_loss),
    name == "deaths" ~ (((averted * -upper_qaly_loss) + lghat_averted))
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


sum_discqaly_income_low <- sum_discqaly_income_low %>%
  drop_na()

# set the factor levels of income_group in the desired order
sum_discqaly_income_low$income_group <- factor(sum_discqaly_income_low$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_discqaly_income_low <- sum_discqaly_income_low %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_discqaly_income_low
write.csv(sum_discqaly_income_low, "analysis/tables/sum_discqaly_income_low.csv")


# get total monetized qalys per income group
sum_discmonqaly_income_low <- qaly %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "deaths" ~ (((averted * -upper_qaly_loss) + lghat_averted)
                        * lower_wtp_threshold)
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


sum_discmonqaly_income_low <- sum_discmonqaly_income_low %>%
  drop_na()

# set the factor levels of income_group in the desired order
sum_discmonqaly_income_low$income_group <- factor(sum_discmonqaly_income_low$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_discmonqaly_income_low <- sum_discmonqaly_income_low %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_discmonqaly_income_low
write.csv(sum_discmonqaly_income_low, "analysis/tables/sum_discmonqaly_income_low.csv")


# ***************
# Friction Costs
# ***************

# calculate the lower confidence intervals for the friction periods
friction_costs_low <- friction_costs %>%
  mutate(friction_period_low = case_when(
    income_group == "HIC" & name == "deaths" ~ (friction_period - friction_sd*1.96),
    income_group %in% c("UMIC", "LMIC", "LIC") ~ (3*30.417)))

# calculate friction costs averted
friction_costs_low <- friction_costs_low %>%
  mutate(friction_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted,
    name == "deaths" ~ (gdppc/365.25) * friction_period_low * averted
  ))


# sum friction costs per income group

friction_costs_sum_low <- friction_costs_low %>%
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
friction_costs_sum_low$income_group <- factor(friction_costs_sum_low$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
friction_costs_sum_low <- friction_costs_sum_low %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
friction_costs_sum_low
write.csv(friction_costs_sum_low, "analysis/tables/friction_costs_sum_low.csv")


# calculating friction costs for each iso3c
friction_costs_sum_iso3c_low <- friction_costs_low %>%
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
friction_costs_sum_iso3c_low
write.csv(friction_costs_sum_iso3c_low, "analysis/tables/friction_costs_sum_iso3c_low.csv")


# ********************
# Healthcare Costs
# ********************

# naming it lowercost_total for the sensitivity analysis to match the column for hic
hc_costs_lmic_low <- hc_costs_lmic %>%
  mutate(lowercost_total = cost_pd2021 * hospitalisations_duration * averted)

# bind with lmic costs
hc_costs_grouped_low <- bind_rows(hic_hccosts_2021, hc_costs_lmic_low)

# sum costs averted for each income group

hc_costs_total_low <- hc_costs_grouped_low  %>%
  group_by(income_group, replicate) %>%
  summarise(hc_costs_total_low = sum(lowercost_total)) %>%
  group_by(income_group) %>%
  summarise(across(hc_costs_total_low,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

#set the factor levels of income_group in the desired order
hc_costs_total_low$income_group <- factor(hc_costs_total_low$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
hc_costs_total_low <- hc_costs_total_low %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
hc_costs_total_low
write.csv(hc_costs_total_low, "analysis/tables/hc_costs_total_low.csv")


# sum costs averted for each iso3c
hc_costs_total_iso3c_low <- hc_costs_grouped_low  %>%
  group_by(iso3c, replicate) %>%
  summarise(health_costs_total_low = sum(lowercost_total)) %>%
  group_by(iso3c) %>%
  summarise(across(health_costs_total_low,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))


# our results table which we can then save in the tables directory
hc_costs_total_iso3c_low
write.csv(hc_costs_total_iso3c_low, "analysis/tables/hc_costs_total_iso3c_low.csv")

### SUM OF EVERYTHING

# get total undiscounted monetized qalys
sum_undiscmonqaly_low <- qaly %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "deaths" ~ (((averted * -upper_qaly_loss) + lg_averted)
                        * lower_wtp_threshold)
  )) %>%
  group_by(replicate) %>%
  summarise(undiscmonqalys_averted_sum = sum(undiscmonqalys_averted, na.rm=TRUE)) %>%
  summarise(
    across(undiscmonqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_undiscmonqaly_low
write.csv(sum_undiscmonqaly_low, "analysis/tables/sum_undiscmonqaly_low.csv")

# get total monetized qalys
sum_discmonqaly_low <- qaly %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(upper_qaly_loss) * lower_wtp_threshold,
    name == "deaths" ~ (((averted * -upper_qaly_loss) + lghat_averted)
                        * lower_wtp_threshold)
  )) %>%
  group_by(replicate) %>%
  summarise(discmonqalys_averted_sum = sum(discmonqalys_averted, na.rm=TRUE)) %>%
  summarise(
    across(discmonqalys_averted_sum,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_discmonqaly_low
write.csv(sum_discmonqaly_low, "analysis/tables/sum_discmonqaly_low.csv")


# calculating friction costs for each iso3c
sum_frictioncost_low <- friction_costs_low %>%
  group_by(replicate) %>%
  summarise(friction_costs_total = sum(friction_costs, na.rm = TRUE)) %>%
  summarise(across(friction_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
sum_frictioncost_low
write.csv(sum_frictioncost_low, "analysis/tables/sum_frictioncost_low.csv")

# sum costs averted for each iso3c
sum_hc_costs_low <- hc_costs_grouped_low  %>%
  group_by(replicate) %>%
  summarise(health_costs_total = sum(lowercost_total)) %>%
  summarise(across(health_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))


# our results table which we can then save in the tables directory
sum_hc_costs_low
write.csv(sum_hc_costs_low, "analysis/tables/sum_hc_costs_low.csv")


# combine the summaries - undiscounted
extrawelfarist_undiscsum_low <- bind_cols(sum_undiscmonqaly_low, sum_frictioncost_low, sum_hc_costs_low) %>%
  summarise(
    total_low =  undiscmonqalys_averted_sum_low +  friction_costs_total_low + health_costs_total_low,
    total_med =  undiscmonqalys_averted_sum_med  + friction_costs_total_med + health_costs_total_med,
    total_high =  undiscmonqalys_averted_sum_high  + friction_costs_total_high + health_costs_total_high
  )

# save results
extrawelfarist_undiscsum_low
write.csv(extrawelfarist_undiscsum_low, "analysis/tables/extrawelfarist_undiscsum_low.csv")

# combine the summaries - discounted
extrawelfarist_discsum_low <- bind_cols(sum_discmonqaly_low, sum_frictioncost_low, sum_hc_costs_low) %>%
  summarise(
    total_low =  discmonqalys_averted_sum_low +  friction_costs_total_low + health_costs_total_low,
    total_med =  discmonqalys_averted_sum_med + friction_costs_total_med + health_costs_total_med,
    total_high =  discmonqalys_averted_sum_high + friction_costs_total_high + health_costs_total_high
  )

# save results
extrawelfarist_discsum_low
write.csv(extrawelfarist_discsum_low, "analysis/tables/extrawelfarist_discsum_low.csv")

# welfarist sums

# sum of undiscounted vslys
low_undisc_welfarist_sum <- vsly_low %>%
  group_by(replicate) %>%
  summarise(total = sum(lg_averted * vly_low, na.rm = TRUE)) %>%
  summarise(
    across(total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

 # our results table which we can then save in the tables directory
low_undisc_welfarist_sum
write.csv(low_undisc_welfarist_sum, "analysis/tables/low_undisc_welfarist_sum.csv")

# do the same for discounted vsly
low_disc_welfarist_sum <- vsly_low %>%
  group_by(replicate) %>%
  summarise(total = sum(lghat_averted * vly_disc_low, na.rm = TRUE)) %>%
  summarise(
    across(total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
low_disc_welfarist_sum
write.csv(low_disc_welfarist_sum, "analysis/tables/low_disc_welfarist_sum.csv")

# corporate low

corporate_low <- 8000000000 * (1/0.845)

# calculate roi
# calculate undiscounted extrawelfarist roi

low_roi_undiscextrawelfarist <- extrawelfarist_undiscsum_low %>%
  mutate(roi_low = ((total_low - (vaccine_costs))/(vaccine_costs)),
         roi_med = ((total_med - (vaccine_costs))/(vaccine_costs)),
         roi_high = ((total_high - (vaccine_costs))/(vaccine_costs))) %>%
  select(roi_low, roi_med, roi_high)

# save results
low_roi_undiscextrawelfarist
write.csv(low_roi_undiscextrawelfarist, "analysis/tables/low_roi_undiscextrawelfarist.csv")

# calculate discounted extrawelfarist roi

low_roi_discextrawelfarist <- extrawelfarist_discsum_low %>%
  mutate(roi_low = ((total_low - (vaccine_costs))/(vaccine_costs)),
         roi_med = ((total_med - (vaccine_costs))/(vaccine_costs)),
         roi_high = ((total_high - (vaccine_costs))/(vaccine_costs))) %>%
  select(roi_low, roi_med, roi_high)

# save results
low_roi_discextrawelfarist
write.csv(low_roi_discextrawelfarist, "analysis/tables/low_roi_discextrawelfarist.csv")

# calculate undiscounted welfarist roi
low_roi_undiscwelfarist <- low_undisc_welfarist_sum %>%
  mutate(roi_low = ((total_low - (vaccine_costs))/(vaccine_costs)),
         roi_med = ((total_med - (vaccine_costs))/(vaccine_costs)),
         roi_high = ((total_high - (vaccine_costs))/(vaccine_costs))) %>%
  select(roi_low, roi_med, roi_high)

# save results
low_roi_undiscwelfarist
write.csv(low_roi_undiscwelfarist, "analysis/tables/low_roi_undiscwelfarist.csv")


# calculate discounted extrawelfarist roi
low_roi_discwelfarist <- low_disc_welfarist_sum  %>%
  mutate(roi_low = ((total_low - (vaccine_costs))/(vaccine_costs)),
         roi_med = ((total_med - (vaccine_costs))/(vaccine_costs)),
         roi_high = ((total_high - (vaccine_costs))/(vaccine_costs))) %>%
  select(roi_low, roi_med, roi_high)

# save results
low_roi_discwelfarist
write.csv(low_roi_discwelfarist, "analysis/tables/low_roi_discwelfarist.csv")

