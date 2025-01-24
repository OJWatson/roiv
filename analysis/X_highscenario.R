## UPPER ESTIMATES

# *****************
# VSLY calculations
# *****************

# extract low USA VSL value (pulling value from data frame from column "low" and row 1)
high_vsl_usa <- vsl_usa$"high"[1]

vsly_high <- vsly %>%
  group_by(iso3c, replicate) %>%
  mutate(vsl_high = high_vsl_usa*(gnipc/gnipc_usa)^1) %>%
  mutate(w_nglg = sum(Ng*lg) / sum(Ng)) %>%
  mutate(w_nglghat = sum(Ng * lghat) / sum(Ng)) %>%
  mutate(vly_high = vsl_high / w_nglg) %>%
  mutate(vly_disc_high = vsl_high / w_nglghat)

# getting total monetary value of vsly (not in percent of gdp)
vsly_undisc_high <- vsly_high %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_undisc_high = sum(lg_averted * vly_high, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(vsly_undisc_high,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
vsly_undisc_high
write.csv(vsly_undisc_high, "analysis/tables/vsly_undisc_highcsv")

# getting total monetary value of vsly (not in percent of gdp)
vsly_disc_high <- vsly_high %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_disc_high = sum(lghat_averted * vly_disc_high, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(vsly_disc_high,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
vsly_disc_high
write.csv(vsly_disc_high, "analysis/tables/vsly_disc_high.csv")

# ****************
# Monetized QALYs
# ****************

# calculate wtp thresholds using the GDP percentages
qaly_sa_high <- qaly_sa %>%
  mutate(upper_wtp_threshold = upper_wtpCI * gdppc)

# calculating the sum of QALYs averted for infections, hospitalisations and deaths
#     for each iso3c using the LOWER qaly loss value
# using lower qaly loss value because it is smaller in magnitude than the lower
#     qaly loss estimate and therefore will produce a lower qaly gain

# sum of all undiscounted qalys for infections, hospitalisations, deaths
sum_undiscqaly_iso3c_high <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(lower_qaly_loss),
    name == "hospitalisations" ~ averted
    * -(lower_qaly_loss),
    name == "deaths" ~ (((averted * -lower_qaly_loss) + lg_averted))
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
sum_undiscqaly_iso3c_high
write.csv(sum_undiscqaly_iso3c_high, "analysis/tables/sum_undiscqaly_iso3c_high.csv")


# sum of qalys for infections, hospitalisations, deaths per income group
sum_undiscqaly_income_high <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(lower_qaly_loss),
    name == "hospitalisations" ~ averted
    * -(lower_qaly_loss),
    name == "deaths" ~ (((averted * -lower_qaly_loss) + lg_averted))
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
sum_undiscqaly_income_high$income_group <- factor(sum_undiscqaly_income_high$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_undiscqaly_income_high <- sum_undiscqaly_income_high %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_undiscqaly_income_high
write.csv(sum_undiscqaly_income_high, "analysis/tables/sum_undiscqaly_income_high.csv")

# get total monetized qalys per iso3c
sum_undiscmonqaly_iso3c_high <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "deaths" ~ (((averted * -lower_qaly_loss) + lg_averted)
                        * upper_wtp_threshold)
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
sum_undiscmonqaly_iso3c_high
write.csv(sum_undiscmonqaly_iso3c_high, "analysis/tables/sum_undiscmonqaly_iso3c_high.csv")

# get total monetized qalys per income group
sum_undiscmonqaly_income_high <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "deaths" ~ (((averted * -lower_qaly_loss) + lg_averted)
                        * upper_wtp_threshold)
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


# set the factor levels of income_group in the desired order
sum_undiscmonqaly_income_high$income_group <- factor(sum_undiscmonqaly_income_high$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_undiscmonqaly_income_high <- sum_undiscmonqaly_income_high %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_undiscmonqaly_income_high
write.csv(sum_undiscmonqaly_income_high, "analysis/tables/sum_undiscmonqaly_income_high.csv")

# do the same for discounted qalys and monetized qalys
# sum of all discounted qalys for infections, hospitalisations, deaths
sum_discqaly_iso3c_high <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(lower_qaly_loss),
    name == "hospitalisations" ~ averted
    * -(lower_qaly_loss),
    name == "deaths" ~ (((averted * -lower_qaly_loss) + lghat_averted))
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
sum_discqaly_iso3c_high
write.csv(sum_discqaly_iso3c_high, "analysis/tables/sum_discqaly_iso3c_high.csv")


# sum of all monetized discounted qalys for infections, hospitalisations, deaths
sum_discmonqaly_iso3c_high <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "deaths" ~ (((averted * -lower_qaly_loss) + lghat_averted)
                        * upper_wtp_threshold)
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
sum_discmonqaly_iso3c_high
write.csv(sum_discmonqaly_iso3c_high, "analysis/tables/sum_discmonqaly_iso3c_high.csv")

# sum of qalys for infections, hospitalisations, deaths per income group
sum_discqaly_income_high <- res_full %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(lower_qaly_loss),
    name == "hospitalisations" ~ averted
    * -(lower_qaly_loss),
    name == "deaths" ~ (((averted * -lower_qaly_loss) + lghat_averted))
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

# set the factor levels of income_group in the desired order
sum_discqaly_income_high$income_group <- factor(sum_discqaly_income_high$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_discqaly_income_high <- sum_discqaly_income_high %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_discqaly_income_high
write.csv(sum_discqaly_income_high, "analysis/tables/sum_discqaly_income_high.csv")


# get total monetized qalys per income group
sum_discmonqaly_income_high <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "deaths" ~ (((averted * -lower_qaly_loss) + lghat_averted)
                        * upper_wtp_threshold)
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


# set the factor levels of income_group in the desired order
sum_discmonqaly_income_high$income_group <- factor(sum_discmonqaly_income_high$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
sum_discmonqaly_income_high <- sum_discmonqaly_income_high %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
sum_discmonqaly_income_high
write.csv(sum_discmonqaly_income_high, "analysis/tables/sum_discmonqaly_income_high.csv")


# ***************
# Friction Costs
# ***************

# calculate the lower confidence intervals for the friction periods
friction_costs_high <- friction_costs %>%
  mutate(friction_period_high = case_when(
    income_group == "HIC" ~ (friction_period + friction_sd*1.96),
    income_group %in% c("UMIC", "LMIC", "LIC") ~ (3*30.417)))

# calculate friction costs averted

friction_costs_high <- friction_costs_high %>%
  mutate(friction_costs = case_when(
    name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
    name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted,
    name == "deaths" ~ (gdppc/365.25) * friction_period_high * averted
  ))


# sum friction costs per income group

friction_costs_sum_high <- friction_costs_high %>%
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
friction_costs_sum_high$income_group <- factor(friction_costs_sum_high$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
friction_costs_sum_high <- friction_costs_sum_high %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
friction_costs_sum_high
write.csv(friction_costs_sum_high, "analysis/tables/friction_costs_sum_high.csv")


# calculating friction costs for each iso3c
friction_costs_sum_iso3c_high <- friction_costs_high %>%
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
friction_costs_sum_iso3c_high
write.csv(friction_costs_sum_iso3c_high, "analysis/tables/friction_costs_sum_iso3c_high.csv")


# ********************
# Healthcare Costs
# ********************

# naming it lowercost_total for the sensitivity analysis to match the column for hic
hc_costs_lmic_high <- hc_costs_lmic %>%
  mutate(uppercost_total = cost_pd2021 * hospitalisations_duration * averted)

# bind with lmic costs
hc_costs_grouped_high <- bind_rows(hic_hccosts_2021, hc_costs_lmic_high)

# sum costs averted for each income group

hc_costs_total_high <- hc_costs_grouped_high  %>%
  group_by(income_group, replicate) %>%
  summarise(hc_costs_total = sum(uppercost_total)) %>%
  group_by(income_group) %>%
  summarise(across(hc_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

#set the factor levels of income_group in the desired order
hc_costs_total_high$income_group <- factor(hc_costs_total_high$income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))
hc_costs_total_high <- hc_costs_total_high %>%
  arrange(income_group)

# our results table which we can then save in the tables directory
hc_costs_total_high
write.csv(hc_costs_total_high, "analysis/tables/hc_costs_total_high.csv")


# sum costs averted for each iso3c
hc_costs_total_iso3c_high <- hc_costs_grouped_high  %>%
  group_by(iso3c, replicate) %>%
  summarise(health_costs_total = sum(uppercost_total)) %>%
  group_by(iso3c) %>%
  summarise(across(health_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))


# our results table which we can then save in the tables directory
hc_costs_total_iso3c_high
write.csv(hc_costs_total_iso3c_high, "analysis/tables/hc_costs_total_iso3c_high.csv")

# ********************
# SUM
# ********************
# get total undiscounted monetized qalys
sum_undiscmonqaly_high <- res_full %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "deaths" ~ (((averted * -lower_qaly_loss) + lg_averted)
                        * upper_wtp_threshold)
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
sum_undiscmonqaly_high
write.csv(sum_undiscmonqaly_high, "analysis/tables/sum_undiscmonqaly_high.csv")

# get total monetized qalys
sum_discmonqaly_high <- res_full %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(lower_qaly_loss) * upper_wtp_threshold,
    name == "deaths" ~ (((averted * -lower_qaly_loss) + lghat_averted)
                        * upper_wtp_threshold)
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
sum_discmonqaly_high
write.csv(sum_discmonqaly_high, "analysis/tables/sum_discmonqaly_high.csv")


# calculating friction costs for each iso3c
sum_frictioncost_high <- friction_costs_high %>%
  group_by(replicate) %>%
  summarise(friction_costs_total = sum(friction_costs, na.rm = TRUE)) %>%
  summarise(across(friction_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
sum_frictioncost_high
write.csv(sum_frictioncost_high, "analysis/tables/sum_frictioncost_high.csv")

# sum costs averted for each iso3c
sum_hc_costs_high <- hc_costs_grouped_high  %>%
  group_by(replicate) %>%
  summarise(health_costs_total = sum(uppercost_total)) %>%
  summarise(across(health_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))


# our results table which we can then save in the tables directory
sum_hc_costs_high
write.csv(sum_hc_costs_high, "analysis/tables/sum_hc_costs_high.csv")


# combine the summaries - undiscounted
extrawelfarist_undiscsum_high <- bind_cols(sum_undiscmonqaly_high, sum_frictioncost_high, sum_hc_costs_high) %>%
  summarise(
    total_low =  undiscmonqalys_averted_sum_low + friction_costs_total_low + health_costs_total_low,
    total_med =  undiscmonqalys_averted_sum_med + friction_costs_total_med + health_costs_total_med,
    total_high =  undiscmonqalys_averted_sum_high + friction_costs_total_high + health_costs_total_high
  )

# save results
extrawelfarist_undiscsum_high
write.csv(extrawelfarist_undiscsum_high, "analysis/tables/extrawelfarist_undiscsum_high.csv")

# combine the summaries - discounted
extrawelfarist_discsum_high <- bind_cols(sum_discmonqaly_high, sum_frictioncost_high, sum_hc_costs_high) %>%
  summarise(
    total_low =  discmonqalys_averted_sum_low + friction_costs_total_low + health_costs_total_low,
    total_med =  discmonqalys_averted_sum_med + friction_costs_total_med + health_costs_total_med,
    total_high =  discmonqalys_averted_sum_high + friction_costs_total_high + health_costs_total_high
  )

# save results
extrawelfarist_discsum_high
write.csv(extrawelfarist_discsum_high, "analysis/tables/extrawelfarist_discsum_high.csv")

# welfarist sums

# sum of undiscounted vslys
high_undisc_welfarist_sum <- vsly_high %>%
  group_by(replicate) %>%
  summarise(total = sum(lg_averted * vly_high, na.rm = TRUE)) %>%
  summarise(
    across(total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
high_undisc_welfarist_sum
write.csv(high_undisc_welfarist_sum, "analysis/tables/high_undisc_welfarist_sum.csv")

# sum of discounted vslys
high_disc_welfarist_sum <- vsly_high %>%
  group_by(replicate) %>%
  summarise(total = sum(lghat_averted * vly_disc_high, na.rm = TRUE)) %>%
  summarise(
    across(total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
high_disc_welfarist_sum
write.csv(high_disc_welfarist_sum, "analysis/tables/high_disc_welfarist_sum.csv")

# corporate investments

corporate_high <- 14000000000 * (1/0.845)

# roi calculations
# calculate undiscounted extrawelfarist roi

high_roi_undiscextrawelfarist <- extrawelfarist_undiscsum_high %>%
  mutate(roi_low = ((total_low - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu)),
         roi_med = ((total_med - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu)),
         roi_high = ((total_high - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
high_roi_undiscextrawelfarist
write.csv(high_roi_undiscextrawelfarist, "analysis/tables/high_roi_undiscextrawelfarist.csv")

# calculate discounted extrawelfarist roi

high_roi_discextrawelfarist <- extrawelfarist_discsum_high %>%
  mutate(roi_low = ((total_low - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu)),
         roi_med = ((total_med - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu)),
         roi_high = ((total_high - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
high_roi_discextrawelfarist
write.csv(high_roi_discextrawelfarist, "analysis/tables/high_roi_discextrawelfarist.csv")

# calculate undiscounted welfarist roi
high_roi_undiscwelfarist <- high_undisc_welfarist_sum %>%
  mutate(roi_low = ((total_low - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu)),
         roi_med = ((total_med - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu)),
         roi_high = ((total_high - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
high_roi_undiscwelfarist
write.csv(high_roi_undiscwelfarist, "analysis/tables/high_roi_undiscwelfarist.csv")

# calculate discounted welfarist roi
high_roi_discwelfarist <- high_disc_welfarist_sum  %>%
  mutate(roi_low = ((total_low - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu)),
         roi_med = ((total_med - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu)),
         roi_high = ((total_high - (dev_funding + del_cost + apa + corporate_high + manu))/(dev_funding + del_cost + apa + corporate_high + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
high_roi_discwelfarist
write.csv(high_roi_discwelfarist, "analysis/tables/high_roi_discwelfarist.csv")
