## UPPER ESTIMATES

# *****************
# VSLY calculations
# *****************

# extract high USA VSL value (pulling value from data frame from column "low" and row 1)
high_vsl_usa <- vsl_usa$"high"[1]

# add new data frame for low vsly calculations
vsl_high <- vsly %>%
  group_by(iso3c, replicate) %>%
  mutate(vsl_high = high_vsl_usa*(gnipc/gnipc_usa)^1)

# add a column for vsly
vsly_high <- vsl_high %>%
  mutate(vsly_undiscounted_high = vsl_high * lg_averted) %>%
  mutate(vsly_discounted_high = vsl_high * lghat_averted)

# getting total monetary value of vsly (not in percent of gdp)
vsly_undiscounted_high <- vsly_high %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_undiscounted = sum(vsly_undiscounted_high, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(vsly_undiscounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
vsly_undiscounted_high
write.csv(vsly_undiscounted_high, "analysis/tables/vsly_undiscounted_high.csv")

# do the same for discounted vsly
vsly_discounted_high <- vsly_high %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_discounted = sum(vsly_discounted_high, na.rm = TRUE)) %>%
  group_by(income_group) %>%
  summarise(
    across(vsly_discounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
vsly_discounted_high
write.csv(vsly_discounted_high, "analysis/tables/vsly_discounted_high.csv")

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
    name == "infections" ~ averted * infections_duration
    * -(lower_qaly_loss/365.25),
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(lower_qaly_loss/365.25),
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
    name == "infections" ~ averted * infections_duration
    * -(lower_qaly_loss/365.25),
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(lower_qaly_loss/365.25),
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
    name == "infections" ~ averted * infections_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
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
    name == "infections" ~ averted * infections_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
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
    name == "infections" ~ averted * infections_duration
    * -(lower_qaly_loss/365.25),
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(lower_qaly_loss/365.25),
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
    name == "infections" ~ averted * infections_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
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
    name == "infections" ~ averted * infections_duration
    * -(lower_qaly_loss/365.25),
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(lower_qaly_loss/365.25),
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
    name == "infections" ~ averted * infections_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
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


# ********************
# Human Capital Costs
# ********************

# data was all point estimates with no uncertainty ranges reported -
#    could change durations to see how that would affect the human capital costs averted?


# ***************
# Friction Costs
# ***************

# calculate the lower confidence intervals for the friction periods
friction_costs_high <- friction_costs %>%
  mutate(friction_period_high = friction_period + (friction_sd*1.96))

# calculate friction costs averted

friction_costs_high <- friction_costs_high %>%
  mutate(friction_costs = (gdppc/365.25) * friction_period_high * averted)


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
    name == "infections" ~ averted * infections_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
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
    name == "infections" ~ averted * infections_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
    name == "hospitalisations" ~ averted * hospitalisations_duration
    * -(lower_qaly_loss/365.25) * upper_wtp_threshold,
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


# sum of human capital costs --- same as base case but including it for consistency
sum_humancapital_high <- human_capital %>%
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
sum_humancapital_high
write.csv(sum_humancapital_high, "analysis/tables/sum_humancapital_high.csv")

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
welfarist_undiscsum_high <- bind_cols(sum_undiscmonqaly_high, sum_humancapital_high, sum_frictioncost_high, sum_hc_costs_high) %>%
  summarise(
    total_low =  undiscmonqalys_averted_sum_low + humcap_total_low + friction_costs_total_low + health_costs_total_low,
    total_med =  undiscmonqalys_averted_sum_med + humcap_total_med + friction_costs_total_med + health_costs_total_med,
    total_high =  undiscmonqalys_averted_sum_high + humcap_total_high + friction_costs_total_high + health_costs_total_high
  )

# save results
welfarist_undiscsum_high
write.csv(welfarist_undiscsum_high, "analysis/tables/welfarist_undiscsum_high.csv")

# combine the summaries - discounted
welfarist_discsum_high <- bind_cols(sum_discmonqaly_high, sum_humancapital_high, sum_frictioncost_high, sum_hc_costs_high) %>%
  summarise(
    total_low =  discmonqalys_averted_sum_low + humcap_total_low + friction_costs_total_low + health_costs_total_low,
    total_med =  discmonqalys_averted_sum_med + humcap_total_med + friction_costs_total_med + health_costs_total_med,
    total_high =  discmonqalys_averted_sum_high + humcap_total_high + friction_costs_total_high + health_costs_total_high
  )

# save results
welfarist_discsum_high
write.csv(welfarist_discsum_high, "analysis/tables/welfarist_discsum_high.csv")

# extra-welfarist sums

# sum of undiscounted vslys
sum_undiscvsly_high <- vsly_high %>%
  group_by(replicate) %>%
  summarise(vsly_undiscounted = sum(vsly_undiscounted_high, na.rm = TRUE)) %>%
  summarise(
    across(vsly_undiscounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_undiscvsly_high
write.csv(sum_undiscvsly_high, "analysis/tables/sum_undiscvsly_high.csv")

# do the same for discounted vsly
sum_discvsly_high <- vsly_high %>%
  group_by(replicate) %>%
  summarise(vsly_discounted = sum(vsly_discounted_high, na.rm = TRUE)) %>%
  summarise(
    across(vsly_discounted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
sum_discvsly_high
write.csv(sum_discvsly_high, "analysis/tables/sum_discvsly_high.csv")

# roi calculations
# calculate undiscounted welfarist roi

highsa_roi_undiscwelfarist_low <- ((welfarist_undiscsum_high$total_low) -
                                    (dev_funding + del_cost$del_cost_total_low + apa))/(dev_funding + del_cost$del_cost_total_low + apa)

highsa_roi_undiscwelfarist_med <- ((welfarist_undiscsum_high$total_med) -
                                    (dev_funding + del_cost$del_cost_total_med + apa))/(dev_funding + del_cost$del_cost_total_med + apa)

highsa_roi_undiscwelfarist_high <- ((welfarist_undiscsum_high$total_high) -
                                     (dev_funding + del_cost$del_cost_total_high + apa))/(dev_funding + del_cost$del_cost_total_high + apa)

# calculate discounted welfarist roi

highsa_roi_discwelfarist_low <- ((welfarist_discsum_high$total_low) -
                                  (dev_funding + del_cost$del_cost_total_low + apa))/(dev_funding + del_cost$del_cost_total_low + apa)

highsa_roi_discwelfarist_med <- ((welfarist_discsum_high$total_med) -
                                  (dev_funding + del_cost$del_cost_total_med + apa))/(dev_funding + del_cost$del_cost_total_med + apa)

highsa_roi_discwelfarist_high <- ((welfarist_discsum_high$total_high) -
                                   (dev_funding + del_cost$del_cost_total_high + apa))/(dev_funding + del_cost$del_cost_total_high + apa)

# undisc extra welfarist roi calculation

highsa_roi_undiscextrawelfarist_low <- (sum_undiscvsly_high$vsly_undiscounted_low -
                                         (dev_funding + del_cost$del_cost_total_low + apa))/(dev_funding + del_cost$del_cost_total_low + apa)

highsa_roi_undiscextrawelfarist_med <- (sum_undiscvsly_high$vsly_undiscounted_med -
                                         (dev_funding + del_cost$del_cost_total_med + apa))/(dev_funding + del_cost$del_cost_total_med + apa)

highsa_roi_undiscextrawelfarist_high <- (sum_undiscvsly_high$vsly_undiscounted_high -
                                          (dev_funding + del_cost$del_cost_total_high + apa))/(dev_funding + del_cost$del_cost_total_high + apa)

# roi calculation for discounted extra-welfarist

highsa_roi_discextrawelfarist_low <- (sum_discvsly_high$vsly_discounted_low -
                                       (dev_funding + del_cost$del_cost_total_low + apa))/(dev_funding + del_cost$del_cost_total_low + apa)

highsa_roi_discextrawelfarist_med <- (sum_discvsly_high$vsly_discounted_med -
                                       (dev_funding + del_cost$del_cost_total_med + apa))/(dev_funding + del_cost$del_cost_total_med + apa)

highsa_roi_discextrawelfarist_high <- (sum_discvsly_high$vsly_discounted_high -
                                        (dev_funding + del_cost$del_cost_total_high + apa))/(dev_funding + del_cost$del_cost_total_high + apa)


