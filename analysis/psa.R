ranges <- list(
  vsl = c(0,30e6), # need to double check
  wtp_hic = c(0,1),
  wtp_umic = c(0,1),
  wtp_lmic = c(0,1),
  wtp_lic = c(0,1),
  qaly_inf = c(0,1),
  qaly_hosp = c(0,1),
  qaly_death = c(0,1),
  friction_period_hic = c(0,365) # need to double check
)

lhs_samples <- lhs::randomLHS(100, length(ranges))

# VSL with mean and SD - q(gamma) requires shape and scale parameters
vsl_mean <- 10519303.5178172
vsl_sd <- 4391174.93786433
vsl_shape <- (vsl_mean / vsl_sd)^2
vsl_scale <- vsl_sd^2 / vsl_mean
vsl_samples <- qgamma(lhs_samples[, 1], shape = vsl_shape, scale = vsl_scale)

# WTP beta distribution for median and IQR
# WTP for HIC
mu_hic <- 0.68  # Median (approximate mean)
iqr_hic <- 0.88 - 0.5  # IQR
var_hic <- ((iqr_hic / 2)^2) / 3  # Variance approximation
shape1_hic <- mu_hic * (1 - mu_hic) / var_hic - 1
shape2_hic <- (1 - mu_hic) * (shape1_hic)
wtp_hic_samples <- qbeta(lhs_samples[, 2], shape1 = shape1_hic, shape2 = shape2_hic)

# WTP for UMIC
mu_umic <- 0.58  # Median (approximate mean)
iqr_umic <- 0.76 - 0.44  # IQR
var_umic <- ((iqr_umic / 2)^2) / 3  # Variance approximation
shape1_umic <- mu_umic * (1 - mu_umic) / var_umic - 1
shape2_umic <- (1 - mu_umic) * shape1_umic
wtp_umic_samples <- qbeta(lhs_samples[, 3], shape1 = shape1_umic, shape2 = shape2_umic)

# WTP for LMIC
mu_lmic <- 0.35  # Median (approximate mean)
iqr_lmic <- 0.48 - 0.23  # IQR
var_lmic <- ((iqr_lmic / 2)^2) / 3  # Variance approximation
shape1_lmic <- mu_lmic * (1 - mu_lmic) / var_lmic - 1
shape2_lmic <- (1 - mu_lmic) * shape1_lmic
wtp_lmic_samples <- qbeta(lhs_samples[, 4], shape1 = shape1_lmic, shape2 = shape2_lmic)

# WTP for LIC
mu_lic <- 0.24  # Median (approximate mean)
iqr_lic <- 0.32 - 0.18  # IQR
var_lic <- ((iqr_lic / 2)^2) / 3  # Variance approximation
shape1_lic <- mu_lic * (1 - mu_lic) / var_lic - 1
shape2_lic <- (1 - mu_lic) * shape1_lic
wtp_lic_samples <- qbeta(lhs_samples[, 5], shape1 = shape1_lic, shape2 = shape2_lic)

# QALYS - beta distribution with mean and 95% CI
# QALY for infection
mu_qaly_inf <- 0.007
lower_qaly_inf <- 0.002
upper_qaly_inf <- 0.011
k_qaly_inf <- 4 * ((mu_qaly_inf - lower_qaly_inf) / (upper_qaly_inf - mu_qaly_inf))^2
var_qaly_inf <- mu_qaly_inf * (1 - mu_qaly_inf) / (k_qaly_inf + 1)
shape1_qaly_inf <- mu_qaly_inf * (1 - mu_qaly_inf) / var_qaly_inf
shape2_qaly_inf <- (1 - mu_qaly_inf) * shape1_qaly_inf

QALY_infection_samples <- qbeta(lhs_samples[, 6], shape1 = shape1_qaly_inf, shape2 = shape2_qaly_inf)

# QALYs for hospitalisations
mu_qaly_hosp <- 0.00009
lower_qaly_hosp <- 0.00001
upper_qaly_hosp <- 0.00028
k_qaly_hosp <- 4 * ((mu_qaly_hosp - lower_qaly_hosp) / (upper_qaly_hosp - mu_qaly_hosp))^2
var_qaly_hosp <- mu_qaly_hosp * (1 - mu_qaly_hosp) / (k_qaly_hosp + 1)
shape1_qaly_hosp <- mu_qaly_hosp * (1 - mu_qaly_hosp) / var_qaly_hosp
shape2_qaly_hosp <- (1 - mu_qaly_hosp) * shape1_qaly_hosp

QALY_hospitalisations_samples <- qbeta(lhs_samples[, 7], shape1 = shape1_qaly_hosp, shape2 = shape2_qaly_hosp)

# QALYs for deaths

mu_qaly_death <- 0.048
lower_qaly_death <- 0.011
upper_qaly_death <- 0.106
k_qaly_death <- 4 * ((mu_qaly_death - lower_qaly_death) / (upper_qaly_death - mu_qaly_death))^2
var_qaly_death <- mu_qaly_death * (1 - mu_qaly_death) / (k_qaly_death + 1)
shape1_qaly_death <- mu_qaly_death * (1 - mu_qaly_death) / var_qaly_death
shape2_qaly_death <- (1 - mu_qaly_death) * shape1_qaly_death

QALY_deaths_samples <- qbeta(lhs_samples[, 8], shape1 = shape1_qaly_death, shape2 = shape2_qaly_death)



# Friction periods for HICs
frictionperiod_samples <- qnorm(lhs_samples[,9], 60.6, 14.8)

sens_df <- data.frame("replicate" = 1:100,
                      "vsl_samples" = vsl_samples,
                      "wtp_hic_samples" = wtp_hic_samples,
                      "wtp_umic_samples" = wtp_umic_samples,
                      "wtp_lmic_samples" = wtp_lmic_samples,
                      "wtp_lic_samples" = wtp_lic_samples,
                      "QALY_infection_samples" = QALY_infection_samples,
                      "QALY_hospitalisations_samples" = QALY_hospitalisations_samples,
                      "QALY_deaths_samples" = QALY_deaths_samples,
                      "frictionperiod_samples" = frictionperiod_samples)

# assign the sampling values to the analysis and using it for calculations

# vsly
vsly_psa <- vsly %>%
  left_join(sens_df, by = "replicate") %>%  # Join based on replicate
  mutate(vsl_usa = vsl_samples)

vsly_psa <- vsly_psa %>%
  group_by(iso3c, replicate) %>%
  mutate(vsl = vsl_usa*(gnipc/gnipc_usa)^1) %>%
  mutate(w_nglg = sum(Ng*lg) / sum(Ng)) %>%
  mutate(w_nglghat = sum(Ng * lghat) / sum(Ng)) %>%
  mutate(vly = vsl / w_nglg) %>%
  mutate(vly_disc = vsl / w_nglghat)


# getting total monetary value of vsly per income group (population-weighted)
vsly_avertedtotals_psa <- vsly_psa %>%
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
vsly_avertedtotals_psa
write.csv(vsly_avertedtotals_psa, "analysis/tables/vsly_avertedtotals_psa.csv")

# wtp thresholds and monetized qalys
# undiscounted
sum_undiscmonqaly_psa <- res_full %>%
  left_join(sens_df, by = "replicate") %>%
  mutate(median_wtp_threshold = case_when(
    income_group == "HIC" ~ wtp_hic_samples,
    income_group == "UMIC" ~ wtp_umic_samples,
    income_group == "LMIC" ~ wtp_lmic_samples,
    income_group == "LIC" ~ wtp_lic_samples)) %>%
  mutate(qaly_loss = case_when(
    name == "infections" ~ QALY_infection_samples,
    name == "hospitalisations" ~ QALY_hospitalisations_samples,
    name == "deaths" ~ QALY_deaths_samples
  )) %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * (qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * (qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * qaly_loss) + lg_averted)
                        * median_wtp_threshold)
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
sum_undiscmonqaly_psa
write.csv(sum_undiscmonqaly_psa, "analysis/tablessum_undiscmonqaly_psa.csv")

# discounted
sum_discmonqaly_psa <- res_full %>%
  left_join(sens_df, by = "replicate") %>%
  mutate(median_wtp_threshold = case_when(
    income_group == "HIC" ~ wtp_hic_samples,
    income_group == "UMIC" ~ wtp_umic_samples,
    income_group == "LMIC" ~ wtp_lmic_samples,
    income_group == "LIC" ~ wtp_lic_samples)) %>%
  mutate(qaly_loss = case_when(
    name == "infections" ~ QALY_infection_samples,
    name == "hospitalisations" ~ QALY_hospitalisations_samples,
    name == "deaths" ~ QALY_deaths_samples
  )) %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * (qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * (qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * qaly_loss) + lghat_averted)
                        * median_wtp_threshold)
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
sum_discmonqaly_psa
write.csv(sum_discmonqaly_psa, "analysis/tables/sum_discmonqaly_psa.csv")

# friction costs
friction_costs_psa <- friction_costs %>%
  left_join(sens_df, by = "replicate") %>%
  mutate(friction_period = case_when(
    income_group == "HIC" ~ frictionperiod_samples,
    income_group %in% c("UMIC", "LMIC", "LIC") ~ (3*30.417)
  )) %>%
  group_by(replicate) %>%
  summarise(friction_costs = sum(friction_costs, na.rm = TRUE)) %>%
  summarise(across(friction_costs,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
friction_costs_psa
write.csv(friction_costs_psa, "analysis/friction_costs_psa.csv")


# combine the summaries - undiscounted
extrawelfarist_undiscsum_psa <- bind_cols(sum_undiscmonqaly_psa, friction_costs_psa, sum_hc_costs) %>%
  summarise(
    total_low = undiscmonqalys_averted_sum_low + friction_costs_low + hc_costs_total_low,
    total_med = undiscmonqalys_averted_sum_med + friction_costs_med + hc_costs_total_med,
    total_high = undiscmonqalys_averted_sum_high + friction_costs_high + hc_costs_total_high
  )

# save results
extrawelfarist_undiscsum_psa
write.csv(extrawelfarist_undiscsum_psa, "analysis/tables/extrawelfarist_undiscsum_psa.csv")

# combine the summaries - discounted
extrawelfarist_discsum_psa <- bind_cols(sum_discmonqaly_psa, friction_costs_psa, sum_hc_costs) %>%
  summarise(
    total_low = discmonqalys_averted_sum_low + friction_costs_low + hc_costs_total_low,
    total_med = discmonqalys_averted_sum_med + friction_costs_med + hc_costs_total_med,
    total_high = discmonqalys_averted_sum_high + friction_costs_high + hc_costs_total_high
  )

# save results
extrawelfarist_discsum_psa
write.csv(extrawelfarist_discsum_psa, "analysis/tables/extrawelfarist_discsum_psa.csv")

# calculate undiscounted extrawelfarist roi

roi_undiscextrawelfarist_psa <- extrawelfarist_undiscsum_psa %>%
  mutate(roi_low = ((total_low - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_med = ((total_med - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_high = ((total_high - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_undiscextrawelfarist_psa
write.csv(roi_undiscextrawelfarist_psa, "analysis/tables/roi_undiscextrawelfarist_psa.csv")

# calculate discounted extrawelfarist roi

roi_discextrawelfarist_psa <- extrawelfarist_discsum_psa %>%
  mutate(roi_low = ((total_low - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_med = ((total_med - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_high = ((total_high - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_discextrawelfarist_psa
write.csv(roi_discextrawelfarist_psa, "analysis/tables/roi_discextrawelfarist_psa.csv")

# welfarist - VSLYs


# calculate undiscounted welfarist roi
roi_undiscwelfarist_psa <- vsly_avertedtotals_psa %>%
  mutate(roi_low = ((vsly_undisc_averted_low - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_med = ((vsly_undisc_averted_med - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_high = ((vsly_undisc_averted_high - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_undiscwelfarist_psa
write.csv(roi_undiscwelfarist_psa, "analysis/tables/roi_undiscwelfarist_psa.csv")


# calculate discounted extrawelfarist roi
roi_discwelfarist_psa <- vsly_avertedtotals_psa %>%
  mutate(roi_low = ((vsly_disc_averted_low - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_med = ((vsly_disc_averted_med - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_high = ((vsly_disc_averted_high - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_discwelfarist_psa
write.csv(roi_discwelfarist_psa, "analysis/tables/roi_discwelfarist_psa.csv")
