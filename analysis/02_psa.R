library(lhs)

ranges <- list(
  vsl = c(0, 30e6), # need to double check
  wtp_hic = c(0,1),
  wtp_umic = c(0,1),
  wtp_lmic = c(0,1),
  wtp_lic = c(0,1),
  qaly_inf = c(0,1),
  qaly_hosp = c(0,1),
  qaly_death = c(0,1),
  friction_period_hic = c(0,365) # need to double check
)

set.seed(123L)
lhs_samples <- lhs::randomLHS(100, length(ranges))

# VSL with mean and SD - q(gamma) requires shape and scale parameters
vsl_mean <- 10519303.5178172
vsl_sd <- 4391174.93786433
vsl_shape <- (vsl_mean / vsl_sd)^2
vsl_scale <- vsl_sd^2 / vsl_mean
vsl_samples <- qgamma(lhs_samples[, 1], shape = vsl_shape, scale = vsl_scale)


# vsl data - $1,912,601	$10,519,304	$19,126,006

# lets check if this looks sensible by comparing against following (or wherever you got your VSL from as they likely have a low and high)
# https://aspe.hhs.gov/sites/default/files/2021-07/hhs-guidelines-appendix-d-vsl-update.pdf
quantile(vsl_samples, c(0.025, 0.975))

## TODO: Address whether this Gamma makes sense against literature and skew knowledge

## TODO: Bring in correct way to get dist below ----------
# The reason for doing the below is that your WTP samples do not align with your
# desired summary stats, e.g. summary(wtp_hic_samples) should give an IQR and median that you have found in literature

# # Define median and IQR
# mu_hic <- 0.68       # Median (approximate mean)
# iqr_hic <- 0.88 - 0.50  # IQR
#
# # Define a function to minimise the difference between observed and beta parameters
# estimate_beta <- function(params) {
#   shape1 <- params[1]
#   shape2 <- params[2]
#
#   # Compute theoretical median and IQR from the beta distribution
#   median_theoretical <- qbeta(0.5, shape1, shape2)
#   iqr_theoretical <- qbeta(0.75, shape1, shape2) - qbeta(0.25, shape1, shape2)
#
#   # Calculate squared differences
#   median_diff <- (median_theoretical - mu_hic)^2
#   iqr_diff <- (iqr_theoretical - iqr_hic)^2
#
#   return(median_diff + iqr_diff)
# }
#
# # Use optimisation to find shape1 and shape2
# optim_result <- optim(
#   par = c(1, 1), # Initial guesses for shape1 and shape2
#   fn = estimate_beta,
#   method = "L-BFGS-B",
#   lower = c(0.01, 0.01) # Parameters must be positive
# )
#
# # Extract results
# shape1_est <- optim_result$par[1]
# shape2_est <- optim_result$par[2]


# WTP for HICs

# WTP beta distribution for median and IQR -------
# WTP for HIC

mu_hic <- 0.68  # Median (approximate mean)
iqr_hic <- 0.88 - 0.5  # IQR
var_hic <- ((iqr_hic / 2)^2) / 3  # Variance approximation
shape1_hic <- mu_hic * (1 - mu_hic) / var_hic - 1
shape2_hic <- (1 - mu_hic) * (shape1_hic)


estimate_beta <- function(params) {
   shape1 <- params[1]
   shape2 <- params[2]
  # Compute theoretical median and IQR from the beta distribution
   median_theoretical <- qbeta(0.5, shape1, shape2)
   iqr_theoretical <- qbeta(0.75, shape1, shape2) - qbeta(0.25, shape1, shape2)

  # Calculate squared differences
   median_diff <- (median_theoretical - mu_hic)^2
   iqr_diff <- (iqr_theoretical - iqr_hic)^2
   return(median_diff + iqr_diff)
  }

# Use optimisation to find shape1 and shape2
  optim_result_wtp_hic <- optim(
      par = c(17.0831, 5.466593), # Initial guesses for shape1 and shape2
      fn = estimate_beta,
      method = "L-BFGS-B",
      lower = c(0.01, 0.01) # Parameters must be positive
    )

  # Extract results
   shape1_est_hic <- optim_result_wtp_hic$par[1]
   shape2_est_hic <- optim_result_wtp_hic$par[2]

wtp_hic_samples <- qbeta(lhs_samples[, 2], shape1 = shape1_est_hic, shape2 = shape2_est_hic)


# WTP for UMIC
mu_umic <- 0.58  # Median (approximate mean)
iqr_umic <- 0.76 - 0.44  # IQR
var_umic <- ((iqr_umic / 2)^2) / 3  # Variance approximation
shape1_umic <- mu_umic * (1 - mu_umic) / var_umic - 1
shape2_umic <- (1 - mu_umic) * shape1_umic


estimate_beta_WTP_umic <- function(params) {
  shape1 <- params[1]
  shape2 <- params[2]
  # Compute theoretical median and IQR from the beta distribution
  median_theoretical <- qbeta(0.5, shape1, shape2)
  iqr_theoretical <- qbeta(0.75, shape1, shape2) - qbeta(0.25, shape1, shape2)

  # Calculate squared differences
  median_diff <- (median_theoretical - mu_umic)^2
  iqr_diff <- (iqr_theoretical - iqr_umic)^2
  return(median_diff + iqr_diff)
}


# Use optimisation to find shape1 and shape2
optim_result_wtp_umic <- optim(
  par = c(27.54688, 11.56969), # Initial guesses for shape1 and shape2
  fn = estimate_beta,
  method = "L-BFGS-B",
  lower = c(0.01, 0.01) # Parameters must be positive
)

# Extract results
shape1_est_umic <- optim_result_wtp_umic$par[1]
shape2_est_umic <- optim_result_wtp_umic$par[2]

wtp_umic_samples <- qbeta(lhs_samples[, 3], shape1 = shape1_est_umic, shape2 = shape2_est_umic)

# WTP for LMIC
mu_lmic <- 0.35  # Median (approximate mean)
iqr_lmic <- 0.48 - 0.23  # IQR
var_lmic <- ((iqr_lmic / 2)^2) / 3  # Variance approximation
shape1_lmic <- mu_lmic * (1 - mu_lmic) / var_lmic - 1
shape2_lmic <- (1 - mu_lmic) * shape1_lmic

estimate_beta_wtp_lmic <- function(params) {
  shape1 <- params[1]
  shape2 <- params[2]
  # Compute theoretical median and IQR from the beta distribution
  median_theoretical <- qbeta(0.5, shape1, shape2)
  iqr_theoretical <- qbeta(0.75, shape1, shape2) - qbeta(0.25, shape1, shape2)

  # Calculate squared differences
  median_diff <- (median_theoretical - mu_lmic)^2
  iqr_diff <- (iqr_theoretical - iqr_lmic)^2
  return(median_diff + iqr_diff)
}


# Use optimisation to find shape1 and shape2
optim_result_wtp_lmic <- optim(
  par = c(42.68, 27.742), # Initial guesses for shape1 and shape2
  fn = estimate_beta,
  method = "L-BFGS-B",
  lower = c(0.01, 0.01) # Parameters must be positive
)

# Extract results
shape1_est_lmic <- optim_result_wtp_lmic$par[1]
shape2_est_lmic <- optim_result_wtp_lmic$par[2]

wtp_lmic_samples <- qbeta(lhs_samples[, 4], shape1 = shape1_est_lmic, shape2 = shape2_est_lmic)

# WTP for LIC
mu_lic <- 0.24  # Median (approximate mean)
iqr_lic <- 0.32 - 0.18  # IQR
var_lic <- ((iqr_lic / 2)^2) / 3  # Variance approximation
shape1_lic <- mu_lic * (1 - mu_lic) / var_lic - 1
shape2_lic <- (1 - mu_lic) * shape1_lic

estimate_beta_wtp_lic <- function(params) {
  shape1 <- params[1]
  shape2 <- params[2]
  # Compute theoretical median and IQR from the beta distribution
  median_theoretical <- qbeta(0.5, shape1, shape2)
  iqr_theoretical <- qbeta(0.75, shape1, shape2) - qbeta(0.25, shape1, shape2)

  # Calculate squared differences
  median_diff <- (median_theoretical - mu_lic)^2
  iqr_diff <- (iqr_theoretical - iqr_lic)^2
  return(median_diff + iqr_diff)
}


# Use optimisation to find shape1 and shape2
optim_result_wtp_lic <- optim(
  par = c(110.6735, 84.11184), # Initial guesses for shape1 and shape2
  fn = estimate_beta,
  method = "L-BFGS-B",
  lower = c(0.01, 0.01) # Parameters must be positive
)

# Extract results
shape1_est_lic <- optim_result_wtp_lic$par[1]
shape2_est_lic <- optim_result_wtp_lic$par[2]

wtp_lic_samples <- qbeta(lhs_samples[, 5], shape1 = shape1_est_lic, shape2 = shape2_est_lic)

# QALYS - beta distribution with mean and 95% CI

## TODO: These shape and scale approximations are not giving the right values
## You can check if it is working correct by looking at your samples:
## quantile(QALY_infection_samples, c(0.025, 0.975)) - this should be close to your lower and upper
## mean(QALY_infection_samples) - this should be close to your mu (assuming this is mean)
## the above should have

# QALY for infection
# Calculate the variance from the range (lower, upper bounds) assuming a uniform Beta distribution
mu_qaly_inf <- 0.007
lower_qaly_inf <- 0.002
upper_qaly_inf <- 0.011
variance_qaly_inf <- (upper_qaly_inf - lower_qaly_inf)^2 / 12

# Derive the Beta distribution shape parameters from mean and variance
shape1_qaly_inf <- mu_qaly_inf * (mu_qaly_inf * (1 - mu_qaly_inf) / variance_qaly_inf - 1)
shape2_qaly_inf <- (1 - mu_qaly_inf) * (mu_qaly_inf * (1 - mu_qaly_inf) / variance_qaly_inf - 1)

# Generate the samples
QALY_infection_samples <- qbeta(lhs_samples[, 6], shape1 = shape1_qaly_inf, shape2 = shape2_qaly_inf)

# QALYs for hospitalisations
mu_qaly_hosp <- 0.00009
lower_qaly_hosp <- 0.00001
upper_qaly_hosp <- 0.00028

# Estimate variance assuming a uniform range for simplicity
variance_qaly_hosp <- (upper_qaly_hosp - lower_qaly_hosp)^2 / 12

# Derive shape parameters from mean and variance
shape1_qaly_hosp <- mu_qaly_hosp * (mu_qaly_hosp * (1 - mu_qaly_hosp) / variance_qaly_hosp - 1)
shape2_qaly_hosp <- (1 - mu_qaly_hosp) * (mu_qaly_hosp * (1 - mu_qaly_hosp) / variance_qaly_hosp - 1)

# Generate samples using the Beta distribution
QALY_hospitalisations_samples <- qbeta(lhs_samples[, 7], shape1 = shape1_qaly_hosp, shape2 = shape2_qaly_hosp)

# QALYs for deaths
mu_qaly_death <- 0.048
lower_qaly_death <- 0.011
upper_qaly_death <- 0.106

# Estimate variance assuming a uniform range for simplicity
variance_qaly_death <- (upper_qaly_death - lower_qaly_death)^2 / 12

# Derive shape parameters from mean and variance
shape1_qaly_death <- mu_qaly_death * (mu_qaly_death * (1 - mu_qaly_death) / variance_qaly_death - 1)
shape2_qaly_death <- (1 - mu_qaly_death) * (mu_qaly_death * (1 - mu_qaly_death) / variance_qaly_death - 1)

# Generate samples using the Beta distribution
QALY_deaths_samples <- qbeta(lhs_samples[, 8], shape1 = shape1_qaly_death, shape2 = shape2_qaly_death)

# check
quantile(QALY_infection_samples, c(0.025, 0.975))
quantile(QALY_hospitalisations_samples, c(0.025, 0.975))
quantile(QALY_deaths_samples, c(0.025, 0.975))

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
saveRDS(sens_df, "analysis/data/derived/psa_sens_df.rds")

# assign the sampling values to the analysis and using it for calculations

# vsly

vsly <- readRDS("analysis/data/derived/vsly.rds")
gnipc_usa <- read_csv("analysis/data/raw/GNIPC_2021.csv") %>% filter(iso3c == "USA") %>% pull(gnipc)

vsly_psa <- vsly %>%
  left_join(sens_df, by = "replicate") %>%  # Join based on replicate
  mutate(vsl_usa = vsl_samples) %>%
  group_by(iso3c, replicate) %>%
  mutate(vsl = vsl_usa*(gnipc/gnipc_usa)^1) %>%
  mutate(w_nglg = sum(Ng*lg) / sum(Ng)) %>%
  mutate(w_nglghat = sum(Ng * lghat) / sum(Ng)) %>%
  mutate(vly = vsl / w_nglg) %>%
  mutate(vly_disc = vsl / w_nglghat)

# add uncertainty functions
lf <- function(x){quantile(x, 0.025, na.rm=TRUE)}
mf <- function(x){quantile(x, 0.5, na.rm=TRUE)}
hf <- function(x){quantile(x, 0.975, na.rm=TRUE)}

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

qaly <- readRDS("analysis/data/derived/qaly.rds")

# undiscounted
sum_undiscmonqaly_psa <- qaly %>%
  left_join(sens_df, by = "replicate") %>%
  mutate(wtp = case_when(
    income_group == "HIC" ~ wtp_hic_samples,
    income_group == "UMIC" ~ wtp_umic_samples,
    income_group == "LMIC" ~ wtp_lmic_samples,
    income_group == "LIC" ~ wtp_lic_samples)) %>%
  mutate(qaly_loss = case_when(
    name == "infections" ~ QALY_infection_samples,
    name == "hospitalisations" ~ QALY_hospitalisations_samples,
    name == "deaths" ~ QALY_deaths_samples
  )) %>%
  mutate(wtp_threshold = wtp*gdppc) %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * (qaly_loss) * wtp_threshold,
    name == "hospitalisations" ~ averted
    * (qaly_loss) * wtp_threshold,
    name == "deaths" ~ (((averted * qaly_loss) + lg_averted)
                        * wtp_threshold)
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
write.csv(sum_undiscmonqaly_psa, "analysis/tables/sum_undiscmonqaly_psa.csv")

# discounted
sum_discmonqaly_psa <- qaly %>%
  left_join(sens_df, by = "replicate") %>%
  mutate(wtp = case_when(
    income_group == "HIC" ~ wtp_hic_samples,
    income_group == "UMIC" ~ wtp_umic_samples,
    income_group == "LMIC" ~ wtp_lmic_samples,
    income_group == "LIC" ~ wtp_lic_samples)) %>%
  mutate(qaly_loss = case_when(
    name == "infections" ~ QALY_infection_samples,
    name == "hospitalisations" ~ QALY_hospitalisations_samples,
    name == "deaths" ~ QALY_deaths_samples
  )) %>%
  mutate(wtp_threshold = wtp*gdppc) %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * (qaly_loss) * wtp_threshold,
    name == "hospitalisations" ~ averted
    * (qaly_loss) * wtp_threshold,
    name == "deaths" ~ (((averted * qaly_loss) + lghat_averted)
                        * wtp_threshold)
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
friction_costs <- readRDS("analysis/data/derived/friction_costs.rds")

friction_costs_psa <- friction_costs %>%
  left_join(sens_df, by = "replicate") %>%
  mutate(friction_period = case_when(
    income_group == "HIC" ~ frictionperiod_samples,
    income_group %in% c("UMIC", "LMIC", "LIC") ~ (3*30.417)
  )) %>%
  mutate(friction_costs = case_when(
  name == "infections" ~ (gdppc/365.25) * infections_duration * averted,
  name == "hospitalisations" ~ (gdppc/365.25) * hospitalisations_duration * averted,
  name == "deaths" ~ (gdppc/365.25) * friction_period * averted
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
write.csv(friction_costs_psa, "analysis/tables/friction_costs_psa.csv")

# call in healthcare costs
sum_hc_costs <- readRDS("analysis/data/derived/sum_hc_costs.rds")

# call in vaccine costs

vaccine_costs <- readRDS("analysis/data/derived/vaccine_costs.rds")

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
  mutate(roi_low = ((total_low - (vaccine_costs))/(vaccine_costs)),
         roi_med = ((total_med - (vaccine_costs))/(vaccine_costs)),
         roi_high = ((total_high - (vaccine_costs))/(vaccine_costs))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_undiscextrawelfarist_psa
write.csv(roi_undiscextrawelfarist_psa, "analysis/tables/roi_undiscextrawelfarist_psa.csv")

# calculate discounted extrawelfarist roi

roi_discextrawelfarist_psa <- extrawelfarist_discsum_psa %>%
  mutate(roi_low = ((total_low - (vaccine_costs))/(vaccine_costs)),
         roi_med = ((total_med - (vaccine_costs))/(vaccine_costs)),
         roi_high = ((total_high - (vaccine_costs))/(vaccine_costs))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_discextrawelfarist_psa
write.csv(roi_discextrawelfarist_psa, "analysis/tables/roi_discextrawelfarist_psa.csv")

# welfarist - VSLYs


# calculate undiscounted welfarist roi
roi_undiscwelfarist_psa <- vsly_avertedtotals_psa %>%
  mutate(roi_low = ((vsly_undisc_averted_low - (vaccine_costs))/(vaccine_costs)),
         roi_med = ((vsly_undisc_averted_med - (vaccine_costs))/(vaccine_costs)),
         roi_high = ((vsly_undisc_averted_high - (vaccine_costs))/(vaccine_costs))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_undiscwelfarist_psa
write.csv(roi_undiscwelfarist_psa, "analysis/tables/roi_undiscwelfarist_psa.csv")


# calculate discounted extrawelfarist roi
roi_discwelfarist_psa <- vsly_avertedtotals_psa %>%
  mutate(roi_low = ((vsly_disc_averted_low - (vaccine_costs))/(vaccine_costs)),
         roi_med = ((vsly_disc_averted_med - (vaccine_costs))/(vaccine_costs)),
         roi_high = ((vsly_disc_averted_high - (vaccine_costs))/(vaccine_costs))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_discwelfarist_psa
write.csv(roi_discwelfarist_psa, "analysis/tables/roi_discwelfarist_psa.csv")

