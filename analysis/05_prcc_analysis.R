# Step 0: load packages and files ------

# Load required packages
install.packages("epiR")
library(epiR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(readr)
library(patchwork)

# Read in and format data
sens_df <- readRDS("analysis/data/derived/psa_sens_df.rds")
vsl <- readRDS("analysis/data/derived/vsl.rds")
qaly <- readRDS("analysis/data/derived/qaly.rds")
vsly <- readRDS("analysis/data/derived/vsly.rds")
gnipc_usa <- read_csv("analysis/data/raw/gnipc_good.csv") %>% filter(iso3c == "USA") %>% pull(gnipc)
epi_psa <- readRDS("analysis/data/derived/epi_psa.rds")
friction_costs <- readRDS("analysis/data/derived/friction_costs.rds")

# You need to actually recalcuate these so that your sampled values are actually being used in the calculations
vsly_psa <- vsly %>%
  left_join(sens_df, by = "replicate") %>%  # Join based on replicate
  mutate(vsl_usa = vsl_samples) %>%
  group_by(replicate) %>%
  mutate(vsl = vsl_usa*(gnipc/gnipc_usa)^1) %>%
  mutate(w_nglg = sum(Ng*lg) / sum(Ng)) %>%
  mutate(w_nglghat = sum(Ng * lghat) / sum(Ng)) %>%
  mutate(vly = vsl / w_nglg) %>%
  mutate(vly_disc = vsl / w_nglghat)

vsly_replicate_summary <- vsly_psa %>%
  group_by(replicate) %>%
  summarise(vsly_undisc_averted = sum(lg_averted * vly, na.rm = TRUE),
            vsly_disc_averted = sum(lghat_averted * vly_disc, na.rm = TRUE)) %>%
  ungroup()  # Remove replicate grouping to keep only one row per replicate

# NEW: vsl
vsl_psa <- vsl %>%
  left_join(sens_df, by = "replicate") %>%  # Join based on replicate
  mutate(vsl_usa = vsl_samples) %>%
  group_by(iso3c, replicate) %>%
  mutate(vsl = vsl_usa*(gnipc/gnipc_usa)^1)

vsl_replicate_summary <- vsl_psa %>%
  group_by(replicate) %>%
  summarise(vsl_averted = sum(vsl*averted, na.rm = TRUE)) %>%
  ungroup()

undiscmonqaly_replicate_summary <- qaly %>%
  left_join(sens_df, by = "replicate") %>%
  mutate(
    # Map each age_group to the correct asymptomatic probability sample
    prob_asymp = case_when(
      age_group %in% c("0-5","5-10","10-15","15-20") ~ asymp_prob_young_samples,
      age_group %in% c("20-25","25-30","30-35","35-40","40-45","45-50","55-60") ~ asymp_prob_adult_samples,
      age_group %in% c("60-65","65-70","70-75","75-80","80+") ~ asymp_prob_elder_samples,
      TRUE ~ NA_real_  # Safety catch for unexpected age_group
    )) %>%
  mutate(wtp = case_when(
    income_group == "HIC" ~ wtp_hic_samples,
    income_group == "UMIC" ~ wtp_umic_samples,
    income_group == "LMIC" ~ wtp_lmic_samples,
    income_group == "LIC" ~ wtp_lic_samples)) %>%
  mutate(wtp_threshold = wtp*gdppc) %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ ((averted * (1 - prob_asymp)) * QALY_infection_samples) * wtp_threshold,
    name == "hospitalisations" ~ ((icu_averted * QALY_icu_samples) +
      (nonicu_averted * QALY_hospitalisations_samples)) * wtp_threshold,
    name == "deaths" ~ lg_averted
    * wtp_threshold)) %>%
  group_by(replicate) %>%
  summarise(undiscmonqalys_averted_sum = sum(undiscmonqalys_averted, na.rm=TRUE)) %>%
  ungroup()

discmonqaly_replicate_summary <- qaly %>%
  left_join(sens_df, by = "replicate") %>%
  mutate(
    # Map each age_group to the correct asymptomatic probability sample
    prob_asymp = case_when(
      age_group %in% c("0-5","5-10","10-15","15-20") ~ asymp_prob_young_samples,
      age_group %in% c("20-25","25-30","30-35","35-40","40-45","45-50","55-60") ~ asymp_prob_adult_samples,
      age_group %in% c("60-65","65-70","70-75","75-80","80+") ~ asymp_prob_elder_samples,
      TRUE ~ NA_real_  # Safety catch for unexpected age_group
    )) %>%
  mutate(wtp = case_when(
    income_group == "HIC" ~ wtp_hic_samples,
    income_group == "UMIC" ~ wtp_umic_samples,
    income_group == "LMIC" ~ wtp_lmic_samples,
    income_group == "LIC" ~ wtp_lic_samples)) %>%
  mutate(wtp_threshold = wtp*gdppc) %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ ((averted * (1 - prob_asymp)) * QALY_infection_samples) * wtp_threshold,
    name == "hospitalisations" ~ ((icu_averted * QALY_icu_samples) +
      (nonicu_averted * QALY_hospitalisations_samples)) * wtp_threshold,
    name == "deaths" ~ lghat_averted
    * wtp_threshold)) %>%
  group_by(replicate) %>%
  summarise(discmonqalys_averted_sum = sum(discmonqalys_averted, na.rm=TRUE)) %>%
  ungroup()

friction_costs_replicate_summary <- friction_costs %>%
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
  ungroup()

epi_psa_summary <- epi_psa %>%
  group_by(replicate) %>%
  summarise(
    deaths = sum(deaths, na.rm = TRUE),
    hospitalisations = sum(hospitalisations, na.rm = TRUE),
    infections = sum(infections, na.rm = TRUE),
    ihr = mean(ihr, na.rm = TRUE),
    ifr = mean(ifr, na.rm = TRUE),
    vaccine_efficacy = mean(vaccine_efficacy, na.rm = TRUE)
  ) %>%
  ungroup()

dim(epi_psa_summary)

# Step 2: Merge with `sens_df` to get input parameters for PRCC -- NEW: for VSL, not VSLY
psa_data <- vsl_replicate_summary %>%
  left_join(sens_df, by = "replicate") %>%
  left_join(undiscmonqaly_replicate_summary, by = "replicate") %>%
  left_join(discmonqaly_replicate_summary, by = "replicate") %>%
  left_join(friction_costs_replicate_summary, by = "replicate") %>%
  left_join(epi_psa_summary, by = "replicate")

# Select inputs (parameter samples) and output (vsl)
inputs <- psa_data %>%
  select(vsl_samples,
         deaths, vaccine_efficacy, infections, hospitalisations)
output <- psa_data$vsl_averted  # Target variable

# Compute PRCC using epi.prcc()
prcc_results <- epi.prcc(dat = cbind(inputs, output), sided.test = 2)

# Convert PRCC results into a data frame
prcc_df <- data.frame(Parameter = names(inputs), PRCC = prcc_results$est, P_Value = prcc_results$p.value)

# Print results
print(prcc_df)

# Step 3: Order by absolute PRCC value for better visualisation

# Sort the data ready for visualisation
prcc_df <- prcc_df %>%
  mutate(
    Parameter = reorder(Parameter, abs(PRCC)),
    P_Label = ifelse(P_Value < 0.001,
                     "p < 0.001",
                     paste0("p = ", signif(P_Value, 3)))
  )

# Make labels for the plots
labels <- c(
  "QALY_hospitalisations_samples" = "QALY losses (hospitalisation)",
  "QALY_icu_samples" = "QALY losses (ICU)",
  "frictionperiod_samples" = "Friction period (HICs)",
  "wtp_lic_samples" = "% GDP (LICs)",
  "wtp_umic_samples" = "% GDP (UMICs)",
  "wtp_lmic_samples" = "% GDP (LMICs)",
  "wtp_hic_samples" = "% GDP (HICs)",
  "vsl_samples" = "USA VSL value",
  "QALY_infection_samples" = "QALY losses (symptomatic infection)",
  "deaths" = "Averted deaths",
  "asymp_prob_young_samples" = "Probability of asymptomatic infection (0-20y)",
  "asymp_prob_adult_samples" = "Probability of asymptomatic infection (20-60y)",
  "asymp_prob_elder_samples" = "Probability of asymptomatic infection (60+y)",
  "hospitalisations" = "Averted hospitalisations",
  "infections" = "Averted infections",
  "vaccine_efficacy" = "Model fit vaccine effectiveness"
)

# Create the tornado plot
prcc_gg_vsl <- ggplot(prcc_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_bar(stat = "identity", width = 0.7) +
  # Add asterisks for significant P values
  geom_text(
    aes(label = P_Label,
        y = PRCC + 0.02 * sign(PRCC)),
    vjust = 0.5,
    hjust = ifelse(prcc_df$PRCC < 0, 0.7, 0.2),
    size = 3
  ) +
  coord_flip() +  # Flip for a horizontal tornado plot
  scale_fill_manual(values = c("#dd5129", "#0f7ba2"), labels = c("Negative", "Positive")) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  scale_x_discrete(labels = labels) +
  labs(title = "VSLs",
       x = "Parameter",
       y = "Partial Rank Correlation Coefficient (PRCC)") +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = "white"))
save_figs(fig = prcc_gg_vsl, name = "prcc_tornado_plot_vsl", width = 10, height = 4)

print(prcc_gg_vsl)
save_figs(fig = prcc_gg_vsl, name = "prcc_tornado_plot_vsl", width = 10, height = 4)

## UNDISCOUNTED MONETIZED QALYS ##
inputs <- psa_data %>%
  select(wtp_hic_samples, wtp_umic_samples, wtp_lmic_samples,
         wtp_lic_samples, QALY_infection_samples, QALY_hospitalisations_samples, QALY_icu_samples,
         asymp_prob_young_samples, asymp_prob_adult_samples, asymp_prob_elder_samples,
         deaths, vaccine_efficacy, infections, hospitalisations)
output <- psa_data$undiscmonqalys_averted_sum

# Compute PRCC using epi.prcc()
prcc_results <- epi.prcc(dat = cbind(inputs, output), sided.test = 2)

# Convert PRCC results into a data frame
prcc_df <- data.frame(Parameter = names(inputs), PRCC = prcc_results$est, P_Value = prcc_results$p.value)

# Print results
print(prcc_df)

prcc_df <- prcc_df %>%
  mutate(
    Parameter = reorder(Parameter, abs(PRCC)),
    P_Label = ifelse(P_Value < 0.001,
                     "p < 0.001",
                     paste0("p = ", signif(P_Value, 3)))
  )

# Create the tornado plot
prcc_gg_undiscmonqalys <- ggplot(prcc_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_bar(stat = "identity", width = 0.7) +
  # Add asterisks for significant P values
  geom_text(
    aes(label = P_Label,
        y = PRCC + 0.02 * sign(PRCC)),
    vjust = 0.5,
    hjust = ifelse(prcc_df$PRCC < 0, 0.7, 0.2),
    size = 3
  ) +
  coord_flip() +  # Flip for a horizontal tornado plot
  scale_fill_manual(values = c("#dd5129", "#0f7ba2"), labels = c("Negative", "Positive")) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  scale_x_discrete(labels = labels) +
  labs(title = "Undiscounted value of health gains (in QALYS)",
       x = "Parameter",
       y = "Partial Rank Correlation Coefficient (PRCC)") +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = "white"))

print(prcc_gg_undiscmonqalys)
save_figs(fig = prcc_gg_undiscmonqalys, name = "prcc_tornado_plot_undiscmonqalys", width = 8, height = 8)

# DISCOUNTED MONETIZED QALYS #
inputs <- psa_data %>%
  select(wtp_hic_samples, wtp_umic_samples, wtp_lmic_samples,
         wtp_lic_samples, QALY_infection_samples, QALY_hospitalisations_samples, QALY_icu_samples,
         asymp_prob_young_samples, asymp_prob_adult_samples, asymp_prob_elder_samples,
         deaths, vaccine_efficacy, infections, hospitalisations)
output <- psa_data$discmonqalys_averted_sum

# Compute PRCC using epi.prcc()
prcc_results <- epi.prcc(dat = cbind(inputs, output), sided.test = 2)

# Convert PRCC results into a data frame
prcc_df <- data.frame(Parameter = names(inputs), PRCC = prcc_results$est, P_Value = prcc_results$p.value)

# Print results
print(prcc_df)

prcc_df <- prcc_df %>%
  mutate(
    Parameter = reorder(Parameter, abs(PRCC)),
    P_Label = ifelse(P_Value < 0.001,
                     "p < 0.001",
                     paste0("p = ", signif(P_Value, 3)))
  )

# Create the tornado plot
prcc_gg_discmonqalys <- ggplot(prcc_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_bar(stat = "identity", width = 0.7) +
  # < 0.001 for p-values
  geom_text(
    aes(label = P_Label,
        y = PRCC + 0.02 * sign(PRCC)),
    vjust = 0.5,
    hjust = ifelse(prcc_df$PRCC < 0, 0.7, 0.2),
    size = 3
  ) +
  coord_flip() +  # Flip for a horizontal tornado plot
  scale_fill_manual(values = c("#dd5129", "#0f7ba2"), labels = c("Negative", "Positive")) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  scale_x_discrete(labels = labels) +
  labs(title = "Discounted value of health gains (in QALYs)",
       x = "Parameter",
       y = "Partial Rank Correlation Coefficient (PRCC)") +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = "white"))

print(prcc_gg_discmonqalys)
save_figs(fig = prcc_gg_discmonqalys, name = "prcc_tornado_plot_discmonqalys", width = 8, height = 8)

# FRICTION COSTS #
inputs <- psa_data %>%
  select(frictionperiod_samples,
         deaths, vaccine_efficacy, infections, hospitalisations)
output <- psa_data$friction_costs

# Compute PRCC using epi.prcc()
prcc_results <- epi.prcc(dat = cbind(inputs, output), sided.test = 2)

# Convert PRCC results into a data frame
prcc_df <- data.frame(Parameter = names(inputs), PRCC = prcc_results$est, P_Value = prcc_results$p.value)

# Print results
print(prcc_df)

prcc_df <- prcc_df %>%
  mutate(
    Parameter = reorder(Parameter, abs(PRCC)),
    P_Label = ifelse(P_Value < 0.001,
                     "p < 0.001",
                     paste0("p = ", signif(P_Value, 3)))
  )

# Create the tornado plot
prcc_gg_frictioncosts <- ggplot(prcc_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(
    aes(label = P_Label,
        y = PRCC + 0.02 * sign(PRCC)),
    vjust = 0.5,
    hjust = ifelse(prcc_df$PRCC < 0, 0.7, 0.2),
    size = 3
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("#dd5129", "#0f7ba2")) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  scale_x_discrete(labels = labels) +
  labs(title = "Friction Costs",
       x = "Parameter",
       y = "Partial Rank Correlation Coefficient (PRCC)") +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white"))

print(prcc_gg_frictioncosts)
save_figs(fig = prcc_gg_frictioncosts, name = "prcc_tornado_plot_frictioncosts", width = 8, height = 6)


##### OLD ## DO NOT USE
# Add titles to each individual plot
#old
prcc_gg_vsly <- prcc_gg_vsly + labs(title = "Discounted VSLYs")
prcc_gg_unvsly <- prcc_gg_unvsly + labs(title = "Undiscounted VSLYs")

#new
prcc_gg_vsl <- prcc_gg_vsl + labs(title = "VSLs")
prcc_gg_undiscmonqalys <- prcc_gg_undiscmonqalys + labs(title = "Undiscounted Monetized QALYs")
prcc_gg_discmonqalys <- prcc_gg_discmonqalys + labs(title = "Discounted Monetized QALYs Averted")
prcc_gg_frictioncosts <- prcc_gg_frictioncosts + labs(title = "Friction Costs")

# Combine the plots with titles
#NEW: no VSLYs, VSL instead
prcc_combined_plot <- wrap_plots(
  prcc_gg_vsl / prcc_gg_undiscmonqalys /
    prcc_gg_discmonqalys / prcc_gg_frictioncosts,
  ncol = 1,
  heights = c(1, 1, 0.5)  # Adjust height of the last row
) +
  plot_annotation(tag_levels = 'A')

# Display the combined plot
prcc_combined_plot
save_figs(fig = prcc_combined_plot, name = "prcc_combined_plot", width = 5, height = 28)

# Combine the plots with titles - discounted

prcc_discounted_plot <- wrap_plots(
  prcc_gg_vsl /
    prcc_gg_discmonqalys / prcc_gg_frictioncosts,
  ncol = 1,
  heights = c(1, 1, 0.5)  # Adjust height of the last row
) +
  plot_annotation(tag_levels = 'A')

# Display the combined plot
prcc_discounted_plot
save_figs(fig = prcc_discounted_plot, name = "prcc_discounted_plot", width = 5, height = 15)

# Combine the plots with titles - Undiscounted

prcc_undiscounted_plot <- wrap_plots(prcc_gg_vsl / prcc_gg_undiscmonqalys,
                                     ncol = 1,
                                     heights = c(1, 1, 0.5)  # Adjust height of the last row
) +
  plot_annotation(tag_levels = 'A')

# Display the combined plot
prcc_undiscounted_plot
save_figs(fig = prcc_undiscounted_plot, name = "prcc_undiscounted_plot", width = 5, height = 15)









######################################### OLD CODE ################################

# Define input parameters
inputs <- psa_data %>%
  select(vsl_samples, wtp_hic_samples, wtp_umic_samples, wtp_lmic_samples,
         wtp_lic_samples, QALY_infection_samples, QALY_hospitalisations_samples,
         QALY_deaths_samples, frictionperiod_samples)

# Create a list of outputs with labels
outputs <- list(
  vsly_disc_averted = "Discounted VSLYs",
  vsly_undisc_averted = "Undiscounted VSLYs",
  undiscmonqalys_averted_sum = "Undiscounted Monetized QALYs",
  discmonqalys_averted_sum = "Discounted Monetized QALYs",
  friction_costs = "Friction Costs"
)

# Initialize an empty dataframe to store all PRCC results
prcc_combined_df <- data.frame()

# Compute PRCC for each output and combine results
for (outcome in names(outputs)) {
  output <- psa_data[[outcome]]

  # Compute PRCC using epi.prcc()
  prcc_results <- epi.prcc(dat = cbind(inputs, output), sided.test = 2)

  # Convert PRCC results into a data frame
  prcc_df <- data.frame(
    Parameter = names(inputs),
    PRCC = prcc_results$est,
    P_Value = prcc_results$p.value,
    Outcome = outputs[[outcome]]  # Label for subcategory
  )

  # Append to combined dataframe
  prcc_combined_df <- rbind(prcc_combined_df, prcc_df)
}

# Define the custom order for the outcomes
output_order <- c(
  "Discounted Monetized QALYs",
  "Undiscounted Monetized QALYs",
  "Discounted VSLYs",
  "Undiscounted VSLYs",
  "Friction Costs"
)

# Reorder the Outcome factor in the prcc_combined_df data frame
prcc_combined_df <- prcc_combined_df %>%
  mutate(Outcome = factor(Outcome, levels = output_order))

labels <- c(
  "QALY_hospitalisations_samples" = "QALY loss per hospitalisations",
  "frictionperiod_samples" = "Friction period (HICs)",
  "wtp_lic_samples" = "% GDP for WTP threshold (LICs)",
  "wtp_umic_samples" = "% GDP for WTP threshold (UMICs)",
  "wtp_lmic_samples" = "% GDP for WTP threshold (LMICs)",
  "wtp_hic_samples" = "% GDP for WTP threshold (HICs)",
  "vsl_samples" = "USA VSL value",
  "QALY_deaths_samples" = "QALY loss per death",
  "QALY_infection_samples" = "QALY loss per infection"

)

prcc_palette <- c("#2f70a1", "#72aeb6")

# Create a single tornado plot with subcategories
prcc_combined_plot <- ggplot(prcc_combined_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  facet_wrap(~Outcome, ncol = 1, scales = "free_y") +  # Subcategories in separate rows
  scale_x_discrete(labels = labels) +
  scale_fill_manual(values = prcc_palette, labels = c("Negative", "Positive")) +  # Fix here
  labs(
    title = "Tornado Plot of PRCC Values by Outcome",
    x = "Parameter",
    y = "Partial Rank Correlation Coefficient (PRCC)"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = "white"),
        axis.title.y = element_blank()
  )


# Display and save the combined plot
print(prcc_combined_plot)
save_figs(fig = prcc_combined_plot, name = "prcc_combined_plot", width = 8, height = 9)



# TODO: Hallie - if you update the below remembering to recalculate these using your actual sampled quantities
# FYI - In the above I only included deaths, vaccine_efficacy and ifr as these would be

####################################
inputs <- psa_data %>%
  select(vsl_samples,
         deaths, vaccine_efficacy, infections, hospitalisations)
output <- psa_data$vsly_undisc_averted  # Target variable

# Compute PRCC using epi.prcc()
prcc_results <- epi.prcc(dat = cbind(inputs, output), sided.test = 2)

# Convert PRCC results into a data frame
prcc_df <- data.frame(Parameter = names(inputs), PRCC = prcc_results$est, P_Value = prcc_results$p.value)

# Print results
print(prcc_df)


prcc_df <- prcc_df %>%
  mutate(Parameter = reorder(Parameter, abs(PRCC)))


# Create the tornado plot
prcc_gg_unvsly <- ggplot(prcc_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_bar(stat = "identity", width = 0.7) +
  # Add asterisks for significant P values
  geom_text(data = prcc_df,
            aes(label = paste("p =",signif(P_Value, 3)), x = Parameter, y = PRCC + 0.02 * sign(PRCC)),  # Adjust for direction
            vjust = 0.5,
            hjust = ifelse(prcc_df$PRCC<0, 0.7, 0.2),     # Nudges asterisk left/right
            size = 3) +
  coord_flip() +  # Flip for a horizontal tornado plot
  scale_fill_manual(values = c("#dd5129", "#0f7ba2"), labels = c("Negative", "Positive")) +
  scale_y_continuous(limits = range(prcc_df$PRCC)*1.6) +
  scale_x_discrete(labels = labels) +
  labs(title = "Tornado Plot of PRCC Values",
       x = "Parameter",
       y = "Partial Rank Correlation Coefficient (PRCC)") +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = "white"))

print(prcc_gg_unvsly)
save_figs(fig = prcc_gg_unvsly, name = "prcc_tornado_plot_unvsly", width = 10, height = 4)


# Check each summary object individually
dim(vsl_replicate_summary)
dim(undiscmonqaly_replicate_summary)
dim(discmonqaly_replicate_summary)
dim(friction_costs_replicate_summary)
dim(sens_df)
dim(epi_psa)

# Check for duplicate replicates in each
vsl_replicate_summary %>% count(replicate) %>% filter(n > 1)
undiscmonqaly_replicate_summary %>% count(replicate) %>% filter(n > 1)
discmonqaly_replicate_summary %>% count(replicate) %>% filter(n > 1)
friction_costs_replicate_summary %>% count(replicate) %>% filter(n > 1)
sens_df %>% count(replicate) %>% filter(n > 1)
epi_psa %>% count(replicate) %>% filter(n > 1)
