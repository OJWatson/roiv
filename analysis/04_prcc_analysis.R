# Step 0: load packages and files ------

# Load required packages
install.packages("epiR")
library(epiR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

# Read in and format data
sens_df <- readRDS("analysis/data/derived/psa_sens_df.rds")
vsly <- readRDS("analysis/data/derived/vsly.rds")
vsly_psa <- vsly %>%
  left_join(sens_df, by = "replicate") %>%  # Join based on replicate
  mutate(vsl_usa = vsl_samples)

# Step 1: Summarise `vsly_psa` to get total global VSLY per replicate  ------
vsly_replicate_summary <- vsly_psa %>%
  group_by(replicate) %>%
  summarise(vsly_undisc_averted = sum(lg_averted * vly, na.rm = TRUE),
            vsly_disc_averted = sum(lghat_averted * vly_disc, na.rm = TRUE)) %>%
  ungroup()  # Remove replicate grouping to keep only one row per replicate


# Step 2: Merge with `sens_df` to get input parameters for PRCC ------
psa_data <- vsly_replicate_summary %>%
  left_join(sens_df, by = "replicate")

# Confirm the structure
str(psa_data)

# Select inputs (parameter samples) and output (vsly_disc_averted)
inputs <- psa_data %>%
  select(vsl_samples, wtp_hic_samples, wtp_umic_samples, wtp_lmic_samples,
         wtp_lic_samples, QALY_infection_samples, QALY_hospitalisations_samples,
         QALY_deaths_samples, frictionperiod_samples)
output <- psa_data$vsly_disc_averted  # Target variable

# Compute PRCC using epi.prcc()
prcc_results <- epi.prcc(dat = cbind(inputs, output), sided.test = 2)

# Convert PRCC results into a data frame
prcc_df <- data.frame(Parameter = names(inputs), PRCC = prcc_results$est, P_Value = prcc_results$p.value)

# Print results
print(prcc_df)

# Step 3: Order by absolute PRCC value for better visualisation

# Sort the data ready for visualisation
prcc_df <- prcc_df %>%
  mutate(Parameter = reorder(Parameter, abs(PRCC)))

# Make labels for the plots
labels <- c(
  "QALY_hospitalisations_samples" = "QALY losses (hospitalisations)",
  "frictionperiod_samples" = "Friction period (HICs)",
  "wtp_lic_samples" = "% GDP (LICs)",
  "wtp_umic_samples" = "% GDP (UMICs)",
  "wtp_lmic_samples" = "% GDP (LMICs)",
  "wtp_hic_samples" = "% GDP (HICs)",
  "vsl_samples" = "USA VSL value",
  "QALY_deaths_samples" = "QALY losses (deaths)",
  "QALY_infection_samples" = "QALY losses (infections)"

)

# Create the tornado plot
prcc_gg_vsly <- ggplot(prcc_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +  # Flip for a horizontal tornado plot
  scale_fill_manual(values = c("#dd5129", "#0f7ba2"), labels = c("Negative", "Positive")) +
  scale_x_discrete(labels = labels) +
  labs(title = "Tornado Plot of PRCC Values",
       x = "Parameter",
       y = "Partial Rank Correlation Coefficient (PRCC)") +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = "white"))
save_figs(fig = prcc_gg_vsly, name = "prcc_tornado_plot_vsly", width = 8, height = 6)

print(prcc_gg_vsly)
####################################

# Read in and format data
sens_df <- readRDS("analysis/data/derived/psa_sens_df.rds")
vsly <- readRDS("analysis/data/derived/vsly.rds")

# Merge PSA data with VSLY data
vsly_psa <- vsly %>%
  left_join(sens_df, by = "replicate") %>%
  mutate(vsl_usa = vsl_samples)


# Step 1: Summarise to get total global VSLY per replicate
vsly_replicate_summary <- vsly_psa %>%
  group_by(replicate) %>%
  summarise(vsly_undisc_averted = sum(lg_averted * vly, na.rm = TRUE),
            vsly_disc_averted = sum(lghat_averted * vly_disc, na.rm = TRUE)) %>%
  ungroup()  # Ensure one row per replicate

undiscmonqaly_replicate_summary <- qaly %>%
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
  ungroup()

discmonqaly_replicate_summary <- qaly %>%
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
  ungroup()

friction_costs_replicate_summary <- friction_costs %>%
  left_join(sens_df, by = "replicate") %>%
  mutate(friction_period = case_when(
    income_group == "HIC" ~ frictionperiod_samples,
    income_group %in% c("UMIC", "LMIC", "LIC") ~ (3*30.417)
  )) %>%
  group_by(replicate) %>%
  summarise(friction_costs = sum(friction_costs, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Merge with `sens_df` to get input parameters for PRCC
psa_data <- vsly_replicate_summary %>%
  left_join(sens_df, by = "replicate") %>%
  left_join(undiscmonqaly_replicate_summary, by = "replicate") %>%
  left_join(discmonqaly_replicate_summary, by = "replicate") %>%
  left_join(friction_costs_replicate_summary, by = "replicate")


# Confirm structure
str(psa_data)

# Define input parameters
inputs <- psa_data %>%
  select(vsl_samples, wtp_hic_samples, wtp_umic_samples, wtp_lmic_samples,
         wtp_lic_samples, QALY_infection_samples, QALY_hospitalisations_samples,
         QALY_deaths_samples, frictionperiod_samples)


########

output <- psa_data$vsly_undisc_averted  # Target variable

# Compute PRCC using epi.prcc()
prcc_unvsly_results <- epi.prcc(dat = cbind(inputs, output), sided.test = 2)

# Convert PRCC results into a data frame
prcc_unvsly_df <- data.frame(Parameter = names(inputs), PRCC = prcc_unvsly_results$est, P_Value = prcc_unvsly_results$p.value)

# Print results
print(prcc_unvsly_df)

# Step 3: Order by absolute PRCC value for better visualisation

# Sort the data ready for visualisation
prcc_unvsly_df <- prcc_unvsly_df %>%
  mutate(Parameter = reorder(Parameter, abs(PRCC)))

# Create the tornado plot
prcc_gg_unvsly <- ggplot(prcc_unvsly_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +  # Flip for a horizontal tornado plot
  scale_fill_manual(values = c("#dd5129", "#0f7ba2"), labels = c("Negative", "Positive")) +
  scale_x_discrete(labels = labels) +
  labs(title = "Tornado Plot of PRCC Values",
       x = "Parameter",
       y = "Partial Rank Correlation Coefficient (PRCC)") +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = "white"))

print(prcc_gg_unvsly)
save_figs(fig = prcc_gg_unvsly, name = "unvsly_prcc_tornado_plot", width = 8, height = 6)

######

output <- psa_data$undiscmonqalys_averted_sum

# Compute PRCC using epi.prcc()
prcc_unmonqalys_results <- epi.prcc(dat = cbind(inputs, output), sided.test = 2)

# Convert PRCC results into a data frame
prcc_unmonqalys_df <- data.frame(Parameter = names(inputs), PRCC = prcc_unmonqalys_results$est, P_Value = prcc_unmonqalys_results$p.value)

# Print results
print(prcc_unmonqalys_df)

# Step 3: Order by absolute PRCC value for better visualisation

# Sort the data ready for visualisation
prcc_unmonqalys_df <- prcc_unmonqalys_df %>%
  mutate(Parameter = reorder(Parameter, abs(PRCC)))

# Create the tornado plot
prcc_gg_unmonqalys <- ggplot(prcc_unmonqalys_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +  # Flip for a horizontal tornado plot
  scale_fill_manual(values = c("#dd5129", "#0f7ba2"), labels = c("Negative", "Positive")) +
  scale_x_discrete(labels = labels) +
  labs(title = "Tornado Plot of PRCC Values",
       x = "Parameter",
       y = "Partial Rank Correlation Coefficient (PRCC)") +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = "white"))

print(prcc_gg_unmonqalys)
save_figs(fig = prcc_gg_unmonqalys, name = "unmonqalys_prcc_tornado_plot", width = 8, height = 6)

#######

output <- psa_data$discmonqalys_averted_sum

# Compute PRCC using epi.prcc()
prcc_monqalys_results <- epi.prcc(dat = cbind(inputs, output), sided.test = 2)

# Convert PRCC results into a data frame
prcc_monqalys_df <- data.frame(Parameter = names(inputs), PRCC = prcc_monqalys_results$est, P_Value = prcc_monqalys_results$p.value)

# Print results
print(prcc_monqalys_df)

# Step 3: Order by absolute PRCC value for better visualisation

# Sort the data ready for visualisation
prcc_monqalys_df <- prcc_monqalys_df %>%
  mutate(Parameter = reorder(Parameter, abs(PRCC)))

# Create the tornado plot
prcc_gg_monqalys <- ggplot(prcc_monqalys_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +  # Flip for a horizontal tornado plot
  scale_fill_manual(values = c("#dd5129", "#0f7ba2"), labels = c("Negative", "Positive")) +
  scale_x_discrete(labels = labels) +
  labs(title = "Tornado Plot of PRCC Values",
       x = "Parameter",
       y = "Partial Rank Correlation Coefficient (PRCC)") +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = "white"))

print(prcc_gg_monqalys)
save_figs(fig = prcc_gg_monqalys, name = "monqalys_prcc_tornado_plot", width = 8, height = 6)

#######


output <- psa_data$friction_costs

# Compute PRCC using epi.prcc()
prcc_friction_results <- epi.prcc(dat = cbind(inputs, output), sided.test = 2)

# Convert PRCC results into a data frame
prcc_friction_df <- data.frame(Parameter = names(inputs), PRCC = prcc_friction_results$est, P_Value = prcc_friction_results$p.value)

# Print results
print(prcc_friction_df)

# Step 3: Order by absolute PRCC value for better visualisation

# Sort the data ready for visualisation
prcc_friction_df <- prcc_friction_df %>%
  mutate(Parameter = reorder(Parameter, abs(PRCC)))

# Create the tornado plot
prcc_gg_friction <- ggplot(prcc_friction_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +  # Flip for a horizontal tornado plot
  scale_fill_manual(values = c("#dd5129", "#0f7ba2"), labels = c("Negative", "Positive")) +
  scale_x_discrete(labels = labels) +
  labs(title = "Tornado Plot of PRCC Values",
       x = "Parameter",
       y = "Partial Rank Correlation Coefficient (PRCC)") +
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = "white"))

print(prcc_gg_friction)
save_figs(fig = prcc_gg_friction, name = "friction_prcc_tornado_plot", width = 8, height = 6)


# Add titles to each individual plot
prcc_gg_vsly <- prcc_gg_vsly + labs(title = "Discounted VSLYs")
prcc_gg_unvsly <- prcc_gg_unvsly + labs(title = "Undiscounted VSLYs")
prcc_gg_unmonqalys <- prcc_gg_unmonqalys + labs(title = "Undiscounted Monetized QALYs")
prcc_gg_monqalys <- prcc_gg_monqalys + labs(title = "Discounted Monetized QALYs Averted")
prcc_gg_friction <- prcc_gg_friction + labs(title = "Friction Costs")

# Combine the plots with titles

prcc_combined_plot <- wrap_plots(
  prcc_gg_vsly / prcc_gg_unvsly / prcc_gg_unmonqalys / prcc_gg_monqalys / prcc_gg_friction,
  ncol = 1,
  heights = c(1, 1, 0.5)  # Adjust height of the last row
) +
  plot_annotation(tag_levels = 'A')

# Display the combined plot
prcc_combined_plot
save_figs(fig = prcc_combined_plot, name = "prcc_combined_plot", width = 5, height = 20)

