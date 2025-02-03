# Step 0: load packages and files ------

# Load required packages
install.packages("epiR")
library(epiR)
library(ggplot2)
library(dplyr)
library(tidyr)

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

# Create the tornado plot
prcc_gg <- ggplot(prcc_df, aes(x = Parameter, y = PRCC, fill = PRCC > 0)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +  # Flip for a horizontal tornado plot
  scale_fill_manual(values = c("#dd5129", "#0f7ba2"), labels = c("Negative", "Positive")) +
  labs(title = "Tornado Plot of PRCC Values",
       x = "Parameter",
       y = "Partial Rank Correlation Coefficient (PRCC)") +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = "white"))
save_figs(fig = prcc_gg, name = "prcc_tornado_plot", width = 8, height = 6)
