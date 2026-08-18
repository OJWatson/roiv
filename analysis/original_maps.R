## NOTE: RUN FILE "extra_iso3c.R" to create maps

hccosts_pp_gdppc_iso3c
undiscvsly_pp_gdppc_iso3c
discvsly_pp_gdppc_iso3c
undiscmonqaly_pp_gdppc_iso3c
discmonqaly_pp_gdppc_iso3c
friction_pp_gdppc_iso3c
vsl_pp_gdppc_iso3c

##############
# UNDISCOUNTED EXTRAWELFARIST: UNDISCOUNTED MONETIZED QALYS, HUMAN CAPITAL COSTS, FRICTION COSTS, HEALTHCARE COSTS
##############

undisc_extrawelfarist_sum_iso3c <- sum_undiscmonqaly_iso3c %>%
  left_join(friction_sum_iso3c, by = "iso3c") %>%
  left_join(hc_costs_total_iso3c, by = "iso3c") %>%
  group_by(iso3c) %>%  # Ensure grouping by iso3c
  reframe(
    undisc_exwelf_total_low = sum(undiscmonqalys_averted_sum_low, friction_total_low, health_costs_total_low, na.rm = TRUE),
    undisc_exwelf_total_med = sum(undiscmonqalys_averted_sum_med, friction_total_med, health_costs_total_med, na.rm = TRUE),
    undisc_exwelf_total_high = sum(undiscmonqalys_averted_sum_high, friction_total_high, health_costs_total_high, na.rm = TRUE)
  ) %>%
  filter(if_any(c(undisc_exwelf_total_low, undisc_exwelf_total_med, undisc_exwelf_total_high), ~ . != 0))

# our results table which we can then save in the tables directory
undisc_extrawelfarist_sum_iso3c
write.csv(undisc_extrawelfarist_sum_iso3c, "analysis/tables/undisc_extrawelfarist_sum_iso3c.csv")

# get in terms of per person vaccinated as percentage of gdppc

undisc_exwelfarist_pp_gdppc_iso3c <- undisc_extrawelfarist_sum_iso3c %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by (iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)))%>%
  mutate(undisc_exwelf_low = (((undisc_exwelf_total_low / vaccines) / gdppc) * 100),
         undisc_exwelf_med = (((undisc_exwelf_total_med / vaccines) / gdppc) * 100),
         undisc_exwelf_high = (((undisc_exwelf_total_low / vaccines) / gdppc) * 100)) %>%
  select(iso3c, undisc_exwelf_low, undisc_exwelf_med, undisc_exwelf_high)

# our results table which we can then save in the tables directory
undisc_exwelfarist_pp_gdppc_iso3c
write.csv(undisc_exwelfarist_pp_gdppc_iso3c, "analysis/tables/undisc_exwelfarist_pp_gdppc_iso3c.csv")

##############
# DISCOUNTED EXTRAWELFARIST: DISCOUNTED MONETIZED QALYS, HUMAN CAPITAL COSTS, FRICTION COSTS, HEALTHCARE COSTS
##############


disc_exwelfarist_sum_iso3c <- sum_discmonqaly_iso3c %>%
  left_join(friction_sum_iso3c, by = "iso3c") %>%
  left_join(hc_costs_total_iso3c, by = "iso3c") %>%
  group_by(iso3c) %>%  # Ensure grouping by iso3c
  reframe(
    disc_exwelf_total_low = sum(discmonqalys_averted_sum_low, friction_total_low, health_costs_total_low, na.rm = TRUE),
    disc_exwelf_total_med = sum(discmonqalys_averted_sum_med, friction_total_med, health_costs_total_med, na.rm = TRUE),
    disc_exwelf_total_high = sum(discmonqalys_averted_sum_high, friction_total_high, health_costs_total_high, na.rm = TRUE)
  ) %>%
  filter(if_any(c(disc_exwelf_total_low, disc_exwelf_total_med, disc_exwelf_total_high), ~ . != 0))

# our results table which we can then save in the tables directory
disc_exwelfarist_sum_iso3c
write.csv(disc_exwelfarist_sum_iso3c, "analysis/tables/disc_exwelfarist_sum_iso3c.csv")


# get in terms of per person vaccinated as a percentage of gdppc
disc_exwelfarist_pp_gdppc_iso3c <- disc_exwelfarist_sum_iso3c %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by (iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)))%>%
  mutate(disc_exwelf_low = (((disc_exwelf_total_low / vaccines) / gdppc) * 100),
         disc_exwelf_med = (((disc_exwelf_total_med / vaccines) / gdppc) * 100),
         disc_exwelf_high = (((disc_exwelf_total_low / vaccines) / gdppc) * 100)) %>%
  select(iso3c, disc_exwelf_low, disc_exwelf_med, disc_exwelf_high)

# our results table which we can then save in the tables directory
disc_exwelfarist_pp_gdppc_iso3c
write.csv(disc_exwelfarist_pp_gdppc_iso3c, "analysis/tables/disc_exwelfarist_pp_gdppc_iso3c.csv")

## NEW WELFARIST WITH VSL, PRODUCTIVITY AND HEALTHCARE

new_welfarist_sum_iso3c <- sum_vsl_iso3c %>%
  left_join(friction_sum_iso3c, by = "iso3c") %>%
  left_join(hc_costs_total_iso3c, by = "iso3c") %>%
  group_by(iso3c) %>%  # Ensure grouping by iso3c
  reframe(
    new_welf_total_low = sum(vsl_total_low, friction_total_low, health_costs_total_low, na.rm = TRUE),
    new_welf_total_med = sum(vsl_total_med, friction_total_med, health_costs_total_med, na.rm = TRUE),
    new_welf_total_high = sum(vsl_total_high, friction_total_high, health_costs_total_high, na.rm = TRUE)
  ) %>%
  filter(if_any(c(new_welf_total_low, new_welf_total_med, new_welf_total_high), ~ . != 0))

# our results table which we can then save in the tables directory
new_welfarist_sum_iso3c
write.csv(new_welfarist_sum_iso3c, "analysis/tables/new_welfarist_sum_iso3c.csv")


# get in terms of per person vaccinated as a percentage of gdppc
new_welfarist_pp_pgdp_iso3c <- new_welfarist_sum_iso3c %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by (iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)))%>%
  mutate(new_welf_low = (((new_welf_total_low / vaccines) / gdppc) * 100),
         new_welf_med = (((new_welf_total_med / vaccines) / gdppc) * 100),
         new_welf_high = (((new_welf_total_high / vaccines) / gdppc) * 100)) %>%
  select(iso3c, new_welf_low, new_welf_med, new_welf_high)

# our results table which we can then save in the tables directory
new_welfarist_pp_pgdp_iso3c
write.csv(new_welfarist_pp_pgdp_iso3c, "analysis/tables/new_welfarist_pp_pgdp_iso3c.csv")

##############
# UNDISCOUNTED WELFARIST
##############

undisc_welf_pp_pgdppc <- undiscvsly_pp_gdppc_iso3c

##############
# DISCOUNTED EXTRAWELFARIST
##############

disc_welf_pp_pgdppc <- discvsly_pp_gdppc_iso3c

