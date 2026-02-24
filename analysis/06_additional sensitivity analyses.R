#### VARYING INCOME ELASTICITY FOR LICs and LMICS ###

vsl_sa <- res_full %>%
  filter(name == "deaths") %>%
  group_by(iso3c, replicate) %>%
  mutate(
    income_elasticity = case_when(
      income_group %in% c("LIC", "LMIC") ~ 1.5,
      income_group %in% c("UMIC", "HIC") ~ 1.0,
      TRUE ~ 1.0
    ),
    vsl = mean_vsl_usa * (gnipc / gnipc_usa)^income_elasticity
  )

saveRDS(vsl_sa, "analysis/data/derived/vsl_sa.rds")

# getting total monetary value of vsl per income group (population-weighted)
vsl_sa_avertedtotal_income <- vsl_sa %>%
  group_by(income_group, replicate) %>%
  summarise(vsl_averted = sum((vsl*averted), na.rm = TRUE))%>%
  group_by(income_group) %>%
  summarise(
    across(vsl_averted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
vsl_sa_avertedtotal_income
write.csv(vsl_sa_avertedtotal_income, "analysis/tables/vsl_sa_avertedtotal_income.csv")

# vsl total worldwide
vsl_sa_avertedtotal <- vsl_sa %>%
  group_by(replicate) %>%
  summarise(vsl_averted = sum((vsl*averted), na.rm = TRUE))%>%
  summarise(
    across(vsl_averted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
vsl_sa_avertedtotal
write.csv(vsl_sa_avertedtotal, "analysis/tables/vsl_sa_avertedtotal.csv")

# get the sum of benefits for roi
new_welfarist_sum_sa <- bind_cols(vsl_sa_avertedtotal, sum_friction, sum_hc_costs) %>%
  summarise(
    total_low = vsl_averted_low + friction_costs_total_low + hc_costs_total_low,
    total_med = vsl_averted_med + friction_costs_total_med + hc_costs_total_med,
    total_high = vsl_averted_high + friction_costs_total_high + hc_costs_total_high
  )

# save results
new_welfarist_sum_sa
write.csv(new_welfarist_sum_sa, "analysis/tables/new_welfarist_sum_sa.csv")

new_roi_welfarist_sa <- new_welfarist_sum_sa %>%
  mutate(roi_low = ((total_low - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_med = ((total_med - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_high = ((total_high - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
new_roi_welfarist_sa
write.csv(new_roi_welfarist_sa, "analysis/tables/new_roi_welfarist_sa.csv")

##### welfarist with using 1 VSLY value for the world - using the mean calculated VSLY

vsly <- readRDS("analysis/data/derived/vsly.rds")

global_vsly <- vsly %>%
  group_by(replicate, iso3c) %>%
  summarise(
    vly_country = mean(vly, na.rm = TRUE),
    pop_country = sum(Ng, na.rm = TRUE)
  )

global_vsly_value <- global_vsly %>%
  group_by(replicate) %>%
  summarise(
    global_vly = weighted.mean(vly_country, pop_country, na.rm = TRUE)) %>%
  summarise(
    across(global_vly,
           list(low = lf,
                med = mf,
                high = hf)))

single_vsly_avertedtotals <- vsly %>%
  group_by(replicate) %>%
  summarise(vsly_undisc_averted = sum((lg_averted*40946.05), na.rm = TRUE)) %>%
  summarise(
    across(vsly_undisc_averted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
single_vsly_avertedtotals
write.csv(single_vsly_avertedtotals, "analysis/tables/single_vsly_avertedtotals.csv")

roi_single_vsly <- single_vsly_avertedtotals %>%
  mutate(roi_low = ((vsly_undisc_averted_low - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_med = ((vsly_undisc_averted_med - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu)),
         roi_high = ((vsly_undisc_averted_high - (dev_funding + del_cost + apa + corporate + manu))/(dev_funding + del_cost + apa + corporate + manu))) %>%
  select(roi_low, roi_med, roi_high)

# save results
roi_single_vsly
write.csv(roi_single_vsly, "analysis/tables/roi_single_vsly.csv")


# getting total monetary value of vsly per income group (population-weighted) to report in supplementary table
single_vsly_avertedincome <- vsly %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_undisc_averted = sum((lg_averted*40946.05), na.rm = TRUE)) %>%
  summarise(
    across(vsly_undisc_averted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
single_vsly_avertedincome
write.csv(single_vsly_avertedincome, "analysis/tables/single_vsly_avertedincome.csv")

# need to get values for the supplementary table
# percentage of GDP per income group
single_vsly_pgdp_income <- vsly %>%
  mutate(vsly = (lg_averted * 40946.05)) %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_total = sum(vsly, na.rm = TRUE)) %>%
  left_join(gdp %>% group_by(income_group) %>% summarise(gdp = sum(gdp, na.rm = TRUE))) %>% #
  mutate(vsly_pgdp = (vsly_total/gdp) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(vsly_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
single_vsly_pgdp_income
write.csv(single_vsly_pgdp_income, "analysis/tables/single_vsly_pgdp_income.csv")

# per person vaccinated, income groups
single_vsly_pp_income <- vsly %>%
  group_by(income_group, replicate) %>% # (step 1)
  summarise(vsly_total = sum(lg_averted*40946.05,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(vsly_pp = vsly_total/vaccines) %>%  # (step 3)
  group_by(income_group) %>%
  summarise(
    across(vsly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
single_vsly_pp_income
write.csv(single_vsly_pp_income, "analysis/tables/single_vsly_pp_income.csv")

# per person as a percentage of GDPpc
single_vsly_pp_gdppc_income <- vsly %>%
  group_by(income_group, replicate) %>%
  summarise(vsly_total = sum(lg_averted * 40946.05, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(income_group) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "income_group") %>%
  left_join(gdppc %>% group_by(income_group) %>%
              summarise(gdppc = sum(gdp, na.rm = TRUE)/sum(Ng, na.rm = TRUE)),
            by = "income_group") %>%
  mutate(vsly_pp_gdppc = ((vsly_total / vaccines) / gdppc) * 100) %>%
  group_by(income_group) %>%
  summarise(
    across(vsly_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
single_vsly_pp_gdppc_income
write.csv(single_vsly_pp_gdppc_income, "analysis/tables/single_vsly_pp_gdppc_income.csv")

# now get per person vaccinated as a % of gdp per capita
single_vsly_pp_gdppc_world <- vsly %>%
  group_by(replicate) %>%
  summarise(vsly_averted = sum((lg_averted*40946.05), na.rm = TRUE)) %>%
  mutate(vsly_pp_gdppc = ((vsly_averted / total_vaccines) / world_gdppc) * 100) %>%
  summarise(
    across(vsly_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
single_vsly_pp_gdppc_world
write.csv(single_vsly_pp_gdppc_world, "analysis/tables/single_vsly_pp_gdppc_world.csv")

# vsly pgdp world
single_vsly_pgdp_world <- vsly %>%
  group_by(replicate) %>%
  summarise(vsly_averted = sum((lg_averted*40946.05), na.rm = TRUE)) %>%
  mutate(vsly_pgdp = ((vsly_averted / total_gdp) * 100)) %>%
  summarise(
    across(vsly_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
single_vsly_pgdp_world
write.csv(single_vsly_pgdp_world, "analysis/tables/single_vsly_pgdp_world.csv")

# per person vaccinate
single_vsly_pp_world <- vsly %>%
  group_by(replicate) %>%
  summarise(vsly_averted = sum((lg_averted*40946.05), na.rm = TRUE)) %>%
  mutate(vsly_pp = vsly_averted / total_vaccines) %>%
  summarise(
    across(vsly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
single_vsly_pp_world
write.csv(single_vsly_pp_world, "analysis/tables/single_vsly_pp_world.csv")
