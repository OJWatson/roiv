
setwd(here::here())
vsl <- readRDS("analysis/data/derived/vsl.rds")
vsly <- readRDS("analysis/data/derived/vsly.rds")
qaly <- readRDS("analysis/data/derived/qaly.rds")
friction_costs <- readRDS("analysis/data/derived/friction_costs.rds")
hc_costs_grouped <- readRDS("analysis/data/derived/hc_costs.rds")
res_full <- readRDS("analysis/data/derived/res_full.rds")

### VSL PER ISO3C RESULTS ###

sum_vsl_iso3c <- vsl %>%
  group_by(iso3c, replicate) %>%
  summarise(vsl_total = sum(vsl*averted, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(vsl_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))
# our results table which we can then save in the tables directory
sum_vsl_iso3c
write.csv(sum_vsl_iso3c, "analysis/tables/sum_vsl_iso3c.csv")

vsl_pp_gdppc_iso3c <- vsl %>%
  group_by(iso3c, replicate) %>%
  summarise(vsl_total = sum(vsl*averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>%
              group_by(iso3c) %>%
              summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "iso3c") %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = sum(gdppc, na.rm = TRUE)), by = "iso3c") %>%
  mutate(vsl_pp_gdppc = ((vsl_total / vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(vsl_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
vsl_pp_gdppc_iso3c
write.csv(vsl_pp_gdppc_iso3c, "analysis/tables/vsl_pp_gdppc_iso3c.csv")

#### VSLY PER ISO3C RESULTS ####

# getting total monetary value of vsly per iso3c (population-weighted)
vsly_avertedtotals_iso3c <- vsly %>%
  group_by(iso3c, replicate) %>%
  summarise(vsly_undisc_averted = sum((lg_averted*vly), na.rm = TRUE),
            vsly_disc_averted = sum((lghat_averted*vly_disc), na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(vsly_undisc_averted:vsly_disc_averted,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
vsly_avertedtotals_iso3c
write.csv(vsly_avertedtotals_iso3c, "analysis/tables/vsly_avertedtotals_iso3c.csv")

# getting vsly in terms of percentage of gdp for each iso3c
# undiscounted
undiscvsly_pgdp_iso3c <- vsly %>%
  mutate(vsly_undisc_pgdp = (lg_averted*vly / gdp) * 100) %>%
  group_by(iso3c, replicate) %>%
  summarise(vsly_undisc_pgdp = sum(vsly_undisc_pgdp)) %>%
  group_by(iso3c) %>%
  summarise(
    across(vsly_undisc_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscvsly_pgdp_iso3c
write.csv(undiscvsly_pgdp_iso3c, "analysis/tables/undiscvsly_pgdp_iso3c.csv")

# discounted
discvsly_pgdp_iso3c <- vsly %>%
  mutate(vsly_disc_pgdp = (lghat_averted*vly_disc / gdp) * 100) %>%
  group_by(iso3c, replicate) %>%
  summarise(vsly_disc_pgdp = sum(vsly_disc_pgdp)) %>%
  group_by(iso3c) %>%
  summarise(
    across(vsly_disc_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discvsly_pgdp_iso3c
write.csv(discvsly_pgdp_iso3c, "analysis/tables/discvsly_pgdp_iso3c.csv")

# undiscounted vsly gained pp vaccinated per iso3c
undiscvsly_pp_iso3c <- vsly %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(undiscvsly_total = sum(lg_averted*vly,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(undiscvsly_pp = undiscvsly_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(undiscvsly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscvsly_pp_iso3c
write.csv(undiscvsly_pp_iso3c, "analysis/tables/undiscvsly_pp_iso3c.csv")

# discounted vsly gained pp vaccinated per iso3c
discvsly_pp_iso3c <- vsly %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(discvsly_total = sum(lghat_averted*vly_disc,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(discvsly_pp = discvsly_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(discvsly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discvsly_pp_iso3c
write.csv(discvsly_pp_iso3c, "analysis/tables/discvsly_pp_iso3c.csv")

# for each country
undiscvsly_pp_gdppc_iso3c <- vsly %>%
  filter(!is.na(vly)) %>%
  group_by(iso3c, replicate) %>%
  summarise(undiscvsly_total = sum(lg_averted * vly, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>%
              group_by(iso3c) %>%
              summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "iso3c") %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = sum(gdppc, na.rm = TRUE)), by = "iso3c") %>%
  mutate(undiscvsly_pp_gdppc = ((undiscvsly_total / vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(undiscvsly_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscvsly_pp_gdppc_iso3c
write.csv(undiscvsly_pp_gdppc_iso3c, "analysis/tables/undiscvsly_pp_gdppc_iso3c.csv")

# for each country
discvsly_pp_gdppc_iso3c <- vsly %>%
  filter(!is.na(vly)) %>%
  group_by(iso3c, replicate) %>%
  summarise(discvsly_total = sum(lghat_averted * vly_disc, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>%
              group_by(iso3c) %>%
              summarise(vaccines = sum(vaccines, na.rm = TRUE)),
            by = "iso3c") %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE))) %>%
  mutate(discvsly_pp_gdppc = ((discvsly_total / vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(discvsly_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discvsly_pp_gdppc_iso3c
write.csv(discvsly_pp_gdppc_iso3c, "analysis/tables/discvsly_pp_gdppc_iso3c.csv")

#### MONETIZED QALYS PER ISO3C RESULTS ####
# calculating number of QALYs averted for infections for each iso3c

inf_qaly_iso3c <- qaly %>%
  filter(name == "infections") %>%
  mutate(averted_inf_qalys = averted
         * -(qaly_loss)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_inf_qalys = sum(averted_inf_qalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_inf_qalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
inf_qaly_iso3c
write.csv(inf_qaly_iso3c, "analysis/tables/inf_qaly_iso3c.csv")

# calculating monetized QALYs averted for infections for each iso3c
inf_monqaly_iso3c <- qaly %>%
  filter(name == "infections") %>%
  mutate(averted_inf_qalys = averted
         * -(qaly_loss)) %>%
  mutate(averted_inf_monqalys = (averted_inf_qalys * median_wtp_threshold)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_inf_monqalys = sum(averted_inf_monqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_inf_monqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
inf_monqaly_iso3c
write.csv(inf_monqaly_iso3c, "analysis/tables/inf_monqaly_iso3c.csv")

# caclulating number of QALYs averted for hospitalisations for each iso3c
hosp_qaly_iso3c <- qaly %>%
  filter(name == "infections") %>%
  mutate(averted_hosp_qalys = averted
         * -(qaly_loss)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_hosp_qalys = sum(averted_hosp_qalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_hosp_qalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hosp_qaly_iso3c
write.csv(hosp_qaly_iso3c, "analysis/tables/hosp_qaly_iso3c")

# calculating monetized QALYs averted for hospitalisations for each iso3c
hosp_monqaly_iso3c <- qaly %>%
  filter(name == "infections") %>%
  mutate(averted_hosp_qalys = averted
         * -(qaly_loss)) %>%
  mutate(averted_hosp_monqalys = averted_hosp_qalys * median_wtp_threshold) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_hosp_monqalys = sum(averted_hosp_monqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_hosp_monqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hosp_monqaly_iso3c
write.csv(hosp_monqaly_iso3c, "analysis/tables/hosp_monqaly_iso3c.csv")

# calculating number of QALYs averted for deaths for each iso3c
deaths_undiscqaly_iso3c <- qaly %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_undiscqalys = ((averted * -qaly_loss) + lg_averted)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_deaths_undiscqalys = sum(averted_deaths_undiscqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_deaths_undiscqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_undiscqaly_iso3c
write.csv(deaths_undiscqaly_iso3c, "analysis/tables/deaths_undiscqaly_iso3c.csv")

# calculating monetized QALYs averted for deaths for each iso3c
deaths_undiscmonqaly_iso3c <- qaly %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_undiscqalys = ((averted * -qaly_loss) + lg_averted)) %>%
  mutate(averted_deaths_undiscmonqalys = (averted_deaths_undiscqalys * median_wtp_threshold)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_deaths_undiscmonqalys = sum(averted_deaths_undiscmonqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_deaths_undiscmonqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_undiscmonqaly_iso3c
write.csv(deaths_undiscmonqaly_iso3c, "analysis/tables/deaths_undiscmonqaly_iso3c.csv")

# sum of all undiscounted qalys for infections, hospitalisations, deaths
sum_undiscqaly_iso3c <- qaly %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted))
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
sum_undiscqaly_iso3c
write.csv(sum_undiscqaly_iso3c, "analysis/tables/sum_undiscqaly_iso3c.csv")

# get qaly's per-person vaccinated
undiscqaly_pp_iso3c <- qaly %>%
  mutate(undiscqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted))
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(undiscqalys_averted = sum(undiscqalys_averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(undiscqaly_pp = undiscqalys_averted / vaccines) %>%
  group_by(iso3c) %>%
  summarise(
    across(undiscqaly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscqaly_pp_iso3c
write.csv(undiscqaly_pp_iso3c, "analysis/tables/undiscqaly_pp_iso3c.csv")

# sum of all monetized undiscounted qalys for infections, hospitalisations, deaths
sum_undiscmonqaly_iso3c <- qaly %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
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
sum_undiscmonqaly_iso3c
write.csv(sum_undiscmonqaly_iso3c, "analysis/tables/sum_undiscmonqaly_iso3c.csv")

# get mean monetized undiscounted QALYS as proportion of GDP per iso3c
undiscmonqaly_pgdp_iso3c <- qaly %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold)
  )) %>%
  mutate(undiscmonqalys_pgdp = (undiscmonqalys_averted / gdp) * 100) %>%
  group_by(iso3c, replicate) %>%
  summarise(undiscmonqalys_pgdp = sum(undiscmonqalys_pgdp)) %>%
  group_by(iso3c) %>%
  summarise(
    across(undiscmonqalys_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
undiscmonqaly_pgdp_iso3c
write.csv(undiscmonqaly_pgdp_iso3c, "analysis/tables/undiscmonqaly_pgdp_iso3c.csv")

# calculating number of QALYs averted for deaths for each iso3c

deaths_discqaly_iso3c <- qaly %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_discqalys = ((averted * -qaly_loss) + lghat_averted)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_deaths_discqalys = sum(averted_deaths_discqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_deaths_discqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_discqaly_iso3c
write.csv(deaths_discqaly_iso3c, "analysis/tables/deaths_discqaly_iso3c.csv")

# calculating monetized QALYs averted for deaths for each iso3c
deaths_discmonqaly_iso3c <- qaly %>%
  filter(name == "deaths") %>%
  mutate(averted_deaths_discqalys = ((averted * -qaly_loss) + lghat_averted)) %>%
  mutate(averted_deaths_discmonqalys = (averted_deaths_discqalys * median_wtp_threshold)) %>%
  group_by(iso3c, replicate) %>%
  summarise(averted_deaths_discmonqalys = sum(averted_deaths_discmonqalys, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(averted_deaths_discmonqalys,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
deaths_discmonqaly_iso3c
write.csv(deaths_discmonqaly_iso3c, "analysis/tables/deaths_discmonqaly_iso3c.csv")

# sum of all discounted qalys for infections, hospitalisations, deaths
sum_discqaly_iso3c <- qaly %>%
  mutate(qalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted))
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
sum_discqaly_iso3c
write.csv(sum_discqaly_iso3c, "analysis/tables/sum_discqaly_iso3c.csv")

# get discounted qaly's per-person vaccinated per iso3c
discqaly_pp_iso3c <- qaly %>%
  mutate(discqaly_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss),
    name == "hospitalisations" ~ averted
    * -(qaly_loss),
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted))
  )) %>%
  group_by(iso3c, replicate) %>%
  summarise(disqaly_averted = sum(discqaly_averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  mutate(discqaly_pp = disqaly_averted / vaccines) %>%
  group_by(iso3c) %>%
  summarise(
    across(discqaly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))


# our results table which we can then save in the tables directory
discqaly_pp_iso3c
write.csv(discqaly_pp_iso3c, "analysis/tables/discqaly_pp_iso3c.csv")

# sum of all monetized discounted qalys for infections, hospitalisations, deaths
sum_discmonqaly_iso3c <- qaly %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold)
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
sum_discmonqaly_iso3c
write.csv(sum_discmonqaly_iso3c, "analysis/tables/sum_discmonqaly_iso3c.csv")


# get mean monetized qalys in proportion of gdp per iso3c
discmonqaly_pgdp_iso3c <- qaly %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold)
  )) %>%
  mutate(discmonqalys_pgdp = (discmonqalys_averted / gdp) * 100) %>%
  group_by(iso3c, replicate) %>%
  summarise(discmonqalys_pgdp = sum(discmonqalys_pgdp)) %>%
  group_by(iso3c) %>%
  summarise(
    across(discmonqalys_pgdp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discmonqaly_pgdp_iso3c
write.csv(discmonqaly_pgdp_iso3c, "analysis/tables/discmonqaly_pgdp_iso3c.csv")

# get in terms of per person vaccinated

# undiscounted monetized qalys gained pp vaccinated per iso3c
undiscmonqaly_pp_iso3c <- qaly %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold))) %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(monqaly_total = sum(undiscmonqalys_averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(monqaly_pp = monqaly_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(monqaly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscmonqaly_pp_iso3c
write.csv(undiscmonqaly_pp_iso3c, "analysis/tables/undiscmonqaly_pp_iso3c.csv")

# discounted monetized qalys gained pp vaccinated per iso3c
discmonqaly_pp_iso3c <- qaly %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold))) %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(discmonqaly_total = sum(discmonqalys_averted,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(discmonqaly_pp = discmonqaly_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(discmonqaly_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discmonqaly_pp_iso3c
write.csv(discmonqaly_pp_iso3c, "analysis/tables/discmonqaly_pp_iso3c.csv")

# for each country
undiscmonqaly_pp_gdppc_iso3c <- qaly %>%
  mutate(undiscmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lg_averted)
                        * median_wtp_threshold))) %>%
  group_by(iso3c, replicate) %>%
  summarise(undiscmonqalys_total = sum(undiscmonqalys_averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE))) %>%
  mutate(undiscmonqalys_pp_gdppc = ((undiscmonqalys_total / vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(undiscmonqalys_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
undiscmonqaly_pp_gdppc_iso3c
write.csv(undiscmonqaly_pp_gdppc_iso3c, "analysis/tables/undiscmonqaly_pp_gdppc_iso3c.csv")

# for each country
discmonqaly_pp_gdppc_iso3c <- qaly %>%
  mutate(discmonqalys_averted = case_when(
    name == "infections" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "hospitalisations" ~ averted
    * -(qaly_loss) * median_wtp_threshold,
    name == "deaths" ~ (((averted * -qaly_loss) + lghat_averted)
                        * median_wtp_threshold))) %>%
  group_by(iso3c, replicate) %>%
  summarise(discmonqalys_total = sum(discmonqalys_averted, na.rm = TRUE)) %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE))) %>%
  mutate(discmonqalys_pp_gdppc = ((discmonqalys_total / vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(discmonqalys_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
discmonqaly_pp_gdppc_iso3c
write.csv(discmonqaly_pp_gdppc_iso3c, "analysis/tables/discmonqaly_pp_gdppc_iso3c.csv")

#### FRICTION COSTS PER ISO3C RESULTS ####
friction_pp_gdppc_iso3c <- friction_costs %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(friction_total = sum(friction_costs,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)), by = "iso3c") %>%
  mutate(friction_pp_gdppc = ((friction_total/vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(friction_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
friction_pp_gdppc_iso3c
write.csv(friction_pp_gdppc_iso3c, "analysis/tables/friction_pp_gdppc_iso3c.csv")

# friction sum for each country
friction_sum_iso3c <- friction_costs %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(friction_total = sum(friction_costs,na.rm=TRUE)) %>%
  group_by(iso3c) %>%
  summarise(
    across(friction_total,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
friction_sum_iso3c
write.csv(friction_sum_iso3c, "analysis/tables/friction_sum_iso3c.csv")

#### HEALTHCARE COSTS PER ISO3C RESULTS ####

# get mean % of GDP for each iso3c
hccosts_pgdp_iso3c <- hc_costs_grouped %>%
  group_by(iso3c, replicate) %>%
  summarise(hccosts_total = sum(cost_total), na.rm = TRUE) %>%
  left_join(gdp %>% group_by(iso3c) %>% summarise(gdp = sum(gdp, na.rm = TRUE)), by = "iso3c") %>%
  mutate(hccosts_pgdp = (hccosts_total/gdp) * 100) %>%
  group_by(iso3c) %>%
  summarise(across(hccosts_pgdp,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))

# our results table which we can then save in the tables directory
hccosts_pgdp_iso3c
write.csv(hccosts_pgdp_iso3c, "analysis/tables/hccosts_pgdp_iso3c.csv")

# healthcare costs averted pp vaccinated per iso3c
hccosts_pp_iso3c <- hc_costs_grouped %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(hccosts_total = sum(cost_total,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(hccosts_pp = hccosts_total/vaccines) %>%  # (step 3)
  group_by(iso3c) %>%
  summarise(
    across(hccosts_pp,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hccosts_pp_iso3c
write.csv(hccosts_pp_iso3c, "analysis/tables/hccosts_pp_iso3c.csv")

# per iso3c
hccosts_pp_gdppc_iso3c <- hc_costs_grouped %>%
  group_by(iso3c, replicate) %>% # (step 1)
  summarise(hccosts_total = sum(cost_total,na.rm=TRUE)) %>% # (step 1)
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>%
  left_join(gdppc %>% group_by(iso3c) %>% summarise(gdppc = mean(gdppc, na.rm = TRUE)), by = "iso3c") %>%
  mutate(hccosts_pp_gdppc = ((hccosts_total/vaccines) / gdppc) * 100) %>%
  group_by(iso3c) %>%
  summarise(
    across(hccosts_pp_gdppc,
           list(
             low = lf,
             med = mf,
             high = hf
           )))

# our results table which we can then save in the tables directory
hccosts_pp_gdppc_iso3c
write.csv(hccosts_pp_gdppc_iso3c, "analysis/tables/hccosts_pp_gdppc_iso3c.csv")

#### DELIVERY COSTS PER ISO3C RESULTS ####
# delivery costs
del_cost_iso3c <- res_full %>%
  left_join(vaccine_iso3c %>% group_by(iso3c) %>% summarise(vaccines = sum(vaccines, na.rm = TRUE))) %>% # (step 2)
  mutate(del_cost = vaccines * 3.70) %>%
  group_by(iso3c, replicate) %>%
  summarise(del_cost_total = sum(del_cost, na.rm = TRUE)) %>%
  group_by(iso3c) %>%
  summarise(across(del_cost_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))



# our results table which we can then save in the tables directory
del_cost_iso3c
write.csv(del_cost_iso3c, "analysis/tables/del_cost_iso3c.csv")


# sum costs averted for each iso3c
hc_costs_total_iso3c <- hc_costs_grouped  %>%
  group_by(iso3c, replicate) %>%
  summarise(health_costs_total = sum(cost_total)) %>%
  group_by(iso3c) %>%
  summarise(across(health_costs_total,
                   list(
                     low = lf,
                     med = mf,
                     high = hf
                   )))


# our results table which we can then save in the tables directory
hc_costs_total_iso3c
write.csv(hc_costs_total_iso3c, "analysis/tables/hc_costs_total_iso3c.csv")
