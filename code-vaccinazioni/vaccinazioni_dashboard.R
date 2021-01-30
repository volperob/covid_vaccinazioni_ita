load("vac_ita_dataprep.RData")

#### summaries ####
vac_rank_plot
vac_rank_sd_plot
vac_trend_plot

vac_ita_all %>% filter(data == max(data)) %>% select(-c(data, area, media_vaccinazioni_3gg, popolazione)) %>% 
  rename(vacc_oggi = n_vaccinazioni, prima_dose_oggi = prima_dose, seconda_dose_oggi = seconda_dose) %>% 
  arrange(desc(tasso_vaccinazioni)) %>% View
vac_ita %>% arrange(desc(data))
vac_ita_today
