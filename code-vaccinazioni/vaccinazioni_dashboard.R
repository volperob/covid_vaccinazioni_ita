load("vac_ita_dataprep.RData")

#### summaries ####

vac_ita_all %>% 
  filter(data == max(data)) %>% 
  ggplot(aes(reorder(nome_area, tasso_vaccinazioni), tasso_vaccinazioni)) +
  geom_col(aes(fill = ifelse(nome_area == "TOTALE", "Normal", "Highlighted"))) +
  coord_flip() +
  xlab("regione") +
  guides(fill = "none") +
  geom_label(nudge_y = 0.3, aes(label = round(tasso_vaccinazioni, digits = 2))) +
  theme_minimal()
  

vac_ita_all %>% filter(data == max(data)) %>% select(-c(data, area, media_vaccinazioni_3gg, popolazione)) %>% 
  rename(vacc_oggi = n_vaccinazioni, prima_dose_oggi = prima_dose, seconda_dose_oggi = seconda_dose) %>% 
  arrange(desc(tasso_vaccinazioni)) %>% View
vac_ita %>% arrange(desc(data))
vac_ita_today
