load("vac_ita_dataprep.RData")

#### viz ####

vac_ita %>% ggplot() +
  geom_col(aes(data, n_vaccinazioni), fill = "steelblue") +
  #geom_line(aes(data, media_vaccinazioni_3gg), size = 3, color = "red") +
  theme_minimal() +
  scale_x_date(date_breaks = "5 days")

vac_ita_all %>% ggplot() +
  geom_col(aes(data, tasso_vaccinazioni_giornaliero, fill = tasso_vaccinazioni_giornaliero)) +
  geom_line(aes(data, tasso_vaccinazioni), color = "steelblue", size = 2) +
  scale_fill_viridis_b() +
  guides(fill = "none") +
  theme_minimal() +
  ylab("vaccinati/pop") +
  facet_wrap(~area) +
  scale_x_date(date_breaks = "14 days", date_minor_breaks = "2 day")

vac_ita %>% ggplot() +
  geom_col(aes(data, tasso_vaccinazioni_giornaliero, fill = tasso_vaccinazioni_giornaliero)) +
  geom_line(aes(data, tasso_vaccinazioni), size = 3, color = "steelblue") +
  scale_fill_viridis_b() +
  guides(fill = "none") +
  theme_minimal() +
  ylab("vaccinati/pop")

dosi_ita_longer %>% 
  ggplot() +
  geom_area(aes(data, num_dosi, fill = dose)) +
  scale_fill_manual(values = c("darkred", "steelblue")) +
  #guides(fill = "none") +
  theme_minimal()
 
#esempio trend: lazio

vac_ita_all %>%
  filter(area == "LAZ") %>% 
  select(data, tot_prime_dosi, tot_seconde_dosi) %>% 
  pivot_longer(!data, names_to = "dose", names_prefix = "tot_", values_to = "num_dosi") %>% 
  mutate(dose = factor(dose, levels = c("seconde_dosi", "prime_dosi")))  %>% 
  ggplot() +
  geom_area(aes(data, num_dosi, fill = dose)) +
  scale_fill_manual(values = c("darkred", "steelblue")) +
  #guides(fill = "none") +
  theme_minimal()

vac_trend_plot <- dosi_reg_longer %>% 
  ggplot() +
  geom_area(aes(data, num_dosi, fill = dose)) +
  scale_fill_manual(values = c("darkred", "steelblue")) +
  #guides(fill = "none") +
  facet_wrap(~area, scales = "free_y") +
  theme_minimal() +
  ggtitle("Dosi cumulative vaccino, Italia e regioni")
vac_trend_plot

ggsave("vac_trend.png", width = 20, height = 12)


#### summaries ####

vac_ita_all %>% filter(data == max(data)) %>% select(-c(data, area, media_vaccinazioni_3gg, popolazione)) %>% 
  rename(vacc_oggi = n_vaccinazioni, prima_dose_oggi = prima_dose, seconda_dose_oggi = seconda_dose) %>% 
  arrange(desc(tasso_vaccinazioni)) %>% View
vac_ita %>% arrange(desc(data))
vac_ita_today


