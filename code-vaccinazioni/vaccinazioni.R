setwd("C:/Users/Roberto/Desktop/rstudio_default/covid")

library(tidyverse)
library(data.table)
library(lubridate)

options(scipen = 9999999)

vac_all <- fread("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-summary-latest.csv",
                 encoding = "UTF-8")

#### lista regioni ####

lista_regioni <- fread("lista_regioni.csv") %>% 
  select(-regione)

vac_all <- vac_all %>% 
  left_join(lista_regioni, by = c("area" = "sigla")) %>% 
  mutate(nome_area = str_replace_all(nome_area, "Provincia Autonoma ", ""),
         nome_area = str_replace_all(nome_area, "Valle d'Aosta / Vallée d'Aoste", "Valle d'Aosta"))

####data prep####

vac_all <- vac_all %>%
  filter(data_somministrazione > "2020-12-01") %>% 
  rename(n_vaccinazioni = totale,
         data = data_somministrazione)

vac_select <- vac_all %>% group_by(area) %>% arrange(data) %>% mutate(tot_vaccinazioni = cumsum(n_vaccinazioni),
                                                 tot_prime_dosi = cumsum(prima_dose),
                                                 tot_seconde_dosi = cumsum(seconda_dose),
                                                 tasso_vaccinazioni = (tot_vaccinazioni / popolazione)*100,
                                                 tasso_seconde_dosi = (tot_seconde_dosi / popolazione)*100,
                                                 tasso_vaccinazioni_giornaliero = (n_vaccinazioni / popolazione)*100
                                                ,media_vaccinazioni_3gg = frollmean(x = n_vaccinazioni, n = 3, fill = 0)
                                                ) %>% ungroup() %>% 
  select(data, nome_area, tot_vaccinazioni, n_vaccinazioni, 
         prima_dose, tot_prime_dosi, 
         seconda_dose, tot_seconde_dosi,
         tasso_vaccinazioni, tasso_seconde_dosi, tasso_vaccinazioni_giornaliero, media_vaccinazioni_3gg,
         popolazione, area) %>% 
  arrange(data)

vac_reg = vac_select

vac_reg_today = vac_select %>% filter(data == max(data)) %>% 
  rename(vacc_oggi = n_vaccinazioni) %>% 
  arrange(desc(tasso_vaccinazioni))

vac_ita <- vac_select %>% 
  group_by(data) %>% 
  summarise(tot_vaccinazioni = sum(tot_vaccinazioni),
            n_vaccinazioni = sum(n_vaccinazioni),
            prima_dose = sum(prima_dose),
            tot_prime_dosi = sum(tot_prime_dosi),
            seconda_dose = sum(seconda_dose),
            tot_seconde_dosi = sum(tot_seconde_dosi),
            popolazione = sum(popolazione)) %>% 
  ungroup() %>% 
  arrange(desc(data)) %>% 
  mutate(popolazione = 60242096,
         tasso_vaccinazioni = (tot_vaccinazioni / popolazione)*100,
         tasso_seconde_dosi = (tot_seconde_dosi / popolazione)*100,
         tasso_vaccinazioni_giornaliero = (n_vaccinazioni / popolazione)*100
         #media_vaccinazioni_3gg = frollmean(x = n_vaccinazioni, n = 3, fill = 0
                                            ) %>% 
  select(-popolazione) 
  
  
vac_ita_today = vac_ita %>% filter(data == max(data)) %>% 
  rename(vacc_oggi = n_vaccinazioni)

colnames <- colnames(vac_select)
colnames_remove <- c("media_vaccinazioni_3gg", "popolazione")
colnames2 <- setdiff(colnames, colnames_remove)

vac_ita_all <- vac_ita %>% 
  mutate(area = "ITA",
         nome_area = "TOTALE") %>% 
  full_join(vac_select, by = colnames2) %>% 
  relocate(nome_area, .after= data)

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

vac_ita_longer <- vac_ita %>% 
  select(data, tot_prime_dosi, tot_seconde_dosi) %>% 
  pivot_longer(!data, names_to = "dose", names_prefix = "tot_", values_to = "num_dosi") %>% 
  mutate(dose = factor(dose, levels = c("seconde_dosi", "prime_dosi"))) 

vac_ita_longer %>% 
  ggplot() +
  geom_area(aes(data, num_dosi, fill = dose)) +
  scale_fill_manual(values = c("darkred", "steelblue")) +
  #guides(fill = "none") +
  theme_minimal()

vac_lazio_trend <- vac_ita_all %>%
  filter(area == "LAZ") %>% 
  select(data, tot_prime_dosi, tot_seconde_dosi) %>% 
  pivot_longer(!data, names_to = "dose", names_prefix = "tot_", values_to = "num_dosi") %>% 
  mutate(dose = factor(dose, levels = c("seconde_dosi", "prime_dosi")))  %>% 
  ggplot() +
  geom_area(aes(data, num_dosi, fill = dose)) +
  scale_fill_manual(values = c("darkred", "steelblue")) +
  #guides(fill = "none") +
  theme_minimal()

vac_trend_plot <- vac_ita_all %>%
  select(data, area, tot_prime_dosi, tot_seconde_dosi) %>% 
  pivot_longer(!c(data, area), names_to = "dose", names_prefix = "tot_", values_to = "num_dosi") %>% 
  mutate(dose = factor(dose, levels = c("seconde_dosi", "prime_dosi")))  %>% 
  ggplot() +
  geom_area(aes(data, num_dosi, fill = dose)) +
  scale_fill_manual(values = c("darkred", "steelblue")) +
  #guides(fill = "none") +
  facet_wrap(~area, scales = "free_y") +
  theme_minimal() +
  ggtitle("Dosi cumulative vaccino, Italia e regioni")
vac_trend
ggsave("vac_trend.png", width = 20, height = 12)

#### summaries ####

vac_ita_all %>% filter(data == max(data)) %>% select(-c(data, area, media_vaccinazioni_3gg, popolazione)) %>% 
  rename(vacc_oggi = n_vaccinazioni, prima_dose_oggi = prima_dose, seconda_dose_oggi = seconda_dose) %>% 
  arrange(desc(tasso_vaccinazioni)) %>% View
vac_ita %>% arrange(desc(data))
vac_ita_today


