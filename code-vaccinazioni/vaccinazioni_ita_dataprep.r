setwd("C:/Users/Roberto/Desktop/rstudio_default/covid/covid_vaccinazioni_ita/wd-vaccinazioni")

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
         data = data_somministrazione) %>% 
  mutate(data = as_date(data))

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

dosi_ita_longer <- vac_ita %>% 
  select(data, tot_prime_dosi, tot_seconde_dosi) %>% 
  pivot_longer(!data, names_to = "dose", names_prefix = "tot_", values_to = "num_dosi") %>% 
  mutate(dose = factor(dose, levels = c("seconde_dosi", "prime_dosi"))) 
fwrite(dosi_ita_longer, "vac_ita_longer.csv")

dosi_reg_longer <- vac_ita_all %>%
  select(data, area, tot_prime_dosi, tot_seconde_dosi) %>% 
  pivot_longer(!c(data, area), names_to = "dose", names_prefix = "tot_", values_to = "num_dosi") %>% 
  mutate(dose = factor(dose, levels = c("seconde_dosi", "prime_dosi")))
fwrite(dosi_reg_longer, "vac_reg_longer.csv")

dosi_ita_longer_day <- vac_ita %>% 
  select(data, prima_dose, seconda_dose) %>% 
  pivot_longer(!data, names_to = "dose", values_to = "num_dosi") %>% 
  mutate(dose = factor(dose, levels = c("seconda_dose", "prima_dose"))) 
fwrite(dosi_ita_longer_day, "vac_ita_longer_day.csv")

dosi_reg_longer_day <- vac_ita_all %>%
  select(data, area, prima_dose, seconda_dose) %>% 
  pivot_longer(!c(data, area), names_to = "dose", values_to = "num_dosi") %>% 
  mutate(dose = factor(dose, levels = c("seconda_dose", "prima_dose"))) 
fwrite(dosi_reg_longer_day, "vac_reg_longer_day.csv")


vac_rank_plot <- vac_ita_all %>% 
  filter(data == max(data)) %>% 
  ggplot(aes(reorder(nome_area, tasso_vaccinazioni), tasso_vaccinazioni)) +
  geom_col(aes(fill = ifelse(nome_area == "TOTALE", "Normal", "Highlighted"))) +
  coord_flip() +
  xlab("regione") +
  guides(fill = "none") +
  geom_label(nudge_y = 0.3, aes(label = round(tasso_vaccinazioni, digits = 2))) +
  theme_minimal()

vac_rank_sd_plot <- vac_ita_all %>% 
  filter(data == max(data)) %>% 
  ggplot(aes(reorder(nome_area, tasso_seconde_dosi), tasso_seconde_dosi)) +
  geom_col(aes(fill = ifelse(nome_area == "TOTALE", "Normal", "Highlighted"))) +
  coord_flip() +
  xlab("regione") +
  guides(fill = "none") +
  geom_label(nudge_y = 0.1, aes(label = round(tasso_seconde_dosi, digits = 2))) +
  theme_minimal()

vac_trend_plot <- dosi_reg_longer %>% 
  ggplot() +
  geom_area(aes(data, num_dosi, fill = dose)) +
  scale_fill_manual(values = c("darkred", "steelblue")) +
  #guides(fill = "none") +
  facet_wrap(~area, scales = "free_y") +
  theme_minimal() +
  ggtitle("Dosi cumulative vaccino, Italia e regioni")

save.image("C:/Users/Roberto/Desktop/rstudio_default/covid/covid_vaccinazioni_ita/wd-vaccinazioni/vac_ita_dataprep.RData")
