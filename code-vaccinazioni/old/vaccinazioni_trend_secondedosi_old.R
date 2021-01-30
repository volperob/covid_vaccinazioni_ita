setwd("C:/Users/Roberto/Desktop/rstudio_default/covid/covid_vaccinazioni_ita/wd-vaccinazioni")

library(tidyverse)
library(data.table)
library(lubridate)
library(xts)

vac_dosi_trend <- fread("vac_reg_longer.csv")

vac_all

vac_lead <- vac_all %>% 
  #filter(data < today()) %>% 
  select(data, area, prima_dose, seconda_dose) %>% 
  group_by(area) %>% 
  mutate(seconda_dose_meno21 = lead(seconda_dose, 21)) %>% 
  ungroup()

vac_lead_long = vac_lead %>% 
  pivot_longer(-c(data, area), names_to = "dose", values_to = "n") %>% 
  mutate(dose = replace_na(dose, 0),
         data = as.POSIXct(data)) %>% 
  mutate(dose = case_when(dose == "prima_dose" ~ "prima dose",
                          dose == "seconda_dose" ~ "seconda dose",
                          dose == "seconda_dose_meno21" ~ "seconda dose (-21 giorni)",
                          TRUE  ~ ""))

class(vac_lead_long$data)
vac <- as.xts(vac_lead_long)

vac_lead_long %>% 
  filter(dose != "seconda dose" & area == "ITA") %>% 
  ggplot() +
  aes(data, n, color = dose) +
  geom_line(size = 2)

vac_lead_long %>% 
  filter(dose != "seconda dose" & area == "LAZ") %>% 
  ggplot() +
  aes(data, n, color = dose) +
  geom_line(size = 2) +
  ggtitle("regione Lazio")

vac_lead_long %>% 
  filter(dose != "seconda dose" & area == "MOL") %>% 
  ggplot() +
  aes(data, n, color = dose) +
  geom_line(size = 2) +
  ggtitle("regione Molise")

vac_lead_long %>% 
  filter(dose != "seconda dose" & area != "ITA") %>% 
  ggplot() +
  aes(data, n, color = dose) +
  geom_line(size = 2) +
  facet_wrap(~ area, scales = "free_y")


