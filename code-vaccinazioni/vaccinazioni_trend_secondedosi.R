load("vac_ita_dataprep.RData")

library(xts)
library(tsibble)

#### giornaliero ####

vac_reg_wider <- dosi_reg_longer_day %>% 
  pivot_wider(names_from = dose, values_from = num_dosi)

vac_reg_wider_ts <- tsibble(vac_reg_wider, key = area, index = data) 
#class(vac_reg_wider_ts$data)

vac_reg_secondedosi <- vac_reg_wider_ts %>% 
  fill_gaps(seconda_dose = 0,
            prima_dose = 0) 

vac_reg_secondedosi <- vac_reg_secondedosi %>% 
  mutate(seconda_dose_lead21 = lead(seconda_dose, n = 21L))

vac_reg_secondedosi$seconda_dose_lead21[vac_reg_secondedosi$data > today()-22] <- NA

vac_reg_secondedosi_long <- vac_reg_secondedosi %>%  
  pivot_longer(-c(data, area), names_to = "dose", values_to = "n") %>% 
  mutate(dose = case_when(dose == "prima_dose" ~ "prima dose",
                          dose == "seconda_dose" ~ "seconda dose",
                          dose == "seconda_dose_lead21" ~ "seconda dose (-21 giorni)",
                          dose == "prima_dose_lag21" ~ "prima dose (+21 giorni)",
                          TRUE  ~ ""))

vac_reg_secondedosi_long %>% View()

vac_reg_secondedosi_long %>% 
  filter(dose %in% c("seconda dose (-21 giorni)", "prima dose") 
         & data <= today() 
         ) %>% 
  ggplot() +
  aes(data, n, color = dose) +
  geom_line(size = 2) +
  facet_wrap(~ area, scales = "free_y") +
  theme_void()

#### cumulato ####

vac_reg_wider_c <- dosi_reg_longer %>% 
  pivot_wider(names_from = dose, values_from = num_dosi)

vac_reg_wider_c_ts <- tsibble(vac_reg_wider_c, key = area, index = data) 

vac_reg_secondedosi_c <- vac_reg_wider_c_ts %>%  
  group_by_key() %>% 
  tidyr::fill(prime_dosi, seconde_dosi, .direction = "down")

vac_reg_secondedosi_c <- vac_reg_secondedosi_c %>% 
  mutate(seconda_dose_lead21 = lead(seconde_dosi, n = 21L))

vac_reg_secondedosi_c$seconda_dose_lead21[vac_reg_secondedosi_c$data > today()-22] <- NA

vac_reg_secondedosi_c_long <- vac_reg_secondedosi_c %>%  
  pivot_longer(-c(data, area), names_to = "dose", values_to = "n") %>% 
  mutate(dose = case_when(dose == "prime_dosi" ~ "prima dose",
                          dose == "seconde_dosi" ~ "seconda dose",
                          dose == "seconda_dose_lead21" ~ "seconda dose (-21 giorni)",
                          dose == "prima_dose_lag21" ~ "prima dose (+21 giorni)",
                          TRUE  ~ ""))

vac_reg_secondedosi_c_long %>% View()

vac_reg_secondedosi_c_long %>% 
  filter(dose %in% c("seconda dose (-21 giorni)", "prima dose") 
         & data <= today() 
  ) %>% 
  ggplot() +
  aes(data, n, color = dose) +
  geom_line(size = 2) +
  facet_wrap(~ area, scales = "free_y") +
  theme_void()

