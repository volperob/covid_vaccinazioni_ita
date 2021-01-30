library(openxlsx)
vac_trend_plot_mar <- dosi_reg_longer %>% 
  filter(area == "MAR") %>% 
  ggplot() +
  geom_area(aes(data, num_dosi, fill = dose)) +
  scale_fill_manual(values = c("darkred", "steelblue")) +
  #guides(fill = "none") +
  facet_wrap(~area, scales = "free_y") +
  theme_minimal() +
  ggtitle("Dosi cumulative vaccino, regione Marche")
vac_trend_plot_mar
ggsave("C://Users/Roberto/Desktop/dosi_marche.png")

dosi_reg_longer_day %>% 
  filter(area == "MAR") %>% arrange(desc(data)) %>% 
  write.xlsx("C://Users/Roberto/Desktop/dosi_marche.xlsx")


vac_ita_all %>% 
  filter(data == max(data)) %>% 
  ggplot(aes(reorder(nome_area, tasso_seconde_dosi), tasso_seconde_dosi)) +
  geom_col(aes(fill = ifelse(nome_area == "Marche", "Normal", "Highlighted"))) +
  coord_flip() +
  xlab("regione") +
  guides(fill = "none") +
  geom_label(nudge_y = 0.1, aes(label = round(tasso_seconde_dosi, digits = 2))) +
  theme_minimal() +
  ggtitle("tasso popolazione vaccinata (2 dosi)")



