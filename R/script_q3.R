Script da questão 03

dados_peixes <- read_csv("avaliacao/dados/brutos/peixes_rio_madeira.csv")


dados_peixes %>%
  count(sexo)

dados_peixes %>%
  group_by(peso_g)%>%
  mutate(sexo_recod = recode(
    sexo, "Fêmea" = "Fêmea",
          "fêmea" = "Fêmea",
          "Macho" = "Macho"  ))%>%
  count(sexo_recod) %>%
  summarise(sexo_recod= sexo_recod
  )%>%
  filter(sexo_recod %in% c("Fêmea", "Macho"))%>%
  arrange(desc(peso_g)) %>%
  top_n(5)



x <- 56.9-43.1
  x
