Script da Questão 04

library(readr)
dados_peixes<- read_csv("avaliacao/dados/brutos/peixes_rio_madeira.csv")
View(peixes_rio_madeira)

dados_peixes %>%
group_by(habito_alimentar, sexo)%>%
  count(habito_alimentar)%>%
summarise(habito_alimentar = habito_alimentar,
          sexo = sexo,
          quant = n)%>%
 filter(habito_alimentar == "Carnívoro" & sexo == "Macho")
