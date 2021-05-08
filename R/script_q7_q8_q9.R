Script da Questão 07, 08 e 09

library(readr)
dados_cursos <- read_csv("avaliacao/dados/brutos/cursos-prouni.csv")
View(cursos_prouni)

dados_cursos %>%
  filter(turno == "Integral")%>%
  summarise(media = mean(nota_integral_ampla, na.rm = T),
            mediana = median(nota_integral_ampla, na.rm = T)
      )
dados_cursos %>%
  select(turno, nota_integral_ampla) %>%
  group_by(turno) %>%
  summarise(med_t = mean(nota_integral_ampla, na.rm = T),
            des = sd(nota_integral_ampla, na.rm = T),
            cv = (des/med_t)*100 ) %>%
  arrange(desc(cv))

dados_cursos %>%
group_by(uf_busca)%>%
  summarise(
    n=n()
  )%>%
arrange(desc(n))

dados_cursos %>%
select(nome)%>%
distinct()%>%
  count()

