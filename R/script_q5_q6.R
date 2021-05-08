Script da Questão 05 e 06

library(readr)
dados_contracheque <- read_csv("avaliacao/dados/brutos/contracheque.csv")
dados_contracheque

dados_contracheque %>%
  select(rendimento_liquido)%>%
  arrange(desc(rendimento_liquido)) %>%
  top_n(1)

dados_contracheque %>%
  filter(rendimento_liquido > 39293.32)%>%
  count()

dados_contracheque %>%
  filter(rendimento_liquido > 100000)%>%
  count()

dados_contracheque %>%
  select(tribunal, rendimento_liquido) %>%
  group_by(tribunal) %>%
  summarise(med_t = mean(rendimento_liquido, na.rm = T),
            des = sd(rendimento_liquido, na.rm = T),
            cv = (des/med_t)*100 ) %>%
  arrange(desc(cv))%>%
  top_n(5)

