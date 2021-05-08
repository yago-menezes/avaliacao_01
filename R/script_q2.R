Script da questão 02


dados_peixes <- read_csv("avaliacao/dados/brutos/peixes_rio_madeira.csv")

dados_peixes %>%
select(bacia, ordem, peso_g) %>%
filter(bacia == "Rio Guaporé") %>%
group_by(ordem) %>%
summarise(med_p = mean(peso_g, na.rm = T),
            des = sd(peso_g, na.rm = T),
             cv = (des/med_p)*100 ) %>%
arrange(med_p)
