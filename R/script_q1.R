Script da questão 1

library(readr)

peixes_rio_madeira <- read_csv("dados/brutos/peixes_rio_madeira.csv")
View(peixes_rio_madeira)

peixes_rio_madeira %>%
count(ordem) %>%
arrange(n)

