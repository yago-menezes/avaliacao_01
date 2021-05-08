Avaliação 01
================
Yago da Silva Menezes </br>
Introdução a Estatistica 2020.1

**Questão 01.** **a)** Nesse item utilizei o código abaixo para ler os
dados, obter a frequência das respectivas ordens e ordená-la de maneira
crescente:

``` r
dados_peixes <- read_csv("dados/brutos/peixes_rio_madeira.csv")
dados_peixes %>% 
count(ordem) %>%
arrange(n)  
```

    ## # A tibble: 12 x 2
    ##    ordem                  n
    ##    <chr>              <int>
    ##  1 Lepidosireniformes     2
    ##  2 Pleuronectiformes      2
    ##  3 Beloniformes           5
    ##  4 Não identificado      17
    ##  5 Myliobatiformes       41
    ##  6 Osteoglossiformes    433
    ##  7 Gymnotiformes        693
    ##  8 Acanthuriformes     1602
    ##  9 Cichliformes        1947
    ## 10 Clupeiformes        2821
    ## 11 Siluriformes       27451
    ## 12 Characiformes      64356

**b)** Diante do item anterior é possível verificar que a ordem do peixe
mais identificada é a do *Characiformes* com *64356* observações.

**c)** É possível verificar por **a)** que na variável ordem *17* peixes
não foram identificados.

**Questão 02.** Nessa questão utilizei o código abaixo para selecionar
as variáveis *bacia*, *ordem* e *peso\_g* , filtrar a observação “Rio
Guaporé” da variável *bacia* e depois agrupar a variável ordem, por fim
calcular a média desconsiderando os que não possuem valores do peso.
Além disso, calculou-se o desvio padrão da variável peso e a fórmula
**cv** contida na questão. Assim, gerou a tibble abaixo com todas essas
informações.

``` r
dados_peixes %>%
select(bacia, ordem, peso_g) %>%
filter(bacia == "Rio Guaporé") %>%
group_by(ordem) %>%
summarise(med_p = mean(peso_g, na.rm = T), 
            des = sd(peso_g, na.rm = T),
             cv = (des/med_p)*100 ) %>%
arrange(med_p)
```

    ## # A tibble: 7 x 4
    ##   ordem           med_p   des    cv
    ##   <chr>           <dbl> <dbl> <dbl>
    ## 1 Characiformes    65.8 106.  162. 
    ## 2 Gymnotiformes    80.6  79.8  99.0
    ## 3 Siluriformes     92.3 206.  223. 
    ## 4 Cichliformes    179.  242.  136. 
    ## 5 Acanthuriformes 198   250.  126. 
    ## 6 Clupeiformes    300.  235.   78.2
    ## 7 Myliobatiformes NaN    NA    NA

**a)** Como podemos obervar as média possuem valores distintos, então a
medida de variabilidade mais adequada é do de **coeficiente de
variação**.

**b)** Diante do código acima, com a referida tibble, é possível notar
que a ordem **Clupeiformes** é que possui a distribuição de peso mais
homogênea, pois seu coeficiente de variação é o menor.

**Questão 03.** Com o código abaixo foi possível identifcar as
observações presentes na variável *sexo*.

``` r
dados_peixes %>%
  count(sexo)
```

    ## # A tibble: 4 x 2
    ##   sexo             n
    ##   <chr>        <int>
    ## 1 fêmea            3
    ## 2 Fêmea        28328
    ## 3 Macho        21469
    ## 4 Não coletado 49570

Em seguida, utilizei o código abaixo para recodificar a variável *sexo*
para *sexo\_recod* de forma que modifique a observação apenas para
*Macho* e *Fêmea*. Além disso, esse código calcula a porcentagem
referente a cada sexo e organiza em uma tibble filtrando apenas os dois
sexos.

``` r
dados_peixes %>%
  mutate(sexo_recod = recode(
    sexo, "Fêmea" = "Fêmea",
          "fêmea" = "Fêmea",
          "Macho" = "Macho"  ))%>%
  count(sexo_recod) %>%
  summarise(sexo_recod= sexo_recod, 
            n = n,
            porc = (n/49800)*100
  )%>%
  filter(sexo_recod %in% c("Fêmea", "Macho"))
```

    ## # A tibble: 2 x 3
    ##   sexo_recod     n  porc
    ##   <chr>      <int> <dbl>
    ## 1 Fêmea      28331  56.9
    ## 2 Macho      21469  43.1

**a)** Portanto, diante da tibble acima o aumento em porcentagem que
devemos dar à quantidade de machos para que possua a mesma quantidade
referente às fêmeas é em porcentagem:

``` r
x <- 56.9-43.1
  x
```

    ## [1] 13.8

**b)** Com o código abaixo foi possível organizar de maneira decrescente
os pesos associados aos *sexo\_recod*.

``` r
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
```

    ## # A tibble: 2,283 x 2
    ## # Groups:   peso_g [1,471]
    ##    peso_g sexo_recod
    ##     <dbl> <chr>     
    ##  1  14600 Fêmea     
    ##  2  12405 Fêmea     
    ##  3  11600 Fêmea     
    ##  4  11230 Fêmea     
    ##  5  10770 Fêmea     
    ##  6  10545 Fêmea     
    ##  7  10030 Fêmea     
    ##  8   9696 Fêmea     
    ##  9   9200 Fêmea     
    ## 10   9110 Fêmea     
    ## # ... with 2,273 more rows

Portanto, o peixe do sexo **Fêmea** possui o maior peso de 14600g.

**Questão 04.** Com o código abaixo é possível filtrar e organizar o
hábito alimentar *Carnívoro* e o sexo *Macho* e contar a quantidade que
possui essas observações. Logo, a quantidade é **8552**

``` r
dados_peixes %>%
group_by(habito_alimentar, sexo)%>%
  count(habito_alimentar)%>%
summarise(habito_alimentar = habito_alimentar,
          sexo = sexo,
          quant = n)%>%
 filter(habito_alimentar == "Carnívoro" & sexo == "Macho")
```

    ## # A tibble: 1 x 3
    ## # Groups:   habito_alimentar [1]
    ##   habito_alimentar sexo  quant
    ##   <chr>            <chr> <int>
    ## 1 Carnívoro        Macho  8552

**Questão 05.** Com o código a seguir, organizei os dados de ordem
decrescente e exibir apenas o primeiro dado. Logo, o maior rendimento
líquido registrado foi de **R$ 7.267.672**.

``` r
dados_contracheque <- read_csv("dados/brutos/contracheque.csv")
```

    ## Warning: 2629 parsing failures.
    ##   row                col   expected              actual                            file
    ## 20914 data_de_publicacao date like  2018-05-11T08:53:36 'dados/brutos/contracheque.csv'
    ## 20915 data_de_publicacao date like  2018-05-11T08:53:36 'dados/brutos/contracheque.csv'
    ## 20916 data_de_publicacao date like  2018-05-11T08:53:36 'dados/brutos/contracheque.csv'
    ## 20917 data_de_publicacao date like  2018-05-11T08:53:36 'dados/brutos/contracheque.csv'
    ## 20918 data_de_publicacao date like  2018-05-11T08:53:36 'dados/brutos/contracheque.csv'
    ## ..... .................. .......... ................... ...............................
    ## See problems(...) for more details.

``` r
dados_contracheque %>%
  select(rendimento_liquido)%>%
  arrange(desc(rendimento_liquido)) %>% 
  top_n(1)
```

    ## # A tibble: 1 x 1
    ##   rendimento_liquido
    ##                <dbl>
    ## 1           7267672.

**Questão 06.** Com o código abaixo é possível filtrar os rendimentos
liquidos com valores acima de R$ 39.293,32 e contar quantos magistrados
se enquandra nessa condição.

``` r
dados_contracheque %>%
  filter(rendimento_liquido > 39293.32)%>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1 37334

Portanto, **37334** magistrados possuem rendimento liquido acima de R$
39.293,32.

**a)** De forma análoga, usando o código abaixo temos a quantidade de
magistrados que possuem redimento liquido acima de R$ 100.000,00, isto
é, **1136** magistrados.

``` r
dados_contracheque %>%
  filter(rendimento_liquido > 100000)%>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1  1136

**b)** Utilizei o código abaixo para selecionar as variáveis *tribunal*
e *rendimento\_liquido*, depois agrupar a variável tribunal, por fim
calcular a média desconsiderando os que não possuem valores do
rendimento. Além disso, calculou-se o desvio padrão da variável
rendimento\_liquido e a fórmula **cv** da Questão 02 para analisar a
variabilidade. Assim, gerou a tibble abaixo com todas essas informações
exibindo os valores do **cv** de forma decrescente.

``` r
dados_contracheque %>%
  select(tribunal, rendimento_liquido) %>%
  group_by(tribunal) %>%
  summarise(med_t = mean(rendimento_liquido, na.rm = T),
            des = sd(rendimento_liquido, na.rm = T),
            cv = (des/med_t)*100 ) %>%
  arrange(desc(cv))%>%
  top_n(5)
```

    ## # A tibble: 5 x 4
    ##   tribunal                                                 med_t     des    cv
    ##   <chr>                                                    <dbl>   <dbl> <dbl>
    ## 1 Tribunal Regional do Trabalho da 7ª Região (CE)         54659. 389453. 713. 
    ## 2 Tribunal Regional do Trabalho da 5ª Região (BA)         40914.  98192. 240. 
    ## 3 Conselho Nacional de Justiça                             6333.  10716. 169. 
    ## 4 Tribunal Superior Eleitoral                              3738.   5921. 158. 
    ## 5 Tribunal Regional do Trabalho da 2ª Região (SP capital) 28545.  27291.  95.6

Portanto, diante da informação do **cv** contida na tibble podemos
concluir que o **Tribunal Regional do Trabalho da 7ª Região (CE)**
possui a maior variabilidade, isto é, **cv = 713**.

**Questão 07.** **a)** Pelo gráfico podemos observar que o turno
**integral** possui a maior mediana das notas, pois os dados estão
expressos nos boxplots e sabemos que a mediana é o 2º quartil (o traço
horizontal dentro da caixa) do boxplot. Portanto, como a mediana de
**integral** está acima das medianas dos outros turnos então é a maior
dentre as outras.

**b)** Com o código abaixo foi possível filtrar o turno *integral* e
calcular a média e mediana da variável *nota\_integral\_ampla*
desconsiderando os valores inexistentes.

``` r
dados_cursos <- read_csv("dados/brutos/cursos-prouni.csv")
dados_cursos %>%
  filter(turno == "Integral")%>%
  summarise(media = mean(nota_integral_ampla, na.rm = T),
            mediana = median(nota_integral_ampla, na.rm = T)
      )
```

    ## # A tibble: 1 x 2
    ##   media mediana
    ##   <dbl>   <dbl>
    ## 1  663.    658.

Logo, a média é aproximadamente **663** e a mediana **658**.

**c)** Vimos na **Questão 02** que quanto menor a porcentagem do **cv**,
mais homogênea é a distribuição de uma determinada variável. Para nosso
caso, queremos saber qual dos cinco *turnos* possui menor homegeneidade
na *nota\_integral\_ampla*, ou seja, qual dos turnos possui o maior
valor do **cv**. Responderemos com o código abaixo, no qual sua
estrutura já foi detalhada em questões anteriores.

``` r
dados_cursos %>%
  select(turno, nota_integral_ampla) %>%
  group_by(turno) %>%
  summarise(med_t = mean(nota_integral_ampla, na.rm = T),
            des = sd(nota_integral_ampla, na.rm = T),
            cv = (des/med_t)*100 ) %>%
  arrange(desc(cv))
```

    ## # A tibble: 5 x 4
    ##   turno             med_t   des    cv
    ##   <chr>             <dbl> <dbl> <dbl>
    ## 1 Curso a Distância  545.  53.2  9.77
    ## 2 Integral           663.  58.0  8.75
    ## 3 Matutino           609.  43.5  7.14
    ## 4 Noturno            602.  41.2  6.85
    ## 5 Vespertino         622.  41.0  6.59

Portanto, é possível verificar na tibble que o turno **Curso a
Distância** possui menor homogeneidade na *nota\_integral\_ampla*.

**Questão 08.** Utilizei o código abaixo para agrupar a variável
*uf\_busca* e organizar a frequência absoluta de forma decrescente.

``` r
dados_cursos %>%
group_by(uf_busca)%>%
  summarise(
    n=n()
  )%>%
arrange(desc(n))
```

    ## # A tibble: 27 x 2
    ##    uf_busca     n
    ##    <chr>    <int>
    ##  1 SP       11533
    ##  2 MG        4175
    ##  3 PR        3918
    ##  4 RS        3060
    ##  5 BA        2505
    ##  6 SC        2195
    ##  7 RJ        1442
    ##  8 GO        1278
    ##  9 PA        1201
    ## 10 PE        1148
    ## # ... with 17 more rows

Assim, pode-se observar na tibble que a **Bahia** ocupa a **5ª
posição**.

**Questão 09.** Com o código abaixo selecionei a variável *nome*
identificando os cursos distintos e por fim obtendo esse total.

``` r
dados_cursos %>%
select(nome)%>%
distinct()%>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   296

Logo, foram identificados *296* cursos distintos.

**Questão 10.** Diante dos gráficos e dos estudos da estatística
descritiva, podemos notar que as represenções são simétricas tanto para
o curso de *Medicina* quanto para o de *Direito*, pois é possível
observar que não possuem valores extremos, ou seja, dados distantes da
maior concentração de amostras. Portanto, se fôssemos calcular *média* e
*mediana* de cada uma dessas distruibuições com certeza os valores
dessas duas medidas de tendência central seriam muito próximos, em cada
conjuntos de dados. Além disso, podemos também visualizar que os dois
gráficos possuem diferenças visuais, o de *Medicina* é mais “fechado” e
“acentuado” e o de *Direito* é mais “aberto” e “dispersado”. Isso
significa que há uma diferença na variabilidade desses dois conjuntos de
dados, especificamente no desvio padrão. No de *Medicina*, com certeza
há um desvio padrão menor do que o de *Direito*, pois há uma maior
homogeneidade na amostra, os dados possui valores mais próximos. Já o de
*Direito* possui uma menor homogeneidade, pois dados possui valores mais
dispersados, então a tendência é ter um desvio padrão maior.
