library(readr)
base <- read_delim("~/Monografia/Base/Base_completa_ingles.csv", "\t", 
                   quote = "\"", col_names = TRUE, 
                   col_types = cols(`1` = col_logical(), `10` = col_logical(),
                                    `11` = col_logical(), `12` = col_logical(), 
                                    `2` = col_logical(), `2003` = col_logical(),
                                    `2004` = col_logical(),
                                    `2005` = col_logical(), 
                                    `2006` = col_logical(),
                                    `2007` = col_logical(), 
                                    `2008` = col_logical(),
                                    `2009` = col_logical(), 
                                    `2010` = col_logical(),
                                    `2011` = col_logical(), 
                                    `2012` = col_logical(),
                                    `2013` = col_logical(), 
                                    `2014` = col_logical(),
                                    `2015` = col_logical(), 
                                    `2016` = col_logical(),
                                    `2017` = col_logical(), 
                                    `2018` = col_logical(), `3` = col_logical(),
                                    `4` = col_logical(), `5` = col_logical(), 
                                    `6` = col_logical(), `7` = col_logical(),
                                    `8` = col_logical(), `9` = col_logical(), 
                                    Ano = col_integer(), 
                                    Coef._Var. = col_number(), 
                                    Data = col_date(format = "%m/%d/%Y"),
                                    Dia_semana = col_integer(), 
                                    Expectativa_ponderada = col_number(),
                                    Goldfajn = col_logical(), 
                                    Hiato = col_logical(),
                                    `IPC-S` = col_number(), 
                                    Mediana = col_number(), 
                                    Meirelles = col_logical(), 
                                    Mes = col_integer(), 
                                    Sem_acontecimentos = col_logical(),
                                    Tombini = col_logical(), 
                                    antes_copom = col_logical(), 
                                    cambio = col_number(), 
                                    desvio_de_expectativa = col_number(),
                                    semana_ata = col_logical(), 
                                    semana_copom = col_logical(),
                                    semana_pos = col_logical(),
                                    Semana_mes = col_factor(levels = c("1", "2",
                                                                       "3", "4",
                                                                       "5")), 
                                    Tipo_periodo = col_factor
                                    (levels = c("Sem acontecimentos", 
                                                "Semana antes do copom", 
                                                "Semana da Reunião do COPOM",
                                                "Semana da Ata", "Hiato",
                                                "Semana pós anúncio da Ata",
                                                "Semana Mista", "Antes e pós")),
                                    BCPresidente = col_factor
                                    (levels = c("Henrique Meirelles", 
                                                "Alexandre Tombini", 
                                                "Ilan Goldfajn")), 
                                    Mes_categ = col_factor
                                    (levels = c("Janeiro", "Fevereiro", "Março",
                                                "Abril", "Maio", "Junho",
                                                "Julho", "Agosto", "Setembro",
                                                "Outubro", "Novembro", 
                                                "Dezembro")), 
                                    Ano_categ = col_factor 
                                    (levels = c("2003", "2004", "2005", "2006",
                                                "2007", "2008", "2009", "2010",
                                                "2011", "2012", "2013", "2014",
                                                "2015", "2016", "2017", 
                                                "2018")), 
                                    `IPC-S` = col_number()), 
                   locale = locale(decimal_mark = ",", encoding = "ISO-8859-1"), 
                   trim_ws = TRUE)
#Este código importa a dataframe de maneira adequada. Função oriunda do pacote 
#Readr

Serie<-ts(base$desvio_de_expectativa, freq=365.25/7, start=c(2003,1))
#Seta como uma série de tempo, cuja variável Y é desvio de expectativa.

summary(Serie)
#Demonstra as principais estatísticas da série de tempo

plot(base$Data, base$desvio_de_expectativa,
     col='red', main = 'Desvio de expectativa - semanal e ponderada', 
     xlab = 'Período', ylab = 'Desvio de expectativa (em pontos percentuais)', 
     type='l')
#Plota gráfico em vermelho

boxplot(base$desvio_de_expectativa)
#Cria uma boxplot do desvio de expectativa

boxplot(base$desvio_de_expectativa ~ base$BCPresidente, 
        main = "Desvio de expectativa por presidente do Banco Central - em 
        boxplot", ylab = "Desvio de expectativa (em pontos percentuais)", 
        xlab = "Presidente do Banco Central",)
#Cria um boxplot do desvio de expectativa por gestão do Banco Central

boxplot(base$desvio_de_expectativa ~ base$BCPresidente, 
        main = "Desvio de expectativa por presidente do Banco Central - em 
        boxplot", ylab = "Desvio de expectativa (em pontos percentuais)", 
        xlab = "Presidente do Banco Central", col='red')
boxplot(base$desvio_de_expectativa ~ base$Ano_categ, 
        main = "Desvio de expectativa por ano - em boxplot", 
        ylab = "Desvio de expectativa (em pontos percentuais)", 
        xlab = "Ano", col='blue')
boxplot(base$desvio_de_expectativa ~ base$Mes_categ, 
        main = "Desvio de expectativa por mês - em boxplot", 
        ylab = "", xlab = "Mês", col='green')
boxplot(base$desvio_de_expectativa ~ base$Semana_mes, 
        main = "Desvio de expectativa por semana do mês - em boxplot", 
        ylab = "Desvio de expectativa (em pontos percentuais)", 
        xlab = "Semana do mês", col='red')
boxplot(base$desvio_de_expectativa ~ base$Tipo_periodo, 
        main = "Desvio por evento do COPOM em pontos percentuais", 
        ylab = "Desvio de expectativa (em pontos percentuais)", 
        xlab = "", col='blue')
boxplot(base$'IPC-S' ~ base$BCPresidente, 
        main = "Inflação semanal (IPC-S) por presidente do Banco Central - em 
        boxplot", ylab = "IPC-S (percentual)", 
        xlab = "Presidente do Banco Central", col='blue')
boxplot(base$'IBC-BR' ~ base$BCPresidente, 
        main = "Atividade econômica (IBC-BR - dessazonalizado, 2002 = 100) por 
        presidente do Banco Central - em boxplot", 
        ylab = "IBC-BR (dessazonalizado, 2002 = 100)", 
        xlab = "Presidente do Banco Central", col='red')
boxplot(base$cambio ~ base$BCPresidente, 
        main = "Câmbio (PTAX fechamento - venda) por presidente do Banco Central 
        - em boxplot", ylab = "Dólar - PTAX venda (em reais)", 
        xlab = "Presidente do Banco Central", col='green')

#Produz gráficos boxplot selecionados.

decomposedSerie <- decompose(Serie, type = c("additive", "multiplicative"), 
                             filter = NULL)
#Decompõe a série, descrevendo sua sazonalidade, tendência e erro

plot (decomposedSerie)
#Plota a decomposição, possibilitando o entendimento sobre estacionariedade
#É uma série aditiva, uma vez que não há uma aproximação com uma curva 
#exponencial dos resultados e a série tem uma amplitude consideravelmente baixa.
#Considerando que a série possui tendência e variância =/ 0, ela provavelmente 
#não é estacionária.
#A ver nos próximos testes (acf).

laggedSerie4 <- lag(Serie, -4) #Lag em mês
plot(laggedSerie, col='blue', 
     main = 'Desvio de expectativa - 4 semanas antes (1 mês ou lag 4)', 
     xlab = 'Período', 
     ylab = 'Desvio de expectativa (em pontos percentuais) 4 semanas antes da 
     data', type='l')
laggedSerie52 <- lag(Serie, -52) #Lag em ano
plot(laggedSerie52, col='green', 
     main = 'Desvio de expectativa - 52 semanas antes (1 ano ou lag 52)', 
     xlab = 'Período', 
     ylab = 'Desvio de expectativa (em pontos percentuais) 52 semanas antes da 
     data', type='l')
base2005 <- filter(base, base$Ano_categ %in% (c("2003", "2004", "2005")))
Serie2005<-ts(base2005$desvio_de_expectativa, freq=365.25/7, start=c(2003,1))
library(stats)
lagSerieA5 <- stats::lag(Serie2005, -5)
graphics::plot(lagSerieA5, col='red', 
               main = 'Desvio de expectativa - ciclo anterior (lag 5) - até 
               2005, em pontos percentuais', xlab = 'Período', 
               ylab = 'Desvio de expectativa (52 semanas antes da data)', 
               type='l')
#lag de ciclo médio do copom, ciclo tinha em média 4,75 semanas.
base2006 <- filter(base, base$Ano_categ %in% (c("2006", "2007", "2008", "2009",
                                                "2010", "2011", "2012", "2013",
                                                "2014", "2015", "2016", "2017",
                                                "2018")))
Serie2006<-ts(base2006$desvio_de_expectativa, freq=365.25/7, start=c(2003,1))
lagSerieA7 <- stats::lag(Serie2006, -7)
graphics::plot(lagSerieA7, col='blue', 
               main = 'Desvio de expectativa - ciclo anterior (lag 7) - a 'par'tir 
               de 2006, em pontos percentuais', xlab = 'Período', 
               ylab = 'Desvio de expectativa (52 semanas antes da data)', 
               type='l')
#A partir de 2006, o ciclo tem, em média, 6,5 semanas.
rlang::last_error()
rlang::last_trace()

library (DataCombine)
library(dplyr)
version
dplyr::lag(base$desvio_de_expectativa, 4)
?lag
base$lag4 <- dplyr::lag(base$desvio_de_expectativa, 4)
base$lag52 <- dplyr::lag(base$desvio_de_expectativa, 52)
SerieMedian<-ts(base$Mediana, freq=365.25/7, start=c(2003,1))
SerieVarCoef<-ts(base$Coef._Var., freq=365.25/7, start=c(2003,1))
SerieIPC<-ts(base$`IPC-S`, freq=365.25/7, start=c(2003,1))
SerieIBC<-ts(base$`IBC-BR`, freq=365.25/7, start=c(2003,1))
SerieDolar<-ts(base$cambio, freq=365.25/7, start=c(2003,1))
#Define todas as séries de tempo como tais

# Agregar a série para analisar a sazonalidade, conforme o livro Introductory
# time series with R
aggSerie <- aggregate(x = base$desvio_de_expectativa, by = base[c("Ano")],
                      FUN = mean)
plot(aggSerie, col='blue', main = 'Média do desvio da expectativa por ano', 
     xlab = 'Ano', ylab = 'Desvio de expectativa (em pontos percentuais)', 
     type='l')
aggSerieMonth <- aggregate(x = base$desvio_de_expectativa, by = base[c("Mes")],
                           FUN = mean)
plot(aggSerieMonth, type='l', col='green', 
     main = 'Média do desvio da expectativa por mês', xlab = 'Mês', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')
aggSerieMedian <- aggregate(x = base$desvio_de_expectativa, by = base[c("Ano")],
                            FUN = median)
plot(aggSerieMedian, col='blue', 
     main = 'Mediana do desvio da expectativa por ano', xlab = 'Ano', 
     ylab = 'Desvio de expectativa (em pontos percentuais)', type='l')
aggSerieMonthMedian <- aggregate(x = base$desvio_de_expectativa, 
                                 by = base[c("Mes")],FUN = median)
plot(aggSerieMonthMedian, type='l', col='green', 
     main = 'Mediana do desvio da expectativa por mês', xlab = 'Mês', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')
library(stringr)
base$Mes0 <- str_pad(base$Mes, width = 2, side = 'left', pad = '0')
base$Mesano <- paste(base$Ano,"-",base$Mes0, "-01", sep="")
base$Meseano <- as.Date(base$Mesano, "%Y-%m-%d")
aggSerieMonthYear <- aggregate(x = base$desvio_de_expectativa, 
                               by = base[c("Meseano")], FUN = mean)
class(aggSerieMonthYear$Meseano)
plot(aggSerieMonthYear, type='l', col='red', 
     main = 'Média do desvio da expectativa por período', xlab = 'Período', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')
aggSerieMonthYearMedian <- aggregate(x = base$desvio_de_expectativa, 
                                     by = base[c("Meseano")], FUN = median)
plot(aggSerieMonthYearMedian, type='l', col='red', 
     main = 'Mediana do desvio da expectativa por período', xlab = 'Período', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')
aggSeriePresident <- aggregate(base$desvio_de_expectativa, 
                               by = base[c("BCPresidente")], FUN = mean)
plot(aggSeriePresident, type='l', col='red', 
     main = 'Média do desvio da expectativa por presidente do banco central', 
     xlab = 'Presidente do Banco Central', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')
aggSeriePresidentMedian <- aggregate(base$desvio_de_expectativa, 
                               by = base[c("BCPresidente")], FUN = median)
plot(aggSeriePresidentMedian, type='l', col='red', 
     main = 'Mediana do desvio da expectativa por presidente do banco central', 
     xlab = 'Presidente do Banco Central', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')
boxplot(Serie ~ cycle (base$desvio_de_expectativa))
aggSerieEvent <- aggregate(base$desvio_de_expectativa, 
                           by = base[c("Tipo_periodo")], FUN = mean)
par(mar = c(14, 5, 3, 3))
plot(aggSerieEvent, type='l', col='red', 
     main = 'Média do desvio da expectativa por evento do COPOM',
     xlab = '', ylab = 'Desvio de expectativa (em pontos percentuais)', las=3)
aggSerieEventMedian <- aggregate(base$desvio_de_expectativa, 
                                 by = base[c("Tipo_periodo")], FUN = median)
plot(aggSerieEventMedian, type='l', col='red', 
     main = 'Mediana do desvio da expectativa por evento do COPOM',
     xlab = '', ylab = 'Desvio de expectativa (em pontos percentuais)', las=3)
aggSerieWeekMonth <- aggregate(base$desvio_de_expectativa, 
                               by = base[c("Semana_mes")], FUN = mean)
par(mar = c(5, 5, 3, 3))
plot(aggSerieWeekMonth, type='l', col='red', 
     main = 'Média do desvio da expectativa por semana do mês',
     xlab = 'Semana do mês', 
     ylab = 'Desvio de expectativa (em pontos percentuais)', las=1)
aggSerieWeekMonthMedian <- aggregate(base$desvio_de_expectativa, 
                                     by = base[c("Semana_mes")], FUN = median)
library(scales)
label_number(acuracy = 0.01)
plot(aggSerieWeekMonthMedian, type='l', col='red', 
     main = 'Mediana do desvio da expectativa por semana do mês',
     xlab = 'Semana do mês', 
     ylab = 'Desvio de expectativa (em pontos percentuais)', las=1)

#Cálculo de ratios, como na p. 23 do livro introductory time series analysis
#with R. O objetivo com o cálculo dessas taxas é entender a diferença de
#determinados períodos em relação ao resto, para identificar sazonalidades ou
#quebras de tendência (caso dos presidentes dos bancos centrais)

#Prestep: fazer o aggregate das médias anuais
layout(1:3)
plot (Serie, col='red', main = 'Desvio de expectativa - semanal e ponderada', 
      xlab = 'Período', ylab = 'Desvio de expectativa (em pontos percentuais)', 
      type='l')
plot(aggSerieMedian, col='blue', 
     main = 'Mediana do desvio da expectativa por ano', xlab = 'Ano', 
     ylab = 'Desvio de expectativa (em pontos percentuais)', type='l')
plot(aggSerie, col='green', main = 'Média do desvio da expectativa por ano', 
     xlab = 'Ano', ylab = 'Desvio de expectativa (em pontos percentuais)', 
     type='l')

plot (Serie, col='red', main = 'Desvio de expectativa - semanal e ponderada', 
      xlab = 'Período', ylab = 'Desvio de expectativa (em pontos percentuais)', 
      type='l')
plot(aggSerieMonth, type='l', col='blue', 
     main = 'Média do desvio da expectativa por mês', xlab = 'Mês', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')
plot(aggSerieMonthMedian, type='l', col='green', 
     main = 'Mediana do desvio da expectativa por mês', xlab = 'Mês', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')

plot (Serie, col='red', main = 'Desvio de expectativa - semanal e ponderada', 
      xlab = 'Período', ylab = 'Desvio de expectativa (em pontos percentuais)', 
      type='l')
plot(aggSerieMonthYear, type='l', col='blue', 
     main = 'Média do desvio da expectativa por período', xlab = 'Período', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')
plot(aggSerieMonthYearMedian, type='l', col='green', 
     main = 'Mediana do desvio da expectativa por período', xlab = 'Período', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')

plot (Serie, col='red', main = 'Desvio de expectativa - semanal e ponderada', 
      xlab = 'Período', ylab = 'Desvio de expectativa (em pontos percentuais)', 
      type='l')
plot(aggSeriePresident, type='l', col='red', 
     main = 'Média do desvio da expectativa por presidente do banco central', 
     xlab = 'Presidente do Banco Central', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')
plot(aggSeriePresidentMedian, type='l', col='red', 
     main = 'Mediana do desvio da expectativa por presidente do banco central', 
     xlab = 'Presidente do Banco Central', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')

plot (Serie, col='red', main = 'Desvio de expectativa - semanal e ponderada', 
      xlab = 'Período', ylab = 'Desvio de expectativa (em pontos percentuais)', 
      type='l')
plot(aggSerieEvent, type='l', col='red', 
     main = 'Média do desvio da expectativa por evento do COPOM',
     xlab = '', ylab = 'Desvio de expectativa (em pontos percentuais)', las=0)
plot(aggSerieEventMedian, type='l', col='red', 
     main = 'Mediana do desvio da expectativa por evento do COPOM',
     xlab = '', ylab = 'Desvio de expectativa (em pontos percentuais)', las=3)

#Montando as ratios para fazer análises
#1. Caso das taxas para presidentes dos bancos centrais.
#2. Caso das taxas para eventos do copom

Sem.acontecimentos <- filter(.data = base, Tipo_periodo == "Sem acontecimentos",
                             .preserve = TRUE)
View(Sem.acontecimentos)
Serie.sem.acontecimentos <- ts(Sem.acontecimentos$desvio_de_expectativa, 
                               start = c(2003,1))
summary(Serie.sem.acontecimentos)
class(Serie.sem.acontecimentos)
Sem.acontecimentos.ratio <- 
  abs(median(Serie.sem.acontecimentos))/abs(median(Serie))
Sem.acontecimentos.ratio
percentual.sem.acontecimentos <- (Sem.acontecimentos.ratio-1)*100
percentual.sem.acontecimentos

Antes.COPOM <- filter(.data = base, Tipo_periodo == "Semana antes do copom",
                             .preserve = TRUE)
View(Antes.COPOM)
Serie.antes.COPOM <- ts(Antes.COPOM$desvio_de_expectativa, start = c(2003,1))
summary(Serie.antes.COPOM)
class(Serie.antes.COPOM)
Antes.COPOM.ratio <- abs(median(Serie.antes.COPOM))/abs(median(Serie))
Antes.COPOM.ratio
percentual.antes.COPOM <- (Antes.COPOM.ratio-1)*100
percentual.antes.COPOM

Semana.COPOM <- filter(.data = base, 
                       Tipo_periodo == "Semana da Reunião do COPOM",
                       .preserve = TRUE)
View(Semana.COPOM)
Serie.semana.COPOM <- ts(Semana.COPOM$desvio_de_expectativa, start = c(2003,1))
summary(Serie.semana.COPOM)
class(Serie.semana.COPOM)
Semana.COPOM.ratio <- abs(median(Serie.semana.COPOM))/abs(median(Serie))
Semana.COPOM.ratio
percentual.semana.COPOM <- (Semana.COPOM.ratio-1)*100
percentual.semana.COPOM

Semana.ata <- filter(.data = base, Tipo_periodo == "Semana da Ata", 
                     .preserve = TRUE)
View(Semana.ata)
Serie.semana.ata <- ts(Semana.ata$desvio_de_expectativa, start = c(2003,1))
summary(Serie.semana.ata)
class(Serie.semana.ata)
Semana.ata.ratio <- abs(median(Serie.semana.ata))/abs(median(Serie))
Semana.ata.ratio
percentual.semana.ata <- (Semana.ata.ratio-1)*100
percentual.semana.ata

Depois.ata <- filter(.data = base, Tipo_periodo == "Semana pós anúncio da Ata", 
                     .preserve = TRUE)
View(Depois.ata)
Serie.depois.ata <- ts(Depois.ata$desvio_de_expectativa, start = c(2003,1))
summary(Serie.depois.ata)
class(Serie.depois.ata)
Depois.ata.ratio <- abs(median(Serie.depois.ata))/abs(median(Serie))
Depois.ata.ratio
percentual.depois.ata <- (Depois.ata.ratio-1)*100
percentual.depois.ata

Henrique.Meirelles <- filter(.data = base, BCPresidente == "Henrique Meirelles", 
                     .preserve = TRUE)
View(Henrique.Meirelles)
Serie.Henrique.Meirelles <- ts(Henrique.Meirelles$desvio_de_expectativa, 
                               start = c(2003,1))
summary(Serie.Henrique.Meirelles)
class(Serie.Henrique.Meirelles)
Henrique.Meirelles.ratio <- 
  abs(median(Serie.Henrique.Meirelles))/abs(median(Serie))
Henrique.Meirelles.ratio
percentual.Henrique.Meirelles <- (Henrique.Meirelles.ratio-1)*100
percentual.Henrique.Meirelles

Alexandre.Tombini <- filter(.data = base, BCPresidente == "Alexandre Tombini", 
                             .preserve = TRUE)
View(Alexandre.Tombini)
Serie.Alexandre.Tombini <- ts(Alexandre.Tombini$desvio_de_expectativa,
                              start = c(2003,1))
summary(Serie.Alexandre.Tombini)
class(Serie.Alexandre.Tombini)
Alexandre.Tombini.ratio <- 
  abs(median(Serie.Alexandre.Tombini))/abs(median(Serie))
Alexandre.Tombini.ratio
percentual.Alexandre.Tombini <- (Alexandre.Tombini.ratio-1)*100
percentual.Alexandre.Tombini

Ilan.Goldfajn <- filter(.data = base, BCPresidente == "Ilan Goldfajn", 
                            .preserve = TRUE)
View(Ilan.Goldfajn)
Serie.Ilan.Goldfajn <- ts(Ilan.Goldfajn$desvio_de_expectativa, 
                          start = c(2003,1))
summary(Serie.Ilan.Goldfajn)
class(Serie.Ilan.Goldfajn)
Ilan.Goldfajn.ratio <- abs(median(Serie.Ilan.Goldfajn))/abs(median(Serie))
Ilan.Goldfajn.ratio
percentual.Ilan.Goldfajn <- (Ilan.Goldfajn.ratio-1)*100
percentual.Ilan.Goldfajn

Henrique.Meirelles.sem.acontecimentos <- filter(.data = base, 
                                                BCPresidente == 
                                                  "Henrique Meirelles", 
                                                Tipo_periodo == 
                                                  "Sem acontecimentos", 
                                                .preserve = TRUE)
View(Henrique.Meirelles.sem.acontecimentos)
Serie.Henrique.Meirelles.sem.acontecimentos <- 
  ts(Henrique.Meirelles.sem.acontecimentos$desvio_de_expectativa, 
     start = c(2003,1))
summary(Serie.Henrique.Meirelles.sem.acontecimentos)
class(Serie.Henrique.Meirelles.sem.acontecimentos)
Henrique.Meirelles.sem.acontecimentos.ratio <- 
  abs(median(Serie.Henrique.Meirelles.sem.acontecimentos))/
  abs(median(Serie.Henrique.Meirelles))
Henrique.Meirelles.sem.acontecimentos.ratio
percentual.Henrique.Meirelles.sem.acontecimentos <- 
  (Henrique.Meirelles.sem.acontecimentos.ratio-1)*100
percentual.Henrique.Meirelles.sem.acontecimentos

Henrique.Meirelles.antes.COPOM <- filter(.data = base, 
                                         BCPresidente == "Henrique Meirelles",
                                         Tipo_periodo == 
                                           "Semana antes do copom",
                                         .preserve = TRUE)
View(Henrique.Meirelles.antes.COPOM)
Serie.Henrique.Meirelles.antes.COPOM <- 
  ts(Henrique.Meirelles.antes.COPOM$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Henrique.Meirelles.antes.COPOM)
class(Serie.Henrique.Meirelles.antes.COPOM)
Henrique.Meirelles.antes.COPOM.ratio <- 
  abs(median(Serie.Henrique.Meirelles.antes.COPOM))/
  abs(median(Serie.Henrique.Meirelles))
Henrique.Meirelles.antes.COPOM.ratio
percentual.Henrique.Meirelles.antes.COPOM <- 
  (Henrique.Meirelles.antes.COPOM.ratio-1)*100
percentual.Henrique.Meirelles.antes.COPOM

Henrique.Meirelles.semana.COPOM <- filter(.data = base, 
                                          BCPresidente == "Henrique Meirelles",
                                          Tipo_periodo == 
                                            "Semana da Reunião do COPOM",
                                          .preserve = TRUE)
View(Henrique.Meirelles.semana.COPOM)
Serie.Henrique.Meirelles.semana.COPOM <- 
  ts(Henrique.Meirelles.semana.COPOM$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Henrique.Meirelles.semana.COPOM)
class(Serie.Henrique.Meirelles.semana.COPOM)
Henrique.Meirelles.semana.COPOM.ratio <- 
  abs(median(Serie.Henrique.Meirelles.semana.COPOM))/
  abs(median(Serie.Henrique.Meirelles))
Henrique.Meirelles.semana.COPOM.ratio
percentual.Henrique.Meirelles.semana.COPOM <- 
  (Henrique.Meirelles.semana.COPOM.ratio-1)*100
percentual.Henrique.Meirelles.semana.COPOM

Henrique.Meirelles.semana.ata <- filter(.data = base, 
                                        BCPresidente == "Henrique Meirelles",
                                        Tipo_periodo == "Semana da Ata",
                                        .preserve = TRUE)
View(Henrique.Meirelles.semana.ata)
Serie.Henrique.Meirelles.semana.ata <- 
  ts(Henrique.Meirelles.semana.ata$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Henrique.Meirelles.semana.ata)
class(Serie.Henrique.Meirelles.semana.ata)
Henrique.Meirelles.semana.ata.ratio <- 
  abs(median(Serie.Henrique.Meirelles.semana.ata))/
  abs(median(Serie.Henrique.Meirelles))
Henrique.Meirelles.semana.ata.ratio
percentual.Henrique.Meirelles.semana.ata <- 
  (Henrique.Meirelles.semana.ata.ratio-1)*100
percentual.Henrique.Meirelles.semana.ata

Henrique.Meirelles.depois.ata <- filter(.data = base, 
                                        BCPresidente == "Henrique Meirelles",
                                        Tipo_periodo == 
                                          "Semana pós anúncio da Ata",
                                        .preserve = TRUE)
View(Henrique.Meirelles.depois.ata)
Serie.Henrique.Meirelles.depois.ata <- 
  ts(Henrique.Meirelles.depois.ata$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Henrique.Meirelles.depois.ata)
class(Serie.Henrique.Meirelles.depois.ata)
Henrique.Meirelles.depois.ata.ratio <- 
  abs(median(Serie.Henrique.Meirelles.depois.ata))/
  abs(median(Serie.Henrique.Meirelles))
Henrique.Meirelles.depois.ata.ratio
percentual.Henrique.Meirelles.depois.ata <- 
  (Henrique.Meirelles.depois.ata.ratio-1)*100
percentual.Henrique.Meirelles.depois.ata

Alexandre.Tombini.sem.acontecimentos <- filter(.data = base, 
                                              BCPresidente == 
                                                "Alexandre Tombini", 
                                              Tipo_periodo == 
                                                "Sem acontecimentos", 
                                              .preserve = TRUE)
View(Alexandre.Tombini.sem.acontecimentos)
Serie.Alexandre.Tombini.sem.acontecimentos <- 
  ts(Alexandre.Tombini.sem.acontecimentos$desvio_de_expectativa, 
     start = c(2003,1))
summary(Serie.Alexandre.Tombini.sem.acontecimentos)
class(Serie.Alexandre.Tombini.sem.acontecimentos)
Alexandre.Tombini.sem.acontecimentos.ratio <- 
  abs(median(Serie.Alexandre.Tombini.sem.acontecimentos))/
  abs(median(Serie.Alexandre.Tombini))
Alexandre.Tombini.sem.acontecimentos.ratio
percentual.Alexandre.Tombini.sem.acontecimentos <- 
  (Alexandre.Tombini.sem.acontecimentos.ratio-1)*100
percentual.Alexandre.Tombini.sem.acontecimentos

Alexandre.Tombini.antes.COPOM <- filter(.data = base, 
                                        BCPresidente == "Alexandre Tombini", 
                                        Tipo_periodo == "Semana antes do copom", 
                                        .preserve = TRUE)
View(Alexandre.Tombini.antes.COPOM)
Serie.Alexandre.Tombini.antes.COPOM <- 
  ts(Alexandre.Tombini.antes.COPOM$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Alexandre.Tombini.antes.COPOM)
class(Serie.Alexandre.Tombini.antes.COPOM)
Alexandre.Tombini.antes.COPOM.ratio <- 
  abs(median(Serie.Alexandre.Tombini.antes.COPOM))/
  abs(median(Serie.Alexandre.Tombini))
Alexandre.Tombini.antes.COPOM.ratio


Alexandre.Tombini.semana.COPOM <- filter(.data = base, 
                                         BCPresidente == "Alexandre Tombini", 
                                         Tipo_periodo == 
                                           "Semana da Reunião do COPOM", 
                                         .preserve = TRUE)
View(Alexandre.Tombini.semana.COPOM)
Serie.Alexandre.Tombini.semana.COPOM <- 
  ts(Alexandre.Tombini.semana.COPOM$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Alexandre.Tombini.semana.COPOM)
class(Serie.Alexandre.Tombini.semana.COPOM)
Alexandre.Tombini.semana.COPOM.ratio <- 
  abs(mean(Serie.Alexandre.Tombini.semana.COPOM))/
  abs(mean(Serie.Alexandre.Tombini))
Alexandre.Tombini.semana.COPOM.ratio

Alexandre.Tombini.semana.ata <- filter(.data = base, 
                                       BCPresidente == "Alexandre Tombini", 
                                       Tipo_periodo == "Semana da Ata", 
                                       .preserve = TRUE)
View(Alexandre.Tombini.semana.ata)
Serie.Alexandre.Tombini.semana.ata <- 
  ts(Alexandre.Tombini.semana.ata$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Alexandre.Tombini.semana.ata)
class(Serie.Alexandre.Tombini.semana.ata)
Alexandre.Tombini.semana.ata.ratio <- 
  abs(mean(Serie.Alexandre.Tombini.semana.ata))/
  abs(mean(Serie.Alexandre.Tombini))
Alexandre.Tombini.semana.ata.ratio

Alexandre.Tombini.depois.ata <- filter(.data = base, 
                                       BCPresidente == "Alexandre Tombini", 
                                       Tipo_periodo == 
                                         "Semana pós anúncio da Ata", 
                                       .preserve = TRUE)
View(Alexandre.Tombini.depois.ata)
Serie.Alexandre.Tombini.depois.ata <- 
  ts(Alexandre.Tombini.depois.ata$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Alexandre.Tombini.depois.ata)
class(Serie.Alexandre.Tombini.depois.ata)
Alexandre.Tombini.depois.ata.ratio <- 
  abs(mean(Serie.Alexandre.Tombini.depois.ata))/
  abs(mean(Serie.Alexandre.Tombini))
Alexandre.Tombini.depois.ata.ratio

Ilan.Goldfajn.sem.acontecimentos <- filter(.data = base, 
                                           BCPresidente == "Ilan Goldfajn", 
                                           Tipo_periodo == "Sem acontecimentos", 
                                           .preserve = TRUE)
View(Ilan.Goldfajn.sem.acontecimentos)
Serie.Ilan.Goldfajn.sem.acontecimentos <- 
  ts(Ilan.Goldfajn.sem.acontecimentos$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Ilan.Goldfajn.sem.acontecimentos)
class(Serie.Ilan.Goldfajn.sem.acontecimentos)
Ilan.Goldfajn.sem.acontecimentos.ratio <- 
  abs(mean(Serie.Ilan.Goldfajn.sem.acontecimentos))/
  abs(mean(Serie.Ilan.Goldfajn))
Ilan.Goldfajn.sem.acontecimentos.ratio

Ilan.Goldfajn.antes.COPOM <- filter(.data = base, 
                                    BCPresidente == "Ilan Goldfajn", 
                                    Tipo_periodo == "Semana antes do copom", 
                                    .preserve = TRUE)
View(Ilan.Goldfajn.antes.COPOM)
Serie.Ilan.Goldfajn.antes.COPOM <- 
  ts(Ilan.Goldfajn.antes.COPOM$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Ilan.Goldfajn.antes.COPOM)
class(Serie.Ilan.Goldfajn.antes.COPOM)
Ilan.Goldfajn.antes.COPOM.ratio <- 
  abs(mean(Serie.Ilan.Goldfajn.antes.COPOM))/abs(mean(Serie.Ilan.Goldfajn))
Ilan.Goldfajn.antes.COPOM.ratio

Ilan.Goldfajn.semana.COPOM <- filter(.data = base, 
                                     BCPresidente == "Ilan Goldfajn", 
                                     Tipo_periodo == 
                                       "Semana da Reunião do COPOM", 
                                     .preserve = TRUE)
View(Ilan.Goldfajn.semana.COPOM)
Serie.Ilan.Goldfajn.semana.COPOM <- 
  ts(Ilan.Goldfajn.semana.COPOM$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Ilan.Goldfajn.semana.COPOM)
class(Serie.Ilan.Goldfajn.semana.COPOM)
Ilan.Goldfajn.semana.COPOM.ratio <- 
  abs(mean(Serie.Ilan.Goldfajn.semana.COPOM))/abs(mean(Serie.Ilan.Goldfajn))
Ilan.Goldfajn.semana.COPOM.ratio

Ilan.Goldfajn.semana.ata <- filter(.data = base, 
                                   BCPresidente == "Ilan Goldfajn", 
                                   Tipo_periodo == "Semana da Ata", 
                                   .preserve = TRUE)
View(Ilan.Goldfajn.semana.ata)
Serie.Ilan.Goldfajn.semana.ata <- 
  ts(Ilan.Goldfajn.semana.ata$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Ilan.Goldfajn.semana.ata)
class(Serie.Ilan.Goldfajn.semana.ata)
Ilan.Goldfajn.semana.ata.ratio <- 
  abs(mean(Serie.Ilan.Goldfajn.semana.ata))/abs(mean(Serie.Ilan.Goldfajn))
Ilan.Goldfajn.semana.ata.ratio

Ilan.Goldfajn.depois.ata <- filter(.data = base, 
                                   BCPresidente == "Ilan Goldfajn", 
                                   Tipo_periodo == "Semana pós anúncio da Ata", 
                                   .preserve = TRUE)
View(Ilan.Goldfajn.depois.ata)
Serie.Ilan.Goldfajn.depois.ata <- 
  ts(Ilan.Goldfajn.depois.ata$desvio_de_expectativa, start = c(2003,1))
summary(Serie.Ilan.Goldfajn.depois.ata)
class(Serie.Ilan.Goldfajn.depois.ata)
Ilan.Goldfajn.depois.ata.ratio <- 
  abs(mean(Serie.Ilan.Goldfajn.depois.ata))/abs(mean(Serie.Ilan.Goldfajn))
Ilan.Goldfajn.depois.ata.ratio

#Análise de intersecção das séries, para considerar o cenário nas análises:

dolar.expec <- ts.intersect(Serie, SerieDolar)
start(dolar.expec)
end(dolar.expec)
dolar.expec[1:3,]
expec <- dolar.expec[,1]; dolar <- dolar.expec[,2]
par("mar")
par(mar=c(1,1,1,1))
layout(1:2)
plot(expec, main = "", ylab = "desvio de expectativa - em pontos percentuais")
plot(dolar, main = "", ylab = "dólar - em reais")
layout(1:1)
par(mar=c(5,5,1,1))
plot(as.vector(expec), as.vector(dolar), 
     xlab = "Desvio de expectativa - em pontos percentuais", 
     ylab = "Dólar - em reais")
abline(reg = lm(dolar~expec))
cor(expec, dolar)

ipc.expec <- ts.intersect(Serie, SerieIPC)
start(ipc.expec)
end(ipc.expec)
ipc.expec[1:3,]
expec2 <- ipc.expec[,1]; ipcs <- ipc.expec[,2]
plot(expec2, main = "", ylab = "desvio de expectativa - em pontos percentuais")
plot(ipcs, main = "", ylab = "ipc-s - em percentual")
plot(as.vector(expec2), as.vector(ipcs), 
     xlab = "Desvio de expectativa - em pontos percentuais", 
     ylab = "IPC-S - em percentual")
abline(reg = lm(ipcs~expec2))
cor(expec2, ipcs)

ibc.expec <- ts.intersect(Serie, SerieIBC)
start(ibc.expec)
end(ibc.expec)
ibc.expec[1:3,]
expec3 <- ibc.expec[,1]; ibcbr <- ibc.expec[,2]
plot(expec3, main = "", ylab = "desvio de expectativa - em pontos percentuais")
plot(ibcbr, main = "", ylab = "IBC-BR (pib 2002 = 100)")
plot(SerieIBC)
plot(as.vector(expec3), as.vector(ibcbr), 
     xlab = "Desvio de expectativa - em pontos percentuais", 
     ylab = "IBC-BR (2002 = 100)")
abline(reg = lm(ibcbr~expec3))
cor(expec3, ibcbr)

#Decompondo a série para analisar
decomposedSerie <- decompose(Serie, type = c("additive", "multiplicative"), 
                             filter = NULL)
plot(decomposedSerie)
TrendSerie <- decomposedSerie$trend
SeasonalSerie <- decomposedSerie$seasonal
LegendNames <- c(TrendSerie, SeasonalSerie)
ts.plot(cbind(TrendSerie, TrendSerie*SeasonalSerie), lty = 1:2, 
        ylab = "Desvio de expectativa - em pontos percentuais", 
        col = c("red", "blue"))
legend("bottomleft", c("Trend", "Seasonal"), col = c("red", "blue"), lty = 1:2)
cov(expec, dolar)
cov(expec2, ipcs)
cov(expec3, ibcbr)
acf(Serie)$acf[6]
acf(Serie, lag.max = NULL)
plot(Serie[1:832],Serie[2:833], xlab = "x_t", ylab = "x_t+1") 
plot(Serie[1:823],Serie[11:833], xlab = "x_t", ylab = "x_t+1 - lag 10") 
plot(Serie[1:783],Serie[51:833], xlab = "x_t", ylab = "x_t+1 - lag 50") 
plot(Serie[1:733],Serie[101:833], xlab = "x_t", ylab = "x_t+1 - lag 100") 
acf(Serie, type = c("covariance"))$acf[2]
acf(Serie, type = c("covariance"))$acf[10]
acf(Serie, type = c("covariance"))$acf[20]
acf(Serie, type = c("covariance"))$acf[30]
acf(Serie, type = c("covariance"))$acf[5]
acf(Serie, type = c("covariance"))$acf[6]
acf(Serie, lag.max = 833, demean = TRUE)
ts.plot(Serie, SerieIPC, lty = c(1,3), 
        ylab = "Linha contínua: desvio da expectativa de inflação 
        Linha pontilhada: IPC-S (último mês)")
ts.plot(Serie, SerieDolar, lty = c(1,3), 
        ylab = "Linha contínua: desvio da expectativa de inflação 
        Linha pontilhada: Dólar (PTAX fechamento dia)")
ts.plot(Serie, SerieIBC, lty = c(1,3), 
        ylab = "Linha contínua: desvio da expectativa de inflação 
        Linha pontilhada: IBC-BR (mensal)")
#Transformação da série do ibc-br para que seja considerada apenas a diferença 
#do valor apurado em relação ao valor de referência, por isso subtraimos 100.
#Para que a transformação em log seja possível, somamos 1 para que não haja
#números negativos. Assim, chegamos à conclusão que precisamos subtrair 99,
#sendo que a série resultante é log(xt+1), onde x é o IBC-BR naquele momento e t
#se refere ao período de observação.
SerieIBCplotavel <- SerieIBC-99
SerieExponentialIBC <- log(SerieIBCplotavel)
SerieIBCplotavel
SerieExponentialIBC
par(mar = c(5,7,5,1))
ts.plot(Serie, SerieExponentialIBC, lty = c(1,3), 
        ylab = "Linha contínua: desvio da expectativa de inflação 
        Linha pontilhada: log(IBC-BR + 1) (mensal - em diferença em relação à 
        atividade em 2000)")
SerieExponentialIBCcomplete <- log(SerieIBC)
ts.plot(Serie, SerieExponentialIBCcomplete, lty = c(1,3), 
        ylab = "Linha contínua: desvio da expectativa de inflação 
        Linha pontilhada: log(IBC-BR) (mensal - valores em 2000 = 100)")
plot(SerieExponentialIBCcomplete)
acf(ts.union(Serie, SerieDolar))
acf(ts.union(Serie, SerieIPC))
acf(ts.union(Serie, SerieExponentialIBCcomplete))
randomSerie <- decompose(Serie)$random
randomSerieDolar <- decompose(SerieDolar)$random
acf (ts.union(randomSerie, randomSerieDolar), na.action = na.pass)
randomSerieIPC <- decompose(SerieIPC)$random
acf (ts.union(randomSerie, randomSerieIPC), na.action = na.pass)
randomSerieIBCComplete <- decompose(SerieExponentialIBCcomplete)$random
acf (ts.union(randomSerie, randomSerieIBCComplete), na.action = na.pass)

#2. Analisar correlação entre as séries considerando apenas random
cor(randomSerie, randomSerieDolar, use = "pairwise.complete.obs")
cor(randomSerie, randomSerieIPC, use = "pairwise.complete.obs")
cor(randomSerie, randomSerieIBCComplete, use = "pairwise.complete.obs")
cor(Serie, SerieExponentialIBCcomplete)
cov(Serie, SerieExponentialIBCcomplete)

print(acf(ts.union(randomSerie, randomSerieDolar), na.action = na.pass))
print(acf(ts.union(randomSerie, randomSerieIPC), na.action = na.pass))
print(acf(ts.union(randomSerie, randomSerieIBCComplete), na.action = na.pass))

#HoltWinters - suavização exponencial faz sentido porque a série, num primeiro 
#momento, parece ter tendência e sazonalidade indefinidos
Serie.hw1 <- HoltWinters(Serie, beta = 0, gamma = 0)
summary(Serie)
frequency(Serie)
