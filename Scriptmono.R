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
               main = 'Desvio de expectativa - ciclo anterior (lag 7) - a partir 
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
cycle("aggSerie", 0)
aggSerieMonth <- aggregate(x = base$desvio_de_expectativa, by = base[c("Mes")],
                           FUN = mean)
plot(aggSerieMonth, type='l', col='green', 
     main = 'Média do desvio da expectativa por mês', xlab = 'Mês', 
     ylab = 'Desvio de expectativa (em pontos percentuais)')
cycle("aggSerieMonth", 0)
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
cycle(Serie)
#Next Steps:
#1. Entender como calcular as ratios na p. 23 do livro usando os resultados de
#aggSerieMonth
