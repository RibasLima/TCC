Verificar inputs da pesquisa Focus (quinta ou sexta) – o input é na sexta. https://www.bcb.gov.br/publicacoes/focus
Verificar M+0 ou M+1
Construir série de erro semanal junto com o professor
Série de eventos semanais
Mediana acumulada prox 12 meses 

Enviar Minha mono atualmente

Observações sobre ciclos e sazonalidade: boxplots.
Boxplot do mês: Média em abril consideravelmente mais alta e em março consideravelmente mais baixa. No entanto, o corpo do candle parece estar com parâmetros bem parecidos entre os diferentes meses. Me inclino a dizer que não há sazonalidade com base nos meses, por causa do corpo do candle. Mas não consigo bater o martelo, por causa da média.

Boxplot do ano: nenhuma tendência identificada.

Boxplot do evento: a tendência é a mesma com leves mudanças de tendência, podendo ser causadas por tamanho da amostra. Nada a se prestar muita atenção – mas pode comprometer a hipótese do estudo.

Boxplot do presidente: claramente há uma mudança no patamar de desvio de expectativa do Tombini.

Boxplot da semana do mês: tudo muito igual, definitivamente nenhuma sazonalidade aqui.


Gráfico da decomposição: se por um lado a sazonalidade é quase irrelevante na série, por outro lado, há um componente de tendência, que inclusive varia com o tempo. A variância oscila, mas não o suficiente para dizer que ela cresce ao infinito. Assim, por causa da tendência, a série é não estacionária.

Observações sobre ciclos e sazonalidade: gráficos triplos de comparação.
Comparação mensal: aqui, com a escala mais clara, podemos ver que não há sazonalidade ou, se há, ela é irrelevante, corroborando o que foi visto no gráfico de decomposição.
Comparação anual: nenhuma sazonalidade identificada. Entretanto, como funciona como uma suavização da curva, podemos ver o quanto o componente mais forte da curva é a tendência.
Comparação mensal e anual: mesmas conclusões da anual, curva menos suavizada e com maior presença do erro.
Caso dos eventos: nenhuma particularidade identificada. Analisar impacto na tese.
Comparação de presidentes: Analisar tendência. Houve mudança? Aparentemente, sim, uma vez que no Meirelles cai, no Tombini sobe e no Goldfajn cai novamente. Entender o que houve nos idos de 2004 e 2005… por que sobe?

Ratios: conforme p. 9 do Copertwait & Metcalfe. Considerando médias em módulo, uma vez que o objetivo é obter o desvio médio de expectativa, e que 0 = desvio inexistente.
Eventos:
Sem acontecimentos: 0.9260047
Semana antes do COPOM: 1.058989
Semana da reunião do COPOM: 1.038477
Semana da ata: 1.01364
Semana pós anúncio da ata: 1.049312
Nenhuma variação acima de 8%. Alguma movimentação a mais em semanas com eventos do copom, especialmente antes do copom e depois da ata, mas nada demais.

Presidentes:
Meirelles: 0.492374 (menor do que a média)
Tombini: 2.266493 (puxa a média pra cima)
Goldfajn: 0.07238074 (mesmo desconsiderando o fato de que o desvio de expectativa na era Goldfajn é negativo, ele ainda apresenta média menor que a do histórico completo de 2003-2018, ainda que superior à média do período Meirelles) 
Grandes variações. Podemos ver que as percepções do mercado variaram muito conforme o presidente do BC.
Henrique Meirelles (média do evento apenas no presidente em relação à média do presidente):
Sem acontecimentos: 0.3167281 (a média do desvio de expectativa é muito menor em relação à média total do Meirelles)
Semana antes do COPOM:  1.2813 (desvio razoável, mercado se movimentando mais?)
Semana da reunião do COPOM: 1.206975 (desvio razoável, mercado ainda se movimentando, porém menos que nas semanas anteriores)
Semana da ata: 1.195411 (continua diminuindo)
Semana pós anúncio da ata: 1.366423 (ué, o mais alto de todos?)
Alexandre Tombini (média do evento apenas no presidente em relação à média do presidente):
Sem acontecimentos: 0.9978639 (estável)
Semana antes do COPOM:  0.9996006 (estável)
Semana da reunião do COPOM:  1.00888 (estável)
Semana da ata: 1.003622 (estável)
Semana pós anúncio da ata: 0.9932708 (estável, novamente)
Ilan Goldfajn (média do evento apenas no presidente em relação à média do presidente):
Sem acontecimentos: 2.085187 (a média de Goldfajn era de -0.0452, enquanto que exclusivamente em semanas sem acontecimentos a sua média era de -0.09425 – ou seja, mais distante do 0, consideravelmente, indicando um mercado que acreditava ainda mais em quedas bruscas da inflação do que em dias normais de COPOM, ainda que levemente)
Semana antes do COPOM:  0.1594799  (-0.007208 de média em semanas antes do copom, ou seja, mais próximo do 0, quase 0 de desvio de expectativa na verdade)
Semana da reunião do COPOM: 0.9552114 (média de 0.04317, então na verdade a média na semana da reunião do COPOM é mais alta que no tempo completo, porém tem a mesma magnitude na direção oposta)
Semana da ata: 0.6979715 (-0.03155 de média, portanto, mais próxima de 0 que a média total)
Semana pós anúncio da ata: 1.422282 (-0.06429 de média, portanto mais distante de 0)
Conclusão: a forma como o mercado reagia aos comunicados parece variar conforme o presidente do banco central. Análises mais profundas são requeridas, porém podemos notar que o mercado parece mais reativo às comunicações na era Meirelles e menos reativo às comunicações na era Tombini. Os dados da era Ilan apontam em maiores movimentações do mercado no período sem acontecimentos, ainda que o desvio de expectativa, mesmo neste momento, seja, em média, pequeno.
Correlações:
Expectativa x dólar: 0.200621. Há uma leve correlação positiva entre o dólar e a expectativa de inflação – confesso que esperava uma correlação mais alta.
Expectativa x inflação: 0.5206881. Há uma correlação mais intensa, o que faz sentido considerando a teoria econômica sobre como os agentes modelam suas expectativas de inflação.
Expectativa x produto (IBC-BR): 0.176251. Também mais leve do que imaginava.
Mais análises da decomposição da série. Lembrando que estamos tratando-a como uma série aditiva, uma vez que o formato do gráfico, que não tem grandes explosões ou mudanças de nível, como séries de PIB por exemplo, aponta para uma série aditiva.
Covariâncias:
Expectativa x dólar: 0.1237232.
Expectativa x inflação corrente: 0.1791453.
Expectativa x produto: 0.01811741. 
Autocorrelação:
lag 1: 0.9713012
lag 4: 0.9285735
lag 5: 0.9103872
lag 9: 0.8353702
lag 19: 0.6788553
lag 29: 0.5522301
Ainda que a correlação vá diminuindo, ainda é bem mais alta que a que se vê no lag 1 do livro (0.47). Seria isso um objetivo à estacionariedade? Além disso, por que o R não vai além do lag 29? Isso aponta, para todos os efeitos, que existe uma correlação bastante elevada na série, possivelmente maior autocorrelação do que correlação com outras variáveis. Também não vemos correlação sazonal para as semanas. Não conseguimos analisar para ano por não termos lag acima de 29, a ver.
Autocovariância:
lag 1: 0.7996119
lag 4: 0.7644368
lag 5: 0.7494652
lag 9: 0.6877083
lag 19: 0.5588594
lag 29: 0.4546167
Correlograma: como podemos ver, mesmo olhando para o correlograma no lag 833 (completo), mais de 5% da série fica fora do intervalo para ROk. Considerando que, para que uma série seja estacionária, é necessário que a média seja constante e que a correlação fique dentro de um mesmo intervalo, sendo que a correlação entre um ponto t1 e t2 não dependa de nada além da distância entre tais pontos, temos mais indícios de que a série não é estacionária. Vale lembrar que a série tem um componente de tendência muito mais forte do que o componente de sazonalidade, e que séries com tal característica, em geral, costumam apresentar correlograma que decai lentamente e quase linearmente.
Correlações - random:
Expectativa x dólar: 0.2183151. A correlação aumenta quando tiramos impactos de tendência e sazonalidade, mas o aumento é muito pequeno.
Expectativa x inflação: 0.4415533. A correlação diminui quando tiramos impacto de tendência e sazonalidade. Estranho, tentar entender porque.
Expectativa x produto (IBC-BR): 0.3068323. Correlação aumenta consideravelmente quando desconsideramos a tendência e a sazonalidade. Entender porque.
Ver: índice de correlação cruzada por lag. No fim do documento.
