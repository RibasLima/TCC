# TCC
Repositório específico para o meu tcc

# To do

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
Comparação de presidentes: Analisar tendência. Houve mudança? Aparentemente, sim, uma vez que no Meirelles cai, no Tombini sobe e no Goldfajn cai novamente. Entender o que houve nos idos de 2004 e 2005… por que sobe?

Ratios: conforme p. 9 do Copertwait & Metcalfe.
Eventos:
Sem acontecimentos: 0.9260047
Semana antes do COPOM: 1.058989
Semana da reunião do COPOM: 1.038477
Semana da ata: 1.01364
Semana pós anúncio da ata: 1.049312
Nenhuma variação acima de 8%. Alguma movimentação a mais em semanas com eventos do copom, especialmente antes do copom e depois da ata, mas nada demais.

Presidentes:
Meirelles: 0.492374
Tombini: 2.266493
Golfajn: -0.07238074
Grandes variações. Podemos ver que as percepções do mercado variaram muito conforme o presidente do BC.
Próximos passos: fazer ratios individuais para cada presidente, comparando as médias dos eventos com as médias dos presidentes.
