library(dygraphs)
library(tidyverse)

# O dygraphs é um 'ggplot' só para séries temporais.
# O dygraphs é fácil de entender qualquer dúvida ou necessidade olhar no 
# https://cran.r-project.org/web/packages/dygraphs/dygraphs.pdf

# O objeto que usamos no dygraph para fazer os gráficos é um ts (Time-series).
# Exemplo de ts.
ts(1:10, # aqui temos os valores de cada período.
   frequency = 4, # a frequência por ano, trimestal (12/4=3).
   start = c(1959, 2)) # quando começa (2º trimestre de 1959)

ts1 <- ts(1:10, frequency = 4, start = c(1959, 2))
ts1
ts2 <- ts(1:12, frequency = 4, start = c(1959, 1))
ts2
ts3 <- ts(1:36, frequency = 12, start = c(1959, 1))
ts3
ts4 <- ts(1:104, frequency = 52, start = c(1959, 1))
ts4

glimpse(mdeaths) # Conhecendo os data frames.
glimpse(fdeaths) # São dois time-series de 1 a 72, de jan-1974 a dez-1979 (1980).

dygraph(mdeaths) # Fazendo os gráficos dygraph.
dygraph(fdeaths)

lungDeaths <- cbind(mdeaths, fdeaths) # Juntando mdeaths e fdeaths no mesmo gráfico.
dygraph(lungDeaths)

dygraph(lungDeaths) %>% dyRangeSelector() # Anexando um 'range selector'.

dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Homens") %>% # Mudando os labels.
  dySeries("fdeaths", label = "Mulheres") %>% # Mudando os labels.
  dyOptions(stackedGraph = TRUE) %>% # 'Empilhando' as linhas.
  # o dyOptions é muito extenso olhar no 
  # https://cran.r-project.org/web/packages/dygraphs/dygraphs.pdf
  dyRangeSelector(height = 20) # Altura do range selector.

hw <- HoltWinters(ldeaths) # hw é uma list de 9 do total de mortes: m + f.

# predict é uma função genérica de predição. Ainda não estudei.
# A função gerou um time-series de 3 linhas:fit (a certa), upr (a debaixo) e lwr (a de cima).
predicted <- predict(hw, n.ahead = 72, prediction.interval = TRUE) 

dygraph(predicted, main = "Previsão de mortes por doenças pulmonares") %>% # Gráfico da predicted com título.
  dyAxis("x", drawGrid = FALSE) %>% # Tirando as linhas (grid) do eixo x.
  dyAxis("y", drawGrid = TRUE) %>% # Não tirando as linhas (grid) do eixo y.
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))


# New haven tempratures: nhtemp.
# Time-series de 1 a 60, de 1912 a dez-1970 (1971).
glimpse(nhtemp)
plot(nhtemp, main = "New Haven Temperatures", ylab = "Temp (F)") # gráfico de linha do R base
dygraph(nhtemp, main = "New Haven Temperatures", ylab = "Temp (F)") # gáfico de linha dygraph

# Fazendo o mesmo, só que com 3 linhas.
lungDeaths_2 <- cbind(ldeaths, mdeaths, fdeaths) # ldeaths = mdeaths + fdeaths
dygraph(lungDeaths_2, main = "Mortes por doença do pulmão na Inglaterra") %>%
dySeries("mdeaths", label = "Homens") %>% # Mudando os labels.
dySeries("fdeaths", label = "Mulheres") %>% # Mudando os labels.
dySeries('ldeaths', label = 'Total') %>% # Mudando os labels.
dyOptions(colors = c('red', 'green', 'brown2'))

# Fazendo o steplot.
dygraph(lungDeaths_2, main = "Mortes por doença do pulmão na Inglaterra") %>%
dyOptions(colors = c('red', 'green', 'orange')) %>% 
dyOptions(stepPlot = TRUE)

dygraph(ldeaths, main = "Deaths from Lung Disease (UK)") %>%
# Colorindo abaixo do gráfico com opacidade (contrário de transparência) 0.4
dyOptions(fillGraph = TRUE, fillAlpha = 0.4) 

dygraph(ldeaths, main = "Deaths from Lung Disease (UK)") %>%
  # Colocando pontos nos gráficos.
  dyOptions(drawPoints = TRUE, pointSize = 2.5)

# Tem o pointshape também: square, diamond, pentagon, hexagon,  circle, star, plus or ex.
dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dySeries("mdeaths", drawPoints = TRUE, pointSize = 4, pointShape = "circle", color = "blue") %>%
  dySeries("fdeaths", stepPlot = TRUE, fillGraph = TRUE, color = "red")


# Tem a opção do agrupamento de linhas. Recebem o mesmo tratamento.
dygraph(lungDeaths_2, main = "Deaths from Lung Disease (UK)") %>%
  dySeries("fdeaths", stepPlot = TRUE, color = "red") %>% 
  dyGroup(c("mdeaths", "ldeaths"), drawPoints = TRUE, color = c("blue", "green"))

# Realçando as linhas: Series Highlighting.
dygraph(lungDeaths_2, main = "Deaths from Lung Disease (UK)") %>%
  dyHighlight(highlightCircleSize = 5, # coloca círculo tamanho 5 em todos.
              highlightSeriesBackgroundAlpha = 0.2, # realça dando opacidade 0.2 aos outros.
              hideOnMouseOut = FALSE) # 

# Realça engrossando a linha en tamanho 3.8 
dygraph(lungDeaths_2, main = "Deaths from Lung Disease (UK)") %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3.8))

# trabalhando os eixos.
dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyAxis("y", label = "Temp (F)", valueRange = c(40, 60)) %>% # trabalhando o eixo y
  dyOptions(axisLineWidth = 5, fillGraph = TRUE, drawGrid = FALSE) # trabalhando os eixos no geral

# labels & legends.
dygraph(discoveries, 
        main = "Descobertas Importantes", 
        ylab = "Descobertas por ano")
# mesmo resultado do gráfico acima.
dygraph(discoveries, main = "Descobertas Importantes") %>%
  dyAxis("y", label = "Descobertas por ano")

# Observe o dyLegend
dygraph(nhtemp, main = "New Haven Temperatures") %>% 
  dySeries("V1", label = "Temperature (F)") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) # A legenda aparece 'always'

dygraph(nhtemp, main = "New Haven Temperatures") %>% 
  dySeries("V1", label = "Temperature (F)") %>%
  dyLegend(show = "follow") # Aqui a legenda 'follow' o mouse.

glimpse(presidents)
dygraph(presidents, main = "Presidential Approval") %>%
  dyAxis("y", valueRange = c(0, 100)) %>%
  dyAnnotation("1950-7-1", text = "A", tooltip = "Korea") %>%
  dyAnnotation("1965-1-1", text = "B", tooltip = "Vietnam")

# Colocando linhas verticais. dyEvent. https://cran.r-project.org/web/packages/dygraphs/dygraphs.pdf
dygraph(presidents, main = "Presidential Approval") %>%
  dyAxis("y", valueRange = c(0, 100)) %>%
  dyEvent("1950-6-30", "Korea", labelLoc = "bottom") %>%
  dyEvent("1965-2-09", "Vietnam", labelLoc = "bottom")

dygraph(presidents, main = "Presidential Approval") %>%
  dyAxis("y", valueRange = c(0, 100)) %>%
  dyEvent(c("1950-6-30", "1965-2-09"), c("Korea", "Vietnam"), labelLoc = "bottom")
