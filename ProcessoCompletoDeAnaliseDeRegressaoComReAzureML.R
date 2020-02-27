### PROCESSO COMPLETO DE UMA ANALISE DE REGRESSAO UTILIZANDO O R E O AZURE MACHINE LEARNING ##

                              
                                # COLETA E TRANSFORMAÇÃO DOS DADOS


# Este codigo contem comandos para filtrar e transformar os dados de aluguel de bikes
# Foi criado para executar tanto no RStudio quanto no Azure


# Variável que controla a execução do script
Azure <- FALSE

  
# Execução de acordo com o valor da variavel Azure
if(Azure) {
  source("src/Tools.R")
  bikes <- maml.mapInputPort(1)
  bikes$dteday <- set.asPOSIXct(bikes)
} else {
  source("Tools.R")
  bikes <- read.csv("bikes.csv", sep = ',', header = TRUE, stringsAsFactors = FALSE)
  
  
  # Selecionar as variaveis que serao usadas
  cols <- c("dteday", "mnth", "hr", "holiday",
            "workingday", "weathersit", "temp",
            "hum", "windspeed", "cnt")
  
  
  # Criando um subset dos dados
  bikes <- bikes[, cols]
  
  # Transformar o objeto de data
  bikes$dteday <- char.toPOSIXct(bikes)
  
  # Esta linha acima gera 2 valores NA
  # Esta linha abaixo corrige
  bikes <- na.omit(bikes)
  
  # Normalizar as variaveis preditoras
  cols <- c("temp", "hum", "windspeed")
  bikes[, cols] <- scale(bikes[, cols])
}

# ?scale
str(bikes)
View(bikes)

# Criando uma variavel para indicar um dia de semana (workday)
bikes$isWorking <- ifelse(bikes$workingday & !bikes$holiday, 1, 0)

# Adiciona uma coluna com a quantidade de meses, o que ajudará no modelo
bikes <- month.count(bikes)

# Cria um fator ordenado para o dia da semana, começando por segunda-feira
# Neste fator é convertido para ordenado numerico para ser compatível com os tipos
# de dados
bikes$dayweek <- as.factor(weekdays(bikes$dteday))


######## ATENÇÃO #########

# => Analise o dataframe bikes.
str(bikes$dayweek)

bikes$dayweek <- as.numeric(ordered(bikes$dayweek,
                                    levels = c("segunda-feira", 
                                               "terça-feira",
                                               "quarta-feira",
                                               "quinta-feira",
                                               "sexta-feira",
                                               "sábado",
                                               "domingo")))

# Agora os dias da semana devem estar como valores numéricos 
str(bikes$dayweek)
str(bikes)

# Adiciona uma variavel com valores unicos para o horário do dia em dias de semana e dias
# de fim de semana. Com isso diferenciamos as horas dos dias de semana, das horas em dias
# de fim de semana.
bikes$workTime <- ifelse(bikes$isWorking, bikes$hr, bikes$hr + 24)

# Transforma os valores de hora na madrugada, quando a demanda por bicicletas e praticamente nula
bikes$xformHr <- ifelse(bikes$hr > 4, bikes$hr - 5, bikes$hr + 19)

# Adiciona uma variavel com valores unicos para o horário do dia em dias de semana e dias
# de fim de semana. Considerando horas da madrugada
bikes$xformWorkHr <- ifelse(bikes$isWorking, bikes$xformHr, bikes$xformHr + 24)

# Resutado final:
View(bikes)
str(bikes)

# TODo ESTE TRABALHO REALIZADO ATE AQUI TAMBÉM E CHAMADO DE FEATURE ENGINEERING OU
# ENGENHARIA DE ATRIBUTOS


                          #### ANÁLISE DE CORRELAÇÃO ####


# Definindo as colunas para a analise de correlaçao
cols <- c("mnth", "hr", "holiday", "workingday", "weathersit",
          "temp", "hum", "windspeed", "isWorking", "monthCount", 
          "dayweek", "workTime", "xformHr", "cnt")

# METODOS DE CORRELAÇAO
# Pearson -> coeficiente usado para medir o grau de relacionamento entre 2 variaveis com relaçao linear
# Spearman -> teste não parametrico, para medir o grau de relacionamento entre 2 variaveis
# Kendall -> teste não paramétrico, para medir a força de dependencia entre 2 variaveis

# Vetor com os nomes dos metodos
metodos <- c("pearson", "spearman")

# Aplicando os metodos de correlação com a funçao cor()
cors <- lapply(metodos, function(method)
  (cor(bikes[, cols], method = method)))

head(cors)

# Preparando o plot
require(lattice)
plot.cors <- function(x, labs){
  diag(x) <- 0.0
  plot( levelplot(x,
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)))
}

# Mapa de correlação
Map(plot.cors, cors, metodos)



                          ### ANÁLISE DE SERIES TEMPORAIS ###


# Avaliando a demanda por aluguel de bikes ao longo do tempo 
# Construindo um time series plot para alguns determinados horarios
# em dias uteis e dias de fim de semana
times <- c(7, 9, 12, 15, 18, 20, 22)

# Time series plot
tms.plot <- function(times) {
  ggplot(bikes[bikes$workTime == times, ], aes(x = dteday, y = cnt)) +
    geom_line() +
    ylab("Número de Bikes") +
    labs(title =  paste("Demanda de bikes as ", as.character(times), ":00", sep = "")) +
    theme(text = element_text(size = 20))    
}

require(ggplot2)
lapply(times, tms.plot)


                            
                              ## ANALISANDO BOX PLOTS ##


# Convertendo a variável dayweek para fator ordenado e plotando em ordem de tempo
bikes$dayweek <- fact.conv(bikes$dayweek)

# Demanda de bikes x poenciais variáveis preditoras
labels <- list("BoxPlots - Demanda de Bikes por Hora",
               "BoxPlots - Demanda de Bikes por Estação",
               "BoxPlots - Demanda de Bikes por dia Útil",
               "BoxPlots - Demanda de Bikes por dia da Semana")

xAxis <- list("hr", "weathersit", "isWorking", "dayweek")

# Função para criar os Boxplots
plot.boxes <- function(X, label) {
  ggplot(bikes, aes_string(x = X, y = "cnt", group = X)) +
    geom_boxplot( ) + 
    ggtitle(label) +
    theme(text = element_text(size = 18))
}

Map(plot.boxes, xAxis, labels)


  
                            ## ANALISANDO DENSITY PLOTS ##


# Visuaizando o relacionamento entre as variaveis preditoras e demanda por bike
labels <- c("Demanda por Bikes vs Temperatura",
            "Demanda por Bikes vs Humidade",
            "Demanda por Bikes vs Velocidade do Vento",
            "Demanda por Bikes vs Hora")

xAxis <- c("temp", "hum", "windspeed", "hr")


# Função para os density plots
plot.scatter <- function(X, label) {
  ggplot(bikes, aes_string(x = X, y = "cnt")) +
    geom_point(aes_string(colour = "cnt"), alpha = 0.1) +
    scale_color_gradient(low = "green", high = "blue") +
    geom_smooth(method = "loess") +
    ggtitle(label) +
    theme(text = element_text(size = 20))
}

Map(plot.scatter, xAxis, labels)


# Explorando a interação entre tempo e dia, em dias da semana e fins de semana
labels <- list("Box Plots - Demanda por bikes as 9:00 para \n dias da semana e fins de semana",
               "Box Plots - Demanda por bikes as 18:00 para \n dias da semana e fins de semana")

times <- list(9, 18)

plot.box2 <- function(time, label) {
  ggplot(bikes[bikes$hr == time, ], aes(x = isWorking, y = cnt, group = isWorking)) +
    geom_boxplot( ) + ggtitle(label) +
    theme(text = element_text(size = 18))
}

Map(plot.box2, times, labels)

# Gera a saida no AzureML
if(Azure) maml.mapOutputPort("bikes")




                         ### MODELAGEM PREDITIVA ###


# Verificando as dimensoes do dataset
dim(bikes)

# Verificando se existem valores missing no dataset
any(is.na(bikes))

# Criando um modelo para identificar os atributos com maior importancia para o modelo preditivo
require(randomForest)


# Avaliando a importancia de todas as variaveis.
modelo_rf <- randomForest(
  cnt ~ ., 
  data = bikes,
  ntree = 100,
  nodesize = 10,
  importance = TRUE
)

# Removendo variáveis colineares
modelo_rf <- randomForest(
  cnt ~ .
  - mnth
  - hr
  - workingday
  - isWorking
  - dayweek
  - xformHr
  - workTime
  - holiday
  - windspeed
  - monthCount
  - weathersit,
  data = bikes,
  ntree = 100,
  nodesize = 10,
  importance = TRUE
)

# Plotando as variaveis por grau de importancia
varImpPlot(modelo_rf)

# Gravando o resultado
df_saida <- bikes[, c("cnt", rownames(modelo_rf$importance))]



# Construindo um modelo de Random Forest
require(randomForest)
model_rf <- randomForest(cnt ~ xformWorkHr + dteday + temp + hum,
                         data = bikes, # altere o nome do objeto data para dataser se estiver no Azure ML
                         ntree = 40,
                         nodesize = 5)

print(model)


# Scores
require(randomForest)
scores <- data.frame(
  actual = bikes$cnt,
  prediction = predict(model, newdata = bikes)
)


# Criando um dataframe
inFrame[, c("dteday", "monthCount", "hr", "xformWorkHr")] <- refFrame[, c("dteday", "monthCount")]


# Nomeando o conjunto de dados
names(inFrame) <- c("cnt", "predicted", "dteday", "monthCount", "hr", "xformWorkHr")


# Time Series Plot mostrando a diferença entre valores reais e valores previstos
library(ggplot2)

inFrame <- inFrame[order(inFrame$dteday), ]
s <- c(7, 9, 12, 15, 18, 20, 22)

lapply(s, function(s){
  ggplot() +
    geom_line(data = inFrame[inFrame$hr == s, ],
              aes(x = dteday, y = cnt)) +
    geom_line(data = inFrame[inFrame$hr == s, ],
              aes(x = dteday, y = predicted), color = "red") +
    ylab("Numero de Bikes") +
    labs(title = paste("Demanda de Bikes as ",
                       as.character(s), ":00", sep = "")) +
    theme(text = element_text(size = 20))          
  
})


# Computando os resíduos
library(dplyr)
inFrame <- mutate(inFrame, resids = predicted - cnt)


# Plotando os residuos
ggplot(inFrame, aes(x = resids)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black")

qqnorm(inFrame$resids)
qqline(inFrame$resids)


# Plotando os residuos com as horas transformadas
inFrame <- mutate(inFrame, fact.hr = as.factor(hr),
                  fact.xformWorkHr = as.factor(xformWorkHr))
facts <- c("fact.hr", "fact.xformWorkHr")

lapply(facts, function(x) {
  ggplot(inFrame, aes_string(x = x, y = "resids")) +
    geom_boxplot() + 
    ggtitle("Resíduos - Demanda de Bikes por Hora - Atual vs Prvisão")
  
})


# Mediana dos residuos por hora
evalFrame <- inFrame %>%
  group_by(hr) %>%
  summarise(medResidByHr = format(round(
    median(predicted - cnt), 2),
    nsmall = 2
  ))


# Computando a Mediana dos residuos
tempFrame <- inFrame %>%
  group_by(monthCount) %>%
  summarise(medResid = median(predicted - cnt))


evalFrame$monthCount <- tempFrame$monthCount
evalFrame$medResidByMcnt <- format(round(tempFrame$medResid, 2), nsmall = 2)

print("Resumo dos residuos")
print(evalFrame)


# output
outFrame <- data.frame(evalFrame)
if(Azure) maml.mapOutputPort("outFrame")