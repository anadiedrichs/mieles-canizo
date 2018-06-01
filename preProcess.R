library(readr)
library(tibble)
dataset <- read_csv("data/Miel.concentraciones.en.microgramos.por.gramo.csv")
# View(dataset) #<-- descomentar para ver dataset en Rstudio
#' Quitar columnas vacías
vacias <- 29:ncol(dataset)
dataset <- dataset[-vacias]
#' Cuántas variables y sus nombres
ncol(dataset)
colnames(dataset)
#' Cuántos ejemplos tenemos de cada zona
table(dataset[1])
dataset$`Zona geográfica` <- as.factor(dataset$`Zona geográfica`)
#' Vamos a realizar algunas gráficas descriptivas
#' 
library(caret)
library(ggplot2)
featurePlot(x=dataset[-1], y=dataset$`Zona geográfica`,  "box")
featurePlot(x=dataset[-1], y=dataset$`Zona geográfica`,  "strip", jitter = TRUE)
featurePlot(x=dataset[,2:6], y=dataset$`Zona geográfica`,  "pairs", auto.key=list(columns=3))
featurePlot(x=dataset[,2:6], y=dataset$`Zona geográfica`,  "ellipse", auto.key=list(columns=3))
# featurePlot(x=dataset[,20:29], y=dataset$`Zona geográfica`,  "pairs", auto.key=list(columns=3))
# featurePlot(x=dataset[,7:14], y=dataset$`Zona geográfica`,  "ellipse", auto.key=list(columns=3))

#' agregar columna etiqueta label para indicar si es o no de Valle de Uco para futura clasificación binaria

vDeUco <- "Valle de Uco"
add_column(dataset,label=0)
dataset[dataset$`Zona geográfica` == vDeUco,"label"] <- 1
dataset[dataset$`Zona geográfica` != vDeUco,"label"] <- -1
featurePlot(x=dataset[-1], y=as.factor(dataset$label),  "box")
#featurePlot(x=dataset[-1], y=as.factor(dataset$label),  "strip", jitter = TRUE)
featurePlot(x=dataset[,2:5], y=as.factor(dataset$label),  "pairs", auto.key=list(columns=2))

#' Correlaciones (lineales) entre las variables
#' 

library(corrplot)
M <- cor(dataset[-1])
corrplot(M, method = "ellipse")
corrplot(M, method = "number")

#' Vamos a normalizar las variables y luego realizar los feature plots, que parecieran estar afectados por outliers
quitar <- c(1,ncol(dataset))
data <-  dataset[-quitar]
media <- apply(data, 2, mean)
std <- apply(data, 2, sd)
data <- scale(data, center = media, scale = std)
data <- as.data.frame(cbind(data,label=dataset$label))
#' feature selection para estimar variables más importantes (variable importance? feature selectior package?)
featurePlot(x=data, y=as.factor(dataset$label),  "box")
featurePlot(x=data, y=as.factor(dataset$label),  "strip", jitter = TRUE)
#' TODO como mejorar el plot porque son muchas variables
featurePlot(x=data[,c(1,2,3,4)], y=as.factor(dataset$label),  "pairs", auto.key=list(columns=2))
featurePlot(x=data[-ncol(data)], y=as.factor(dataset$label),  "pairs", auto.key=list(columns=2))

#' Li y Na parecieran ser variables candidatas de mayor peso en el modelo clasificador.

boxplot(data[-ncol(data)])

#' TODO task: no prioritario, pero habría que ver opciones en la librería o programar algo 
#' para visualizar varias variables a la vez.

