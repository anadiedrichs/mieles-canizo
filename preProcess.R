library(readr)
library(tibble)
dataset <- read_csv("data/Miel.concentraciones.en.microgramos.por.gramo.csv")
# View(dataset) #<-- descomentar para ver dataset en Rstudio
#' quitar columnas vacías
vacias <- 29:ncol(dataset)
dataset <- dataset[-vacias]
#' Cuántos ejemplos tenemos de cada zona
table(dataset[1])
dataset$`Zona geográfica` <- as.factor(dataset$`Zona geográfica`)
#' Vamos a realizar algunas gráficas descriptivas
#' 
library(caret)
library(ggplot2)
featurePlot(x=dataset[-1], y=dataset$`Zona geográfica`,  "box")
featurePlot(x=dataset[-1], y=dataset$`Zona geográfica`,  "strip", jitter = TRUE)
featurePlot(x=dataset[,2:15], y=dataset$`Zona geográfica`,  "pairs", auto.key=list(columns=3))

#' agregar columna etiqueta label para indicar si es o no de Valle de Uco para futura clasificación binaria
vDeUco <- "Valle de Uco"
add_column(dataset,label=0)
dataset[dataset$`Zona geográfica` == vDeUco,"label"] <- 1
dataset[dataset$`Zona geográfica` != vDeUco,"label"] <- -1
featurePlot(x=dataset[-1], y=as.factor(dataset$label),  "box")
#featurePlot(x=dataset[-1], y=as.factor(dataset$label),  "strip", jitter = TRUE)
featurePlot(x=dataset[,2:15], y=as.factor(dataset$label),  "pairs", auto.key=list(columns=2))

#' TODO size of featurePlot 
#' Li y Na parecieran ser variables candidatas de mayor peso en el modelo clasificador.

#' normalizar las variables y luego realizar los feature plots
quitar <- c(1,ncol(dataset))
data <-  dataset[-quitar]
media <- apply(data, 2, mean)
std <- apply(data, 2, sd)
data <- scale(data, center = media, scale = std)
data <- as.data.frame(cbind(data,label=dataset$label))
#' feature selection para estimar variables más importantes (variable importance? feature selectior package?)
featurePlot(x=data, y=as.factor(dataset$label),  "box")
#featurePlot(x=data, y=as.factor(dataset$label),  "strip", jitter = TRUE)
#' TODO este plot da error
featurePlot(x=data, y=as.factor(dataset$label),  "pairs", auto.key=list(columns=2))

#' Desordenar el dataset previo al entrenamiento.
rowsIndex <- sample(1:nrow(dataset),size=nrow(dataset),replace=FALSE)
#' TODO Caso binario probar con SVM, LINEAL, NNET. 
#data[,"label"] <- as.factor(data[,"label"])
#library(e1071)
#svm.model <- svm(formula=as.factor(label)~.,data,type="C-classification")
#d <- data[,-ncol(data)]
#pred <- predict(svm.model,d)

