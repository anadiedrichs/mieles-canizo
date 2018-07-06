library(readr)
library(tibble)
library(DataExplorer)
library(caret)
library(ggplot2)

#' Cargamos dataset a memoria
dataset <- read_csv("../data/Miel.concentraciones.en.microgramos.por.gramo.csv")
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
names(dataset)[1] <- "Zona"

#' agregar columna etiqueta label para indicar si es o no de Valle de Uco para futura clasificación binaria

vDeUco <- "Valle de Uco"
add_column(dataset,label=0)
dataset[dataset$Zona == vDeUco,"label"] <- 1
dataset[dataset$Zona != vDeUco,"label"] <- -1

#' ## Boxplot de distribución de variables por zona
plot_boxplot(dataset,by="Zona")
#' ## Boxplot de distribución de variables por si está o no en zona Valle de Uco
plot_boxplot(dataset,by="label")

#' ## Correlaciones (lineales) entre las variables
#' 
plot_correlation(dataset[-1],use="pairwise.complete.obs")

#' ## Plots con variables normalizadas
#' Vamos a normalizar las variables y luego realizar los feature plots, que parecieran estar afectados por outliers
quitar <- c(1,ncol(dataset))
data <-  dataset[-quitar]
media <- apply(data, 2, mean)
std <- apply(data, 2, sd)
data <- scale(data, center = media, scale = std)
data <- as.data.frame(cbind(data,label=dataset$label))

#'  Boxplot de distribución de variables por si está o no en zona Valle de Uco
plot_boxplot(data,by="label")

#' ### Feature plot, pairs of variables
featurePlot(x=dataset[,2:6], y=as.factor(dataset$label),  "pairs", auto.key=list(columns=2))
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(dataset[,2:7], pch = 19,  cex = 0.5,
      col = my_cols[dataset$Zona],
      lower.panel=NULL)

#' Li y Na parecieran ser variables candidatas de mayor peso en el modelo clasificador.
#' ## Boxplot todos los datos
boxplot(data[-ncol(data)])



