#' # Análisis PCA
#' 
#' ### Ejemplo de uso de la librería con iris dataset
#' 
#' Hay que instalar ggplot2 y ggfortify
#' 
library(ggfortify)

df <- iris[c(1:4)]

autoplot(prcomp(df),data=iris,colour = 'Species', label=TRUE)

#' más ejemplos en https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
#' 
#' ## PCA con nuestro dataset
#' 
#' ### Carga y procesamiento de datos
#' 
library(readr)
library(tibble)
dataset <- read_csv("../data/Miel.concentraciones.en.microgramos.por.gramo.csv")
#' Quitar columnas vacías
vacias <- 29:ncol(dataset)
dataset <- dataset[-vacias]
#' Columna clase o etiqueta
vDeUco <- "Valle de Uco"
add_column(dataset,label=0)
names(dataset)[1] <- "Zona"
dataset[dataset$Zona == vDeUco,"label"] <- 1
dataset[dataset$Zona != vDeUco,"label"] <- -1

#' ### PCA
#' 

autoplot(prcomp(dataset[-1]),data=dataset,colour = 'Zona')


autoplot(prcomp(dataset[-1]),data=dataset,colour = 'Zona',loadings = TRUE)

autoplot(prcomp(dataset[-1]),data=dataset,colour = 'Zona',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

library(DataExplorer)

plot_prcomp(dataset[-1])

