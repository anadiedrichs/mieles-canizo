library(readr)
library(tibble)

#' Carga datos del excel generado por Brenda Canizo
#' 
load.raw.data <- function()
{
  dataset <- suppressMessages(read_csv("../data/Miel.concentraciones.en.microgramos.por.gramo.csv"))
  #' Quitar columnas vacías
  vacias <- 29:ncol(dataset)
  dataset <- dataset[-vacias]
  return(dataset)
}
#' Carga el dataset listo para ser entrenado por cualquier modelo
#' Caso binario: son o no de Valle de Uco
load.dataset <- function()
{
  dataset <- load.raw.data()
  # etiquetar caso binario
  vDeUco <- "Valle de Uco"
  add_column(dataset,label=0)
  dataset[dataset$`Zona geográfica` == vDeUco,"label"] <- 1
  dataset[dataset$`Zona geográfica` != vDeUco,"label"] <- -1
  # normalizar
  quitar <- c(1,ncol(dataset))
  data <-  dataset[-quitar]
  media <- apply(data, 2, mean)
  std <- apply(data, 2, sd)
  data <- scale(data, center = media, scale = std)
  data <- as.data.frame(cbind(data,label=dataset$label))
  #data$label <- as.factor(data$label)
  return(list(dataset = data, media = media, std = std, raw=dataset))
}