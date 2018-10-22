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
  add_column(dataset,label="Valle_de_Uco")
  dataset[dataset$`Zona geográfica` == vDeUco,"label"] <- "V" #Valle de Uco
  dataset[dataset$`Zona geográfica` != vDeUco,"label"] <- "O" # Otras regiones
  # normalizar
  quitar <- c(1,ncol(dataset))
  data <-  dataset[-quitar]
  media <- apply(dataset[-quitar], 2, mean)
  std <- apply(dataset[-quitar], 2, sd)
  dataset[-quitar] <- scale(dataset[-quitar], center = media, scale = std)
 # data <- data.frame(cbind(data,label=dataset$label,stringsAsFactors=FALSE))
  #data$label <- as.factor(data$label)
  return(list(dataset = dataset[-1], media = media, std = std, raw=dataset))
}