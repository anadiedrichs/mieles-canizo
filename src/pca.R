#' # Análisis PCA
#' 
#' ### Ejemplo de uso de la librería con iris dataset
#' 
#' Hay que instalar ggplot2 y ggfortify
#' 
library(ggfortify)
library(factoextra)

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
#' Agrego columna Region
dataset[dataset$Zona == vDeUco,"Region"] <- "Valle_de_Uco"
dataset[dataset$Zona != vDeUco,"Region"] <- "Rest_of_Mza"
data <- dataset[,2:(ncol(dataset)-1)] # quito primera y última columna
#' ### PCA
#' 

#' Usando factoextra para graficar
#' 
pca.model <- prcomp(data,scale. = TRUE)

#' Valores regresados por la funcion prcomp()
  
names(pca.model)
#' sdev : es la desviación estándar de los componenest principales
#'  (las raíces cuadradas de los eigenvalues (valores propios))

head(pca.model$sdev)

#' rotación: la matriz de variables "loadings", columnas que son vectores propios (eigenvectors)

head(unclass(pca.model$rotation)[, 1:4])
#' en center y scale se guardan los valores de media y desviación que normalizaron los datos
#' 
print(pca.model$center)
print(pca.model$scale)
#' Para ver con detalle coordenadas, coseno cuadrado y contribuciones
get_pca_ind(pca.model)
#' Por ejemplo para acceder al cos2 de los individuos usar ´get_pca_ind(pca.model)$cos2´
#' Eigenvalues y varianza
summary(pca.model)

#' What mean eigenvalues ?

#' Recall that eigenvalues measures the variability retained by each PC. It’s large for the first PC and small for the subsequent PCs.

#' The importance of princpal components (PCs) can be visualized with a scree plot.

fviz_screeplot(pca.model, ncp=6)

#' How to determine the number of components to retain?
  
#' An eigenvalue > 1 indicates that PCs account for more variance than accounted by one of the original variables in standardized data. 
#' This is commonly used as a cutoff point for which PCs are retained.
#' You can also limit the number of component to that number that accounts for a certain fraction of the total variance. For example, if you are satisfied with 80% of 
#' the total variance explained then use the number of components to achieve that.
#' Note that, a good dimension reduction is achieved when the the first few PCs account for a large proportion of the variability (80-90%).

#' ### Graph of variables
#' 
fviz_pca_var(pca.model)
#' mismo gráfico en tono azules
fviz_pca_var(pca.model, col.var = "steelblue")
#' Los colores de las variables varían según su contribución.
#' 
fviz_pca_var(pca.model, col.var = "contrib", 
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())
#' Elegimos un variables con el cos2 >= 0.6
#' 
fviz_pca_var(pca.model, col.var = "cos2", select.var = list(cos2=0.6),
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())
#' Elegimos variables con contribucion mayor igual 
fviz_pca_var(pca.model, col.var = "contrib", select.var = list(contrib=0.6) ,
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())

#'  How to interpret the correlation plot?
#' The graph of variables shows the relationships between all variables :
#' Positively correlated variables are grouped together.
#' Negatively correlated variables are positioned on opposite sides of 
#' the plot origin (opposed quadrants).
#' The distance between variables and the origine measures the quality 
#' of the variables on the factor map. Variables that are away from the origin are well represented on the factor map.

#' ### Gráfico de individuos
#' Usando librería factoextra.
#' Graficamos los individuos mediante puntos, cuyo color varía según el coseno
#' cuadrado (cos2), que representa la calidad de los individuos en el factor map
fviz_pca_ind(pca.model, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))
#' Coloreamos individuos por grupos (son de VAlle de Uco o no).
#' Sobre colores, ver RColorBrewer
#'
fviz_pca_ind(pca.model, label="none", habillage=dataset$Region,
             addEllipses=TRUE, ellipse.level=0.95, palette = "Dark2")
#' O podemos cambiar los colores usando una paleta de colores 
#' definida por uno mismo.
fviz_pca_ind(pca.model, label="none", habillage=dataset$Region,
             addEllipses=TRUE, ellipse.level=0.95, palette = c("#999999", "#E69F00", "#56B4E9"))

#' con ggfortify
autoplot(pca.model,data=dataset,colour = 'Region')

#' ## Biplot de individuos y variables
#' 
fviz_pca_biplot(pca.model, label = "var", habillage=dataset$Region,
                addEllipses=TRUE, ellipse.level=0.85,
                ggtheme = theme_minimal())

#' usando librería ggfortify, gráfico individuos + loadings
autoplot(pca.model,data=dataset,colour = 'Region',loadings = TRUE)

autoplot(pca.model,data=dataset,colour = 'Region',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4)
#' con librería DataExplorer
#' 
library(DataExplorer)

plot_prcomp(dataset[-1])

#' más ejemplos en https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
#' 
