#' # Feature Selection: prueba con distintos enfoques
#' 
#' ### Carga de dataset
source("utils.R")
library(caret)
d <- load.dataset()
data <- d$dataset
head(data)
#' nombres de las variables
colnames(data)
#' Para experimentos reproducibles, usar la misma semilla para generación de números aleatorios.
set.seed(7)
#' ## Variables redundantes
#' calculate correlation matrix
correlationMatrix <- cor(data)
#' summarize the correlation matrix
print(correlationMatrix)
#' find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
#' print indexes of highly correlated attributes
print(highlyCorrelated)
#' print variables which are hichly correlated
print(colnames(data)[highlyCorrelated])

#' ## Rank features by importance
#' ### Learning Vector Quantization
set.seed(7)
#' prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#' train the model LVQ, learning vector quantization
model <- train(as.factor(label)~., data=data, method="lvq", preProcess="scale", trControl=control)
#' estimate variable importance
importance <- varImp(model, scale=FALSE)
#' summarize importance
print(importance)
#' plot importance
plot(importance)
#' ### Random Forest
#' 
set.seed(7)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(as.factor(label)~., data=data, method="rf", preProcess="scale", trControl=control)
#' estimate variable importance
importance <- varImp(model, scale=FALSE)
#' summarize importance
print(importance)
#' plot importance
plot(importance)


#***************************************************
#' ##  Relative importance of variables
#' Paquete relaimpo, informacion para citarlo en https://cran.r-project.org/web/packages/relaimpo/citation.html
library(relaimpo)
lm.model <- lm(formula=label~.,data= data)

#' La siguiente línea puede demorarse en ejecutar si se usan
#' los métodos lmg o pmvd
types = c("last", "first", "betasq", "pratt", "genizi", "car")  #"lmg", 
relImportance <- calc.relimp(lm.model, 
                             type = types, rela=TRUE)
plot(relImportance)

#' Ordenarmos de mayor importancia a menor importancia (criterio decreciente),
#' para cada uno de los métodos

s <- sort(relImportance$pratt, decreasing=TRUE)
print(s)
barplot(s[1:10],main="pratt")
s <- sort(relImportance$first, decreasing=TRUE)
print(s)
barplot(s[1:10],main="first")
s <-sort(relImportance$last, decreasing=TRUE)
print(s)
barplot(s[1:10],main="last")
s <-sort(relImportance$betasq, decreasing=TRUE)
print(s)
barplot(s[1:10],main="betasq")
s <- sort(relImportance$genizi, decreasing=TRUE)
print(s)
barplot(s[1:10],main="genizi")
s <-sort(relImportance$car, decreasing=TRUE)
print(s)
barplot(s[1:10],main="car")

#' ## Usando el paquete FSelector: feature ranking
#' Este paquete depende de paquetes RWekajars, RWeka que requieren java 8 instalado.
#' 1ro instalar java version 8, 2do instalar librerias rJava, RWekajars, RWeka
#' 3ro variable de entorno export LD_LIBRARY_PATH=/usr/lib/jvm/jre/lib/amd64:/usr/lib/jvm/jre/lib/amd64/default
#' 
library(FSelector)

#' chi-squared method
chi.squared(label~.,data)
#sort(chi.squared(label~.,data),decreasing = TRUE)
#' Pearson linear correlation
#' 
linear.correlation(label~.,data)
#' Spearman's correlation
rank.correlation(label~.,data)

#' Information Gain
information.gain(label~.,data)
#' Gain Ratio
gain.ratio(label~.,data)

#' ## MARS 
#'
library(earth)
marsModel <- earth(label ~ ., data=data) # build model
ev <- evimp (marsModel) 
print(ev)
plot(ev)
#' ## Recursive feature elimination
#' Prueba combinaciones de variables usando un modelo random forest.
#' Más info en https://topepo.github.io/caret/recursive-feature-elimination.html
set.seed(7)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(data[,-ncol(data)], as.factor(data$label), sizes=c(1:ncol(data)), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
#' Plot resultados plot the results
plot(results, type=c("g", "o"))
#' Variables más óptimas, cuya combinación dio el mejor resultado (mayor accuracy)
print(results$optVariables)

