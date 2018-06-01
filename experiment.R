
#' * Verificar que working directory es la misma carpeta/directorio donde se encuentra este archivo.
#' * Ver working directory actual: usar getwd()
#' * Modificar working directory: setwd("PATH/A/MI/CARPETA")
source("utils.R")
library(caret)
d <- load.dataset()
data <- d$dataset
head(data)
## Para experimentos reproducibles, usar la misma semilla para generación de números aleatorios.
set.seed(7)

#' Desordenar el dataset previo al entrenamiento
#' y dividir en conjuntos de entrenamiento y testeo

index <- sample(1:nrow(data), round(nrow(data) * 0.7))
train <- data[index,]
test <- data[-index,]

### MODELO LINEAL 

#' ## Linear models
lm.model <- lm(formula=label~.,data= train)
summary(lm.model)
#' predicción en conjunto entrenamiento
pred <- predict(lm.model,train)
pred[which(pred > 0)] <- 1
pred[which(pred < 0)] <- -1
# matriz de confusión
table(pred,train$label) 
confusionMatrix(table(pred,train$label))
#' prediccion en conjunto de testeo
pred <- predict(lm.model,test)
pred[which(pred > 0)] <- 1
pred[which(pred < 0)] <- -1
# matriz de confusión
table(pred,test$label) 
confusionMatrix(table(pred,test$label))

#' ## Modelo Random Forest 
#
library(randomForest)
set.seed(7)
rf.model <- randomForest(x = train[-ncol(train)], y = as.factor(train$label))
plot(rf.model)
summary(rf.model)
#' Variables más importantes según Gini coefficient (mayor valor, más importante)
importance(rf.model)
#' Resultados predicción random forest en conjunto testeo
pred <- predict(rf.model,test)
table(pred,as.factor(test$label)) 
confusionMatrix(pred,as.factor(test$label))
#' como obtener uno de los árboles creados por el modelo
#' cambiar el 1 por otro nro de árbol para ir visualizando los otros
getTree(rf.model, 1, labelVar=TRUE)

### Modelo árbol
# An implementation of the random forest and bagging ensemble algorithms 
# utilizing conditional inference trees as base learners.
library(party)
set.seed(7)
#conditional inference tree
cmodel <- ctree(formula=as.factor(label)~.,data = train)
plot(cmodel, main="Conditional inference tree")
print(cmodel)
pred <- predict(cmodel,test)
table(pred,as.factor(test$label)) 
confusionMatrix(pred,as.factor(test$label))


#' ## MODELO ARBOL CARET 
library(rpart)
library(rpart.plot)
set.seed(7)
rpart.model <- rpart(formula=as.factor(label)~.,data= train)
summary(rpart.model)
prp(rpart.model)
printcp(rpart.model)
plotcp(rpart.model)
#' prediccion en conjunto de testeo
pred <- predict(rpart.model,test)
pred[which(pred > 0)] <- 1
pred[which(pred <= 0)] <- -1
#' matriz de confusión
# TODO error en las dimensiones de vector pred, no son iguales a length(test)
# table(pred,test$label) 
# confusionMatrix(table(pred,test$label))
