source("utils.R")
library(party)
library(caret)
SEED <- 825 # seed semilla para números aleatorios
d <- load.dataset()
data <- d$dataset
#' Desordenar el dataset previo al entrenamiento
#' y dividir en conjuntos de entrenamiento y testeo
set.seed(SEED)
index <- sample(1:nrow(data), round(nrow(data) * 0.7))
train <- data[index,]
test <- data[-index,]

#' Training with cross-validation k=10
set.seed(SEED)
mySeeds <- sapply(simplify = FALSE, 1:11, function(u) sample(10^4, 3))

METRIC <- "ROC" #Accuracy
train_control <- trainControl(method="cv", number=10,seeds = mySeeds
                              ,classProbs=TRUE, summaryFunction = twoClassSummary)

#' ## RANDOM FOREST MODEL
#' ### RF default
set.seed(SEED)
model.rf <- train(as.factor(label)~., data=train, 
                  trControl=train_control, method="rf",metric=METRIC, importance=T)

#'  ### Results of random forest model
print(model.rf)
plot(model.rf)
print(model.rf$finalModel)
#' ### Variable importance
varImp(model.rf)
plot(varImp(model.rf))
#' Predicción en conjunto de testeo test-set
pred <- predict(model.rf,test)
c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
print(c)
table.results <- as.data.frame(cbind(model="RF",t(c$overall),t(c$byClass)))



#' ## CTree or conditional inference tree

require(party)
set.seed(SEED)
model.ctree <- train(as.factor(label)~., data=train, 
                  trControl=train_control, method="ctree",metric=METRIC)
print(model.ctree)
plot(model.ctree)
#v <- varImp(model.ctree)
#' modelo final 
print(model.ctree$finalModel)
plot(model.ctree$finalModel)
#' Predicción en test-set
pred <- predict(model.ctree,test)
c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
print(c)
d <- as.data.frame(cbind(model="ctree",t(c$overall),t(c$byClass)))
table.results <- rbind(table.results,d)

#' ## Rpart
#' 
require(rpart)
library(rpart.plot)
#train_control_d <- trainControl(method="cv", number=10, seeds = mySeeds, classProbs = TRUE)

set.seed(SEED)
model.rpart <- train(x = train[,-ncol(train)], y = as.factor(train$label),  
                   trControl=train_control, method="rpart",metric=METRIC)
#' resumen experimental
print(model.rpart)
#' grafico parametros vs accuracy
print(model.rpart)
#' Variable importance
varImp(model.rpart)
#' modelo final 
print(model.rpart$finalModel)
#' Predicción en test-set
pred <- predict(model.rpart,test)
c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
print(c)
d <- as.data.frame(cbind(model="rpart",t(c$overall),t(c$byClass)))
table.results <- rbind(table.results,d)

rpart.plot(model.rpart$finalModel) # plot model tree

#' ## C5.0
#' 
library(C50)

set.seed(SEED)
mySeeds <- sapply(simplify = FALSE, 1:11, function(u) sample(10^4, 4))

METRIC <- "ROC" #Accuracy

train_control <- trainControl(method="cv", number=10,seeds = mySeeds
                              ,classProbs=TRUE, summaryFunction = twoClassSummary)


#train_control_c <- trainControl(method="cv", number=10) # issue: dimension of seeds should be number of resamples

set.seed(SEED)
#c50Grid <- expand.grid(trials = c(1:9, (1:10)*10),model = c("tree", "rules"),winnow = c(TRUE, FALSE))
c50Grid <- expand.grid(trials = 1,model = "tree",winnow = c(TRUE, FALSE))

model.c50 <- train(as.factor(label)~., data=train, 
                   trControl=train_control, method="C5.0",metric=METRIC,tuneGrid=c50Grid )
#' salida modelos
print(model.c50)

#' Valores del modelo elegido, modelo final.
model.c50$finalModel$tuneValue
#' Más texto o verbosidad 
model.c50$finalModel
#' Plot del árbol de decisión
mm <- C5.0(as.factor(label)~., data=train,trials = 1, rules = FALSE, winnow=FALSE)
plot(mm)
#summary(mm)

#' Predicción en test-set
pred <- predict(model.c50,test)
c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
print(c)
d <- as.data.frame(cbind(model="C5.0",t(c$overall),t(c$byClass)))
table.results <- rbind(table.results,d)

#' ## Comparación de todos los modelos en test.set
#' 
results <- resamples(list(RF=model.rf,ctree=model.ctree,C5.0=model.c50,rpart=model.rpart))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
#' Tabla final de resultados
print(table.results)
#' guardar resultados a disco
#' 
write.csv(table.results,file="results.csv")


library(pROC)
#' Dibujo la curva ROC para cada modelo
roc.rf <- predict(model.rf,test,type = "prob")
roc.c50 <- predict(model.c50,test,type = "prob")
roc.ctree <- predict(model.ctree,test,type = "prob")
roc.rpart <- predict(model.rpart,test,type = "prob")

roc1 <- roc(test$label, roc.rf$V) 
roc2 <- roc(test$label, roc.c50$V) 
roc3 <- roc(test$label, roc.ctree$V) 
roc4 <- roc(test$label, roc.rpart$V) # Draw ROC curve.
#plot(roc4,print.auc = TRUE,col="orange",add=TRUE)
# roc version 3, usando libreria ggplot2 y funcion ggroc


roc.list <- list(RF=roc1, C.50=roc2, Ctree=roc3,RPart=roc4 )

library(ggplot2)
library(magrittr)

len <- length(roc.list)
vars <- c("RF", "RPart","Ctree","C.50")
#t(sapply(unlist(lapply(roc.list,auc)),round,3))
auclabels <- lapply(roc.list,auc) %>%
             unlist %>% 
             sapply(round,3) 

auclabels <- paste("AUC:",auclabels)

dat <- data.frame(x = rep(.5, len), y = rep(.5, len), name=vars, 
                  labs=auclabels)
g.group <- ggroc(roc.list, aes="group")
p <- g.group + facet_grid(.~name)
p + geom_text(aes(x, y, label=labs, group=NULL),data=dat)