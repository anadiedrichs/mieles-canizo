source("utils.R")
library(party)
library(caret)

d <- load.dataset()
data <- d$dataset
#' Desordenar el dataset previo al entrenamiento
#' y dividir en conjuntos de entrenamiento y testeo

index <- sample(1:nrow(data), round(nrow(data) * 0.7))
train <- data[index,]
test <- data[-index,]

#' Training with cross-validation k=10
mySeeds <- sapply(simplify = FALSE, 1:11, function(u) sample(10^4, 3))

train_control <- trainControl(method="cv", number=10, seeds = mySeeds)
#' ## Linear model
set.seed(825)
model.lm <- train(label~., data=train, 
                  trControl=train_control, method="lm")
print(model.lm)
summary(model.lm)
print(model.lm$finalModel)
#' Predicción en test-set
pred <- predict(model.lm,test)
pred[which(pred > 0)] <- 1
pred[which(pred <= 0)] <- -1
c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
print(c)
table.results <- as.data.frame(cbind(model="lm",t(c$overall),t(c$byClass)))

#' ## RANDOM FOREST MODEL
#' ### RF default
set.seed(825)
model.rf <- train(as.factor(label)~., data=train, 
                  trControl=train_control, method="rf",metric="Accuracy")

#'  ### Results of random forest model
print(model.rf)
plot(model.rf)
print(model.rf$finalModel)
#' ### Variable importance
varImp(model.rf)
plot(varImp(model.rf))
#' Predicción en test-set
pred <- predict(model.rf,test)
c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
print(c)
d <- as.data.frame(cbind(model="rf",t(c$overall),t(c$byClass)))
table.results <- rbind(table.results,d)

#' ### RF tunegrid mtry
#' 
#' Lo siguiente puede tardar en ejecutarse.
set.seed(825)
#mySeeds <- sapply(simplify = FALSE, 1:11, function(u) sample(10^4, 10))
train_control <- trainControl(method="cv", number=10, )
tunegrid <- expand.grid(.mtry=c(1:27))
model.rf2 <- train(as.factor(label)~., data=train, 
                  trControl=train_control, method="rf",metric="Accuracy", tuneGrid= tunegrid)

#'  ### Results of random forest model
print(model.rf2)
plot(model.rf2)
print(model.rf2$finalModel)
#' ### Variable importance
varImp(model.rf2)
plot(varImp(model.rf2))
#' Predicción en test-set
pred <- predict(model.rf2,test)
c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
print(c)
d <- as.data.frame(cbind(model="rf-tunegrid",t(c$overall),t(c$byClass)))
table.results <- rbind(table.results,d)



#' ## CTree or conditional inference tree
set.seed(825)
require(party)
model.ctree <- train(as.factor(label)~., data=train, 
                  trControl=train_control, method="ctree",metric="Accuracy")
print(model.ctree)
plot(model.ctree)
#' modelo final 
print(model.ctree$finalModel)
plot(model.ctree$finalModel)
#' Predicción en test-set
pred <- predict(model.ctree,test)
c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
print(c)
d <- as.data.frame(cbind(model="ctree",t(c$overall),t(c$byClass)))
table.results <- rbind(table.results,d)

# decidimos quitar LDA en la reunión 2018-09-27
# ## LDA o linear discriminant analysis
# 
# set.seed(825)
# model.lda <- train(as.factor(label)~., data=train, 
#                     trControl=train_control, method="lda",metric="Accuracy")
# print(model.lda)
#' modelo final 
# print(model.lda$finalModel)
#' Predicción en test-set
# pred <- predict(model.lda,test)
# c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
# print(c)
# d <- as.data.frame(cbind(model="LDA",t(c$overall),t(c$byClass)))
# table.results <- rbind(table.results,d)

#' ## C5.0
#' 
set.seed(825)
require(C50)
train_control_c <- trainControl(method="cv", number=10) # issue: dimension of seeds should be number of resamples

model.c50 <- train(as.factor(label)~., data=train, 
                   trControl=train_control_c, method="C5.0",metric="Accuracy")
#' salida modelos
print(model.c50)
#' resumen 
summary(model.c50)
#' modelo final 
print(model.c50$finalModel)
#' Predicción en test-set
pred <- predict(model.c50,test)
c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
print(c)
d <- as.data.frame(cbind(model="C5.0",t(c$overall),t(c$byClass)))
table.results <- rbind(table.results,d)

#' ## Rpart
#' 
set.seed(825)
#require(rpart)
library(rpart.plot)
#train_control_d <- trainControl(method="cv", number=10, seeds = mySeeds, classProbs = TRUE)

model.rpart <- train(x = train[,-ncol(train)], y = as.factor(train$label),  
                   trControl=train_control, method="rpart",metric="Accuracy")
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

#' ## Comparación de todos los modelos en test.set
#' 
results <- resamples(list(RF=model.rf,ctree=model.ctree,LDA=model.lda,C5.0=model.c50,rpart=model.rpart))
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