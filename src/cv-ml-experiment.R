source("utils.R")
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

#' ## CTree or conditional inference tree
set.seed(825)
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

#' ## LDA o linear discriminant analysis
#' 
set.seed(825)
model.lda <- train(as.factor(label)~., data=train, 
                     trControl=train_control, method="lda",metric="Accuracy")
print(model.lda)
#' modelo final 
print(model.lda$finalModel)
#' Predicción en test-set
pred <- predict(model.lda,test)
c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
print(c)
d <- as.data.frame(cbind(model="LDA",t(c$overall),t(c$byClass)))
table.results <- rbind(table.results,d)

#' ## C5.0
#' 
set.seed(825)
library(C50)
model.c50 <- train(as.factor(label)~., data=train, 
                   trControl=train_control, method="C5.0",metric="Accuracy")
print(model.c50)
#' modelo final 
print(model.c50$finalModel)
#' Predicción en test-set
pred <- predict(model.c50,test)
c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
print(c)
d <- as.data.frame(cbind(model="C5.0",t(c$overall),t(c$byClass)))
table.results <- rbind(table.results,d)

#' ## Comparación de todos los modelos en test.set
#' 
results <- resamples(list(RF=model.rf,ctree=model.ctree,LDA=model.lda,C5.0=model.c50))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
#' Tabla final de resultados
print(table.results)