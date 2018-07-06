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
confusionMatrix(as.factor(pred), as.factor(test$label))


#' ## RANDOM FOREST MODEL
set.seed(825)
model.rf <- train(as.factor(label)~., data=train, 
                  trControl=train_control, method="rf",metric="Accuracy")

#' ### Results of random forest model
print(model.rf)
plot(model.rf)
print(model.rf$finalModel)
#' ### Variable importance
varImp(model.rf)
plot(varImp(model.rf))
#' Predicción en test-set
pred <- predict(model.rf,test)
confusionMatrix(pred, as.factor(test$label))


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
confusionMatrix(pred, as.factor(test$label))

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
confusionMatrix(pred, as.factor(test$label))

#' ## Comparación de todos los modelos en test.set
#' 
results <- resamples(list(RF=model.rf,ctree=model.ctree,LDA=model.lda))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)
