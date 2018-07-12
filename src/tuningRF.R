source("utils.R")
library(caret)

d <- load.dataset()
data <- d$dataset
#' Desordenar el dataset previo al entrenamiento
#' y dividir en conjuntos de entrenamiento y testeo

index <- sample(1:nrow(data), round(nrow(data) * 0.7))
train <- data[index,]
test <- data[-index,]
seed <- 875
control <- trainControl(method="cv", number=10)

# Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(1:27))
modellist <- list()
for (ntree in c(50,100, 150, 200, 250,300,400,500)) {
  set.seed(seed)
  fit <- train(as.factor(label)~., data=train, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

#' TODO probar los resultados en testset del mejor modelo

for (ntree in c(50,100, 150, 200, 250,300,400,500)) {
  
  key <- toString(ntree)
  model.rf <- modellist[[key]]
  print(model.rf)
  plot(model.rf)
  print(model.rf$finalModel)
  #'  Variable importance
  varImp(model.rf)
  plot(varImp(model.rf))
  #' Predicción en test-set
  pred <- predict(model.rf,test)
  c <- confusionMatrix(as.factor(pred), as.factor(test$label),mode = "prec_recall")
  print(c)
  
  
  }


#########################################


# Algorithm Tune tuneRF funciton from randomForest package
set.seed(seed)
library(randomForest)
bestmtry <- tuneRF(x= train[-ncol(train)], y=train$label, stepFactor=1.5, improve=1e-5, ntree=50)
print(bestmtry)