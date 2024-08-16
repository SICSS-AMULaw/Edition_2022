# Ex 1 ####
library(caret)
library(mlbench)
library(dplyr)
data(Vehicle)

data_set <- Vehicle

ctrl.loo <- trainControl(method = 'LOOCV',
                         search = 'grid') 

(model.knn <- train(Class ~ ., 
                    data = data_set, 
                    method = 'knn', 
                    tuneGrid = data.frame(k = 1:5),
                    trControl = ctrl.loo))
1-model.knn$results[2] #boot error rate 

mean(predict(model.knn) != data_set$Class) #resubtitution error

confusionMatrix(predict(model.knn), data_set$Class)
# Ex 2 ####
library(caret)
library(DAAG)
library(dplyr)

data_set <- leafshape

ctrl.loo <- trainControl(method = 'LOOCV',
                         search = 'grid') 

(model.nb <- train(location ~ ., 
                   data = data_set, 
                   method = 'nb',
                   trControl = ctrl.loo))

1-model.nb$results[2] #boot error rate 

mean(predict(model.nb) != data_set$location) #resubtitution error
# Ex 3 ####
library(caret)
library(MASS)
library(dplyr)

data_set <- painters

ctrl.loo <- trainControl(method = 'LOOCV',
                         search = 'grid') 

(model.lda <- train(School ~ ., 
                    data = data_set, 
                    method = 'lda',
                    trControl = ctrl.loo))

1-model.lda$results[2] #boot error rate 

mean(predict(model.lda) != data_set$School) #resubtitution error

confusionMatrix(predict(model.lda), data_set$School)
# Ex 4 ####
library(caret)
library(MASS)
library(dplyr)
library(rattle)

data_set <- Cars93

ctrl.loo <- trainControl(method = 'LOOCV',
                         search = 'grid') 

(model.tree <- train(Type ~ Length + Weight + EngineSize + Horsepower + RPM, 
                     data = data_set, 
                     method = 'rpart',
                     trControl = ctrl.loo,
                     tuneLength = 5))

1-model.tree$results[2] #boot error rate 

mean(predict(model.tree) != data_set$Type) #resubtitution error

confusionMatrix(predict(model.tree), data_set$Type)

fancyRpartPlot(model.tree$finalModel)
