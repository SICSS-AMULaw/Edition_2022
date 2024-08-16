# Exercise 1
library(dplyr)
library(caret)

#Creating a data frame
depression <- c(6, 4, 0, 4, 0, 11, 11, 5, 8, 4, 12, 8, 9, 8, 11)
anxiety <- c(8, 3, 2, 1, 8, 9, 6, 7, 6, 9, 11, 8, 6, 10, 4)
chaos <- c(9, 3, 8, 6, 4, 8, 6, 4, 5, 4, 6, 5, 7, 8, 3)
group <- factor(rep(1:3, each = 5))
data.set <- data.frame(depression, anxiety, chaos, group)

ctrl.loo = trainControl(method = "LOOCV")

model_nb = train(group ~ depression + anxiety + chaos,
                 data = data.set,
                 method = 'nb',
                 trControl = ctrl.loo,
                 tuneGrid = data.frame(fL = 0, adjust = 1, usekernel = FALSE))

model_nb

1 - as.numeric(model_nb$results[4]) #LOOCV

# Exercise 2
library(mlbench)
library(caret)

data(Sonar)
data <- Sonar

ctrl.loo = trainControl(method = "LOOCV")

model_qda = train(Class ~ .,
                  data = data,
                  method = 'qda',
                  trControl = ctrl.loo)

model_qda

1 - as.numeric(model_qda$results[2]) # LOOCV

predicted_labels <- predict(model_qda)
true_labels <-  data$Class
mean(predicted_labels != true_labels) # Resubstitution Error
confusionMatrix(predicted_labels, (true_labels))

# Exercise 3
library(caret)
library(mlbench)
data(Glass)

data_set <- Glass

ctrl.loo <- trainControl(method = 'LOOCV',
                         search = 'grid') 

(model.knn <- train(Type ~ ., 
                    data = data_set, 
                    method = 'knn', 
                    tuneGrid = data.frame(k = 1:3),
                    trControl = ctrl.loo))

1-model.knn$results[2] #LOO CV

mean(predict(model.knn) != data_set$Type) #resubtitution error

confusionMatrix(predict(model.knn), data_set$Type)

# Exercise 4
library(mlbench)
library(caret)
library(rattle)

data(Vehicle)
Vehicle

data_set <- Vehicle

ctrl.loo <- trainControl(method = 'LOOCV',
                         search = 'grid') 

(model.tree <- train(Class ~ ., 
                     data = data_set, 
                     method = 'rpart',
                     trControl = ctrl.loo,
                     tuneLength = 5))

1-model.tree$results[2] #LOO CV

mean(predict(model.tree) != data_set$Class) #resubtitution error

fancyRpartPlot(model.tree$finalModel)

# Exercise 5
library(caret)

data <- read.table('http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data',
                   header = TRUE,
                   row.names = 1,
                   sep = ',')

ctrl.loo = trainControl(method = "LOOCV")

model_lda = train(famhist ~ .,
                  data = data,
                  method = 'lda',
                  trControl = ctrl.loo)

model_lda

1 - as.numeric(model_lda$results[2]) # LOOCV

predicted_labels <- predict(model_lda)
true_labels <-  data$famhist
mean(predicted_labels != true_labels) # Resubstitution Error
