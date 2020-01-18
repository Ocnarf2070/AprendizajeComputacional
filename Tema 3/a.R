rm(list = ls())
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
library(randomForest)
rf <- randomForest(Species ~ ., data=trainData, ntree=100, proximity=TRUE)
predRF <- predict(rf, newdata=testData)
confusionMatrixRF<-table(predRF, testData$Species)
accuracyRF<-sum(diag(confusionMatrixRF))/sum(confusionMatrixRF)
