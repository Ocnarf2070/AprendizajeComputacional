library(rpart)
library(nnet)
library(e1071)
library(mlbench)
library(caret)
library(caretEnsemble)
seed<-7


ind <- sample(150,150)
idt <- ind[1:10]
dtrain <- iris[-idt,]
dtest <- iris[idt,]

dtrain$Species=as.numeric(dtrain$Species)
dtest$Species=as.numeric(dtest$Species)
# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('nnet', 'svmRadial')
set.seed(seed)
models <- caretList(Species~., data=dtrain, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)


# stack using random forest
set.seed(seed)
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
stackingSVM <- caretStack(models, method="svmRadial",  trControl=stackControl)
print(stackingSVM)




prediccionGlobal<-predict(stackingSVM,dtest,type="raw")
confusionMatrix(factor(round(prediccionGlobal)),factor(dtest$Species ))
pred<-predict(adaboost,adadata)