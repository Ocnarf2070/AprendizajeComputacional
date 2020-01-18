rm(list = ls())
library(stats)
library(pROC)
library(nnet)
library(e1071)
library(rpart)
DataTrain <- read.csv("Titanic/train.csv")
DataTest <-  read.csv("Titanic/test.csv")
DataTrain$PassengerId = NULL
DataTrain$Name = NULL
DataTrain$Cabin = NULL
DataTrain$Ticket = NULL

DataTrain <- DataTrain[!(is.na(DataTrain$Pclass)),]
DataTrain <- DataTrain[!(is.na(DataTrain$Age)),]
DataTrain <- DataTrain[!(is.na(DataTrain$Embarked) | DataTrain$Embarked==""),]
DataTrain <- DataTrain[!(is.na(DataTrain$SibSp)),]
DataTrain <- DataTrain[,c(2:ncol(DataTrain),1)]
# View(DataTrain)

DataTest <- read.csv("Titanic/test.csv")
DataTest$PassengerId = NULL
DataTest$Name = NULL
DataTest$Cabin = NULL
DataTest$Ticket = NULL

DataTest <- DataTest[!(is.na(DataTest$Pclass)),]
DataTest <- DataTest[!(is.na(DataTest$Age)),]
DataTest <- DataTest[!(is.na(DataTest$Embarked)),]
DataTest <- DataTest[!(is.na(DataTest$SibSp)),]
# View(DataTest)

size <- length(DataTrain[[1]])
ind <- sample(size, size)
accuracyTree<-vector()
rocTree <- vector()
accuracySVM<-vector()
rocSVM <- vector()
accuracyNeural<-vector()
rocNeural <- vector()
k <- 10
#Perform k fold cross validation
for(i in 1:k){
  ii <- floor(size/k)*(i-1)+1
  is <- floor(size/k)*i
  idt <- ind[ii : is]
  dtrain <- DataTrain[-idt,]
  dtest <- DataTrain[idt,]
  
  #Pruned decision tree
  tree <- rpart(Survived ~ ., dtrain)
  optCpIdx <- which.min(tree$cptable[,"xerror"])
  optCp <- tree$cptable[optCpIdx, "CP"]
  tree<-prune (tree, cp=optCp)
  
  #SVM
  svm <- svm(Survived~ ., dtrain)
  
  #Neural network with 3 neurons 
  neural <- nnet(Survived ~ ., data = dtrain, size=3, maxit=1000, decay=5e-4,trace = FALSE);
  
  #Acurracy and ROC of the tree
  predTree <- predict(tree, dtest)
  predTree <- round(predTree)
  confusionMatrixTree<-table(predTree, dtest$Survived)
  accuracyTree[i]<-sum(diag(confusionMatrixTree))/sum(confusionMatrixTree)
  rocTree[i] <- auc((dtest$Survived==1)*1, predTree)
  
  #Acurracy and ROC of SVM
  predSVM <- predict(svm, dtest)
  predSVM <- round(predSVM)
  confusionMatrixSVM<-table(predSVM, dtest$Survived)
  accuracySVM[i]<-sum(diag(confusionMatrixSVM))/sum(confusionMatrixSVM)
  rocSVM[i] <- auc((dtest$Survived==1)*1, predSVM)
  
  #Acurracy and ROC of the neural network
  predNeural <- predict(neural, dtest)
  predNeural <- round(predNeural)
  confusionMatrixNeural<-table(predNeural, dtest$Survived)
  accuracyNeural[i]<-sum(diag(confusionMatrixNeural))/sum(confusionMatrixNeural)
  rocNeural[i] <- auc((dtest$Survived==1)*1, predNeural)
  
}
#The mean of the acurracy of each clasificators
cat("mean rpart: ",mean (accuracyTree),"\n\r")
cat("mean SVM: ",mean (accuracySVM),"\n\r")
cat("mean Neural: ",mean (accuracyNeural),"\n\r")


# The best one is the SVM. We train it now with all our data:
svm <- svm(Survived ~ ., DataTrain)
predictions <- round(predict(svm, DataTest))
