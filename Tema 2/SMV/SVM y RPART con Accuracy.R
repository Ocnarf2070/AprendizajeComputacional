library(e1071)
library(rpart)
set.seed(100)
ind <- sample(150,150)
idt <- ind[1:10]
dtrain <- iris[-idt,]
dtest <- iris[idt,]
m1 <- svm(Species ~ ., data = dtrain)
matrizconfusionSVM<-table(predict(m1,dtest), dtest$Species, dnn=c("Prediction", "Actual"))
accuracySVM<- sum(diag(matrizconfusionSVM))/sum(matrizconfusionSVM)

tree<-rpart (Species ~ ., data = dtrain, method="class")
t_pred = predict(tree,dtest,type="class")

matrizconfusionRPART <- table(dtest$Species, t_pred)
accuracyRPART<- sum(diag(matrizconfusionRPART))/sum(matrizconfusionRPART)

matrizconfusionSVM
matrizconfusionRPART
accuracySVM
accuracyRPART