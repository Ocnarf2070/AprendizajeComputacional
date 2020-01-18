library(e1071)
library(rpart)
set.seed(100)
ind <- sample(150, 150)
idt <- ind[1:10]
dtrain <- iris[-idt,]
dtest <- iris[idt,]
m1 <- svm(Species ~ ., data=dtrain)
matrizConfusionSVM <- table(predict(m1, dtest), dtest$Species)
accuracySVM <- sum(diag(matrizConfusionSVM))/sum(matrizConfusionSVM)

tree <- rpart(Species ~ ., data=dtrain, method = "class")
t_pred <- predict(tree, dtest, type="class")

matrizConfusionRPART <- table(dtest$Species, t_pred)
accuracyRPART <- sum(diag(matrizConfusionRPART))/sum(matrizConfusionRPART)

matrizConfusionSVM
matrizConfusionRPART
accuracySVM
accuracyRPART

