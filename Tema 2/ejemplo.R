library(e1071)
library(rpart)
library(nnet)
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

#calcular CP

idx <- which.min(tree$cptable[,"xerror"])
cpx <- tree$cptable[idx,"CP"]
cpx

#Perceptron multicapa
#decay incremento de los pesos
#size capa oculta
#numeromaximo de iteraciones
#size si es necesario poner un valor
nn.fit <- nnet(Species~.,data=dtrain, size= 1, maxit= 1000, decay = 5e-4)
pred_perceptron <- predict(nn.fit, dtest, type = "class")

matrizconfusionNNET <- table(dtest$Species,pred_perceptron)

accuracyNNET<-sum(diag(matrizconfusionNNET))/sum(matrizconfusionNNET)

#bucle for para ver con cuántas capas es mejor el accuracy 

neurona = 1
neuronas_mejor_ac = 1
accuracyAnterior = 0
for (neurona in 1:10) {
  nn.fit <- nnet(Species~.,data=dtrain, size= neurona, maxit= 1000, decay = 5e-4)
  pred_perceptron <- predict(nn.fit, dtest, type = "class")
  
  matrizconfusionNNET <- table(dtest$Species,pred_perceptron)
  
  accuracyNNET<-sum(diag(matrizconfusionNNET))/sum(matrizconfusionNNET)
  accuracyAnterior
  accuracyNNET
  if (accuracyAnterior<accuracyNNET){
    neuronas_mejor_ac<- neurona
    accurayAnterior <- accuracyNNET
  }
}

neuronas_mejor_ac
accuracyAnterior