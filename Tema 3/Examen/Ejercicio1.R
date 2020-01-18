#a)-----------------------------------------
rm(list = ls())
library(randomForest)
Data<- read.csv("detect-malicious-URL.csv")
Data$X<-NULL

size <- length(Data[[1]])
ind <- sample(size, size)
accuracyRF<-vector()

k <- 10
for(i in 1:k){
  ii <- floor(size/k)*(i-1)+1
  is <- floor(size/k)*i
  idt <- ind[ii : is]
  dtrain <- Data[-idt,]
  dtest <- Data[idt,]
  RandForest<-randomForest(label~.,dtrain, ntree=100,importance=T)
  predRF <- predict(RandForest, dtest)
  confusionMatrixRF<-table(predRF, dtest$label)
  accuracyRF[i]<-sum(diag(confusionMatrixRF))/sum(confusionMatrixRF)
}
cat("Media del acurracy: ",mean (accuracyRF),"\n")

#b)----------------------------------------------

library(rpart)
library(nnet)
library(e1071)
ind <- sample(size,size)
idt <- ind[1:10]
dtrain <- Data[-idt,]
dtest <- Data[idt,]
dtestM<-dtest
perceptron<-nnet(label~., dtrain, size=15,maxit=100)
arbol<-rpart(label~., dtrain)
SVMradial<-svm(label~.,dtrain)
pper<-predict(perceptron,dtest,type="class")
par<-predict(arbol,dtest,type="class")
svmpred<-predict(SVMradial,dtest,type="class")
pper
par
svmpred
dtestM$label<-NULL
dtestM <- rbind(dtestM,dtestM,dtestM)
dtestM<-data.frame(dtestM,label=c(as.character(par) ,pper,as.character(svmpred) ))
dtestM
fitSVM<-svm(label~., dtestM )
prediccionGlobal<-predict(fitSVM,dtest,type="class")
matrizconfusion<-table(prediccionGlobal, dtest$label)
matrizconfusion
accuracy<-sum (diag(matrizconfusion))/sum (matrizconfusion)
cat("Acurracy: ",accuracy,"\n")
