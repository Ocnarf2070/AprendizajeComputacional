library(rpart)
library(nnet)
library(e1071)
ind <- sample(150,150)
idt <- ind[1:10]
dtrain <- iris[-idt,]
dtest <- iris[idt,]
dtestM<-dtest
perceptron<-nnet(Species~., dtrain, size=3)
arbol<-rpart(Species~., dtrain)
pper<-predict(perceptron,dtest,type="class")
par<-predict(arbol,dtest,type="class")
pper
par
dtestM$Species<-NULL
dtestM <- rbind(dtestM,dtestM)
dtestM<-data.frame(dtestM,Species=c(as.character(par) ,pper) )
dtestM
fitSVM<-svm(Species~., dtestM )
prediccionGlobal<-predict(fitSVM,dtest,type="class")
matrizconfusion<-table(prediccionGlobal, dtest$Species)
matrizconfusion
accuracy<-sum (diag(matrizconfusion))/sum (matrizconfusion)
accuracy
