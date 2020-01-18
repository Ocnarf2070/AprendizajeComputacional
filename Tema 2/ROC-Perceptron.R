library(stats)
library(pROC)
library(nnet)
library(e1071)
library(rpart)
datos_icb <- read.table("D:/Users/franc/Google Drive/Ingenieria de Informatica/Curso2018-19/Aprendizaje Computacional/Tema 2/datos_icb.txt",header=T)
ind <- sample(500,500)
ii <- 1
is <- 10
idt <- ind[ii:is] 
dtrain <- datos_icb[-idt,]
dtest <- datos_icb[idt,]
lr.fit <- nnet(recid~., data=dtrain, size=5, maxit=500, decay=1, trace=FALSE)
lr.pred <- predict(lr.fit,dtest,type="raw")
aucs <- auc((dtest$recid=="SI")*1, lr.pred)
obj.roc<-roc((dtest$recid=="SI")*1,lr.pred )
plot(obj.roc)
aucs 
D:/Users/franc/Google Drive/Ingenieria de Informatica/Curso2018-19/Aprendizaje Computacional/Tema 2/