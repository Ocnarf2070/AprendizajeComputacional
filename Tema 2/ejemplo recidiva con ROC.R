library(pROC)
library(rpart)
datos <-  read.table("C:/datos_icb.txt", header=T, quote="\"")
set.seed(100)
ind <- sample(500,500)
idt <- ind[1:10]
dtrain <- datos[-idt,]
dtest <- datos[idt,]
lr.fit <- rpart(recid~., data=dtrain)

plot(lr.fit , compress=TRUE)
text(lr.fit , use.n=TRUE)
lr.pred <- predict(lr.fit,dtest,type="prob")

ROC<-auc((dtest$recid=="SI")*1, lr.pred[,1])
ROC
obj.roc<-roc((dtest$recid=="SI")*1,lr.pred[,1])

plot(obj.roc)  

