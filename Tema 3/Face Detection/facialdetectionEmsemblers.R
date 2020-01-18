rm(list = ls())

library(randomForest)
library(pROC)
library(rpart)
library(gbm)

data <- read.csv("training/training.csv", stringsAsFactors = F)
data.update <- na.omit(data)
im.train <- data.update$Image
data.update$Image <- NULL

library("doParallel")
registerDoParallel()

im.train <- foreach(im = im.train, .combine=rbind) %dopar% { 
  
  as.integer(unlist(strsplit(im, " ")))
  
}

im.train <- as.data.frame(im.train)
data <- cbind(data.update, im.train)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.8, 0.2))
train <- data.update[ind==1,]
test <- data.update[ind==2,]


## Random Forest
arbolx <- randomForest(nose_tip_x~., train,importance=TRUE)
arboly <- randomForest(nose_tip_y~., train,importance=TRUE)
importance(arbolx)
importance(arboly)

predRFx <- predict(arbolx, test)
predRFx <- as.data.frame(predRFx)

predRFy <- predict(arboly, test)
predRFy <- as.data.frame(predRFy)

cm_rf <- cbind(predRFx, truex = test$nose_tip_x)
cm_rf <- cbind(cm_rf, MSEx = ((cm_rf$truex-cm_rf$predRFx)^2))

cm_rf <- cbind(cm_rf, predRFy)
cm_rf <- cbind(cm_rf, truey = test$nose_tip_y)
cm_rf <- cbind(cm_rf, MSEy = ((cm_rf$truey-cm_rf$predRFy)^2))

MSE_MEAN_RFx <- mean(cm_rf$MSEx)
MSE_MEAN_RFy <- mean(cm_rf$MSEy)
cat("MSE for the nose_tip_x for Random Forest: ",MSE_MEAN_RFx,"\n")
cat("MSE for the nose_tip_y for Random Forest: ",MSE_MEAN_RFy,"\n")


##Bagging
#Because we cannot use diectly bagging from package adabag, so we will use
# the package ipred for the bagging
library(ipred)
BagX<-bagging(nose_tip_x~.,data = train)
BagY<-bagging(nose_tip_y~.,data = train)

predBagx <- predict(BagX, test)
predBagy <- predict(BagY, test)
predBagx <- as.data.frame(predBagx)
predBagy <- as.data.frame(predBagy)

cmBag <- cbind(predBagx, truex = test$nose_tip_x)
cmBag <- cbind(cmBag, MSEx = ((cmBag$truex-cmBag$predBagx)^2))
cmBag <- cbind(cmBag,predBagy)
cmBag <- cbind(cmBag, truey = test$nose_tip_y)
cmBag <- cbind(cmBag, MSEy = ((cmBag$truey-cmBag$predBagy)^2))

MSE_MEAN_BAGx <- mean(cmBag$MSEx)
MSE_MEAN_BAGy <- mean(cmBag$MSEy)
cat("MSE for the nose_tip_x for Bagging: ",MSE_MEAN_BAGx,"\n")
cat("MSE for the nose_tip_y for Bagging: ",MSE_MEAN_BAGy,"\n")


## Adaboost
# Because we cannot use directly adaboots for regretions, we must use gbm, which is the adaboots for regretions
adaX <- gbm(nose_tip_x~., data=train, distribution="gaussian", cv.folds = 3) 
adaY <- gbm(nose_tip_y~., data=train, distribution="gaussian", cv.folds = 3) 

best.iterX <- gbm.perf(adaX,method="cv")
best.iterY <- gbm.perf(adaY,method="cv")

predAdax <- predict(adaX, test, best.iterX)
predAday <- predict(adaY, test, best.iterY)
predAdax <- as.data.frame(predAdax)
predAday <- as.data.frame(predAday)

cmAda <- cbind(predAdax, truex = test$nose_tip_x)
cmAda <- cbind(cmAda, MSEx = ((cmAda$truex-cmAda$predAdax)^2))
cmAda <- cbind(cmAda,predAday)
cmAda <- cbind(cmAda, truey = test$nose_tip_y)
cmAda <- cbind(cmAda, MSEy = ((cmAda$truey-cmAda$predAday)^2))

MSE_MEAN_ADAx <- mean(cmAda$MSEx)
MSE_MEAN_ADAy <- mean(cmAda$MSEy)
cat("MSE for the nose_tip_x for Adaboost: ",MSE_MEAN_ADAx,"\n")
cat("MSE for the nose_tip_y for Adaboost: ",MSE_MEAN_ADAy,"\n")

##Stacking
#We will use linear regretion and general lineal regretion for the stacking, which
# later we use a SMV with this stacking.
library(e1071)

lrX<-lm(nose_tip_x~.,data = train)
glrX<-glm(nose_tip_x~.,data = train)

testMX<-test
testMX$nose_tip_x<-NULL
plrX<-predict(lrX,new=testMX)
pglrX<-predict(glrX,new=testMX)

testMX<-rbind(testMX,testMX)
testMX<-data.frame(testMX,nose_tip_x=c(plrX,pglrX) )

testMY<-test
lrY<-lm(nose_tip_y~.,data = train)
glrY<-glm(nose_tip_y~.,data = train)
plrY<-predict(lrY,data=test)
pglrY<-predict(glrY,data=test)

testMY<-test
testMY$nose_tip_y<-NULL
plrY<-predict(lrY,new=testMY)
pglrY<-predict(glrY,new=testMY)

testMY<-rbind(testMY,testMY)
testMY<-data.frame(testMY,nose_tip_y=c(plrY,pglrY) )


StackSVM_X<-svm(nose_tip_x~.,data = testMX)
StackSVM_Y<-svm(nose_tip_y~.,data = testMY)

predStackx <- predict(StackSVM_X, test)
predStacky <- predict(StackSVM_Y, test)
predStackx <- as.data.frame(predStackx)
predStacky <- as.data.frame(predStacky)

cmStack <- cbind(predStackx, truex = test$nose_tip_x)
cmStack <- cbind(cmStack, MSEx = ((cmStack$truex-cmStack$predStackx)^2))
cmStack <- cbind(cmStack,predStacky)
cmStack <- cbind(cmStack, truey = test$nose_tip_y)
cmStack <- cbind(cmStack, MSEy = ((cmStack$truey-cmStack$predStacky)^2))

MSE_MEAN_STACKx <- mean(cmStack$MSEx)
MSE_MEAN_STACKy <- mean(cmStack$MSEy)
cat("MSE for the nose_tip_x for Stacking SVM: ",MSE_MEAN_STACKx,"\n")
cat("MSE for the nose_tip_y for Stacking SVM: ",MSE_MEAN_STACKy,"\n")

