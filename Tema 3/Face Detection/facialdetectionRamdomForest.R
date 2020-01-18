

library(randomForest)

library(pROC)

library(rpart)

library(adabag)

library(gbm)

#install.packages("doParallel")

#install.packages("gbm") Hay que instalarlo




data <- read.csv("training/training.csv", stringsAsFactors = F)



corregido <- na.omit(data)




im.train <- corregido$Image

corregido$Image <- NULL




library("doParallel")

registerDoParallel()




im.train <- foreach(im = im.train, .combine=rbind) %dopar% { 
  
  as.integer(unlist(strsplit(im, " ")))
  
}




im.train <- as.data.frame(im.train)

data <- cbind(corregido, im.train)




ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.8, 0.2))

train <- corregido[ind==1,]

test <- corregido[ind==2,]




## Random Forest

arbolx <- randomForest(nose_tip_x~., train,importance=TRUE)

arboly <- randomForest(nose_tip_y~., train,importance=TRUE)

importance(arbolx)

importance(arboly)




predRFx <- predict(arbolx, test)

predRFx <- as.data.frame(predRFx)

predRFy <- predict(arboly, test)

predRFy <- as.data.frame(predRFy)




cm <- cbind(predRFx, truex = test$nose_tip_x)

cm <- cbind(cm, MAPEx = (abs(cm$predRFx-cm$truex)/cm$truex)*100)




cm <- cbind(cm, predRFy)

cm <- cbind(cm, truey = test$nose_tip_y)

cm <- cbind(cm, MAPEy = (abs(cm$predRFy-cm$truey)/cm$truey)*100)




MAPE_PROMEDIORFX <- sum(cm$MAPEx)/nrow(cm)

MAPE_PROMEDIORFY <- sum(cm$MAPEy)/nrow(cm)

MAPE_PROMEDIORFX

MAPE_PROMEDIORFY




## Adaboost

adaX <- gbm(nose_tip_x~., data=train, distribution="gaussian", cv.folds = 3) # Al ser regresion el paquete adaboost no sirve

adaY <- gbm(nose_tip_y~., data=train, distribution="gaussian", cv.folds = 3) # hay que usar gbm (adaboost para regresion)




best.iterX <- gbm.perf(adaX,method="cv")

best.iterY <- gbm.perf(adaY,method="cv")




predAdax <- predict(adaX, test, best.iterX)

predAday <- predict(adaY, test, best.iterY)




predAdax <- as.data.frame(predAdax)

predAday <- as.data.frame(predAday)




cmAda <- cbind(predAdax, truex = test$nose_tip_x)

cmAda <- cbind(cmAda, MAPEx = (abs(cmAda$predAdax-cmAda$truex)/cmAda$truex)*100)




cmAda <- cbind(cmAda,predAday)

cmAda <- cbind(cmAda, truey = test$nose_tip_y)

cmAda <- cbind(cmAda, MAPEy = (abs(cmAda$predAday-cmAda$truey)/cmAda$truey)*100)




MAPE_PROMEDIOADAX <- sum(cmAda$MAPEx)/nrow(cmAda)

MAPE_PROMEDIOADAY <- sum(cmAda$MAPEy)/nrow(cmAda)

MAPE_PROMEDIOADAX

MAPE_PROMEDIOADAY

