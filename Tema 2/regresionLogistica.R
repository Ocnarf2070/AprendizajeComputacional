
#Regresion logística con validación cruzada
library(stats)
library(pROC)
library(nnet)
library(e1071)
library(rpart)
datos <- read.table("D:/Users/franc/Google Drive/Ingenieria de Informatica/Curso2018-19/Aprendizaje Computacional/Tema 2/datos_icb.txt",header = TRUE)
set.seed(1000)



  
  ind <- sample(500,500)
  aucs <- sample(10,10)

  
    ii <- 0
    is <- 50
    idt <- ind[ii:is]
    dtrain <- datos[-idt,]
    dtest <- datos[idt,]
    lr.fit <- glm(recid~., data=dtrain, family=binomial("logit"))
    plot(lr.fit,data=dtrain$recid)
    lr.pred <- predict(lr.fit,dtest,type="response")
    aucs <- auc((dtest$recid=="SI")*1, lr.pred)
    plot (aucs)
  
