#a)Utilizamos el algorimo de ADABOOSTING
rm(list = ls())
n<-10
d<-rep(1,n)*1/n;
t<-2
#Ya sabemos a priori que elementos estaran mal clasificados
miss_index<-list();
miss_index[[1]]<-c(2,4,7,9)
miss_index[[2]]<-c(2,5,6,9)
#Ejecución del algoritmo
for (i in 1:t){
  #dn
  cat(sprintf("d%d:\t",i))
  cat(sprintf("%.4f\t",d))
  index <- miss_index[[i]]
  #Error
  error <- sum(d[index])
  cat(sprintf(" error%d: %.4f\n",i,error));
  #---------------------------------------------------------
  #ALPHA
  #---------------------------------------------------------
  alpha=(1/2)*log((1-error)/error);
  cat(sprintf("ALPHA%d: %.4f\n",i,alpha));
  #---------------------------------------------------------
  #Preparacion de la siguiente iteracion
  y_h<-rep(1,n)
  y_h[index]=-1
  e<-exp(-alpha*y_h)
  cat("e:\t")
  cat(sprintf("%.4f\t",e))
  cat("\n")
  d_e<-d*e
  z<-sum(d_e)
  cat(sprintf("d%de:\t",i))
  cat(sprintf("%.4f\t",d_e))
  d<-d_e/z
  cat(sprintf(" Z%d: %.4f\n",i,z));
}

#b)
rm(list = ls())
X1<-c(20,30,25,34,31,70,80,74,87,87)
X2<-c(77,73,63,30,20,76,70,51,37,15)
Y<-c(1,-1,1,-1,1,-1,1,-1,1,-1)
Data<- data.frame(X1,X2,Y)
library(adabag);
adaboost <- boosting(Y~X1+X2, data=Data, boos=T, mfinal=5,coeflearn='Breiman')

#c)
c1<-c(0,0,100,100,50)
c2<-c(0,100,0,100,50)
Points<-data.frame(c1,c2)
names(Points)<-c("X1","X2")
cat("Prediccion de puntos:\n")
pred<-predict(adaboost,Points)
for (i in 1:length(c1)){
  text<-sprintf("(%s) => %s\n",paste(Test1[i,], collapse=", "),pred[i])
  cat(text)
}
