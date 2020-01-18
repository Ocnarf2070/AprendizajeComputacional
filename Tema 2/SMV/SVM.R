SupportVectorMachine2VarLin <- function(Data){
  library(MASS)
  library(e1071)
  colnames(Data)<-c("x1","x2","y")
  x<-cbind(Data$x1, Data$x2)
  y <- Data$y
  n0 <- sum(y=='1')
  n1 <- sum(y=='-1')
  colores <- c(rep("green",n0), rep("red",n1))
  pchn <- 21
  
  # Diagrama de dispersion
  plot(x, pch = pchn, bg = colores)
  formula<-y~x1+x2
  
  svm.prog <- svm (formula, data=Data, kernel='linear', cost=C, cross=2, scale=FALSE, gamma=0.5)
  show(summary(svm.prog))
  # Indices de los vectores soporte
  show(svm.prog$index)
  # Coeficientes por los que se multiplican las observaciones para obtener 
  # el vector perpendicular al hiperplano que resuelve el problema
  coefs<-svm.prog$coefs
  show(coefs)
  # Termino independiente 
  rho<-svm.prog$rho
  show(rho)
  #Con toda esta información es posible calcular el hiperplano
  x.svm <-x [svm.prog$index,]
  w <- crossprod(x.svm, svm.prog$coefs)
  w0 <- -svm.prog$rho
  text<-sprintf("%.2fx+%.2fy+%.2f=0",w,w0)
  print(text)
  
  plot(x, pch = pchn, bg = colores)
  
  
  abline(-w0/w[2], -w[1]/w[2], lwd=2, col='blue')  
  return(svm.prog);
}

x1<-c(0,4)
x2<-c(0,4)
y<-c(1,-1)
d<-data.frame(x1,x2,y)
s<-SupportVectorMachine2VarLin(d)
p1<-c(5,1)
p2<-c(6,-4)
p<-data.frame(p1,p2)
predict(s,p)
