rm(list = ls())
library(MASS)
library(e1071)

x1<-c(1,3,1,3,2,3,4)
x2<-c(1,3,3,1,2.5,2.5,3)
y<-c(-1,1,1,-1,1,-1,-1)
dataA<-data.frame(x1,x2,y)
names(dataA)<-c("X1","X2","Y")
#Linealmente separable

x1<-c(1,3,1,3,2,3,4,1.5,1)
x2<-c(1,3,3,1,2.5,2.5,3,1.5,2)
y<-c(-1,1,1,-1,1,-1,-1,1,-1)
dataB<-data.frame(x1,x2,y)
names(dataB)<-c("X1","X2","Y")
#No linealmente separable

C<-1000
myFormula<-Y~.
svm.lineal <- svm(myFormula,data=dataA,kernel='linear',cross=2, scale=FALSE, gamma=0.5)
# Indices de los vectores soporte
svm.lineal$index
# Coeficientes por los que se multiplican las observaciones para obtener 
# el vector perpendicular al hiperplano que resuelve el problema
svm.lineal$coefs
# Termino independiente 
svm.lineal$rho
#Con toda esta informacion es posible calcular el hiperplano 
x <- cbind(dataA$X1, dataA$X2)
x.svm <-x [svm.lineal$index,]
w <- crossprod(x.svm, svm.lineal$coefs);w
ancho<-1/sqrt(sum(w*w));ancho
w0 <- svm.lineal$rho;w0
print(c(w[0],"+",w[1],"=0"))

plot(x)


abline(w0/w[2], -w[1]/w[2], lwd=2, col='blue') 



