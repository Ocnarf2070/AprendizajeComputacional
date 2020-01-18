library(MASS)
library(e1071)
R.Programas <- read.csv("D:/Users/franc/Google Drive/Ingenieria de Informatica/Curso2018-19/Aprendizaje Computacional/Tema 2/SMV/R Programas.data", header=FALSE)


x <- cbind(R.Programas$V8, R.Programas$V9)
y <- R.Programas$V2
n0 <- sum(y=='M')
n1 <- sum(y=='B')
# Para que los graficos queden mas bonitos (rojo = maligno, verde = benigno)
colores <- c(rep("green",n0), rep("red",n1))
pchn <- 21

# Diagrama de dispersion
plot(x, pch = pchn, bg = colores, xlab='smoothness', ylab='concavepoints')

C <- 1000
svm.lineal <- svm (R.Programas$V2~R.Programas$V8+R.Programas$V9, data= R.Programas, kernel='linear', cost=C, cross=2, scale=FALSE, gamma=0.5)
summary(svm.lineal)
# Indices de los vectores soporte
svm.lineal$index
# Coeficientes por los que se multiplican las observaciones para obtener 
# el vector perpendicular al hiperplano que resuelve el problema
svm.lineal$coefs
# Termino independiente 
svm.lineal$rho
#Con toda esta información es posible calcular el hiperplano 
x.svm <-x [svm.lineal$index,]
w <- crossprod(x.svm, svm.lineal$coefs)
w0 <- -svm.lineal$rho

plot(x, pch = pchn, bg = c(rep("green",n0), rep("red",n1)), xlab='smoothness', ylab='concavepoints')


abline(-w0/w[2], w[1]/w[2], lwd=2, col='blue')  
