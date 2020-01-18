rm(list = ls())
set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)
dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
plot(svmfit, dat)
x.svm <-x [svmfit$index,]
w <- crossprod(x.svm, svmfit$coefs)
w0 <- svmfit$rho
abline(w0/w[2], -w[1]/w[2]) 
abline((w0 - 1) / w[2], -w[1] / w[2], lty = 2)
abline((w0 + 1) / w[2], -w[1] / w[2], lty = 2)


SupportVectorMachine2VarLin <- function(Data,pred){
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
  # plot(x, pch = pchn, bg = colores)
  formula<-y~x1+x2
  
  svm.prog <- svm (formula, data=Data,type = 'C-classification',kernel = "linear", cost = 1000, scale = FALSE)
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
  #Con toda esta informacion es posible calcular el hiperplano
  x.svm <-x [svm.prog$index,]
  w <- crossprod(x.svm, svm.prog$coefs)
  w0 <- svm.prog$rho
  text<-sprintf("Hiperplanos:\n%.5fx+%.5fy+%.2f=0\n",w[1],w[2],-w0)
  cat(text)
  text<-sprintf("%.5fx+%.5fy+%.2f=-1\n",w[1],w[2],-w0)
  cat(text)
  text<-sprintf("%.5fx+%.5fy+%.2f=1\n",w[1],w[2],-w0)
  cat(text)
  text<-sprintf("Ancho de banda: %.5f\n",2/norm(w,type = '2'))
  cat(text)
  
  lim<-x+c(-2,2)
  
  plot(x, pch = pchn, bg = colores,xlim = c(min(lim[,1]), max(lim[,1])), ylim =c(min(lim[,2]), max(lim[,2])) )
  
  abline(w0/w[2], -w[1]/w[2]) 
  abline((w0 - 1) / w[2], -w[1] / w[2], lty = 2)
  abline((w0 + 1) / w[2], -w[1] / w[2], lty = 2)
  
  show(predict(svm.prog,pred)[1:ncol(pred)])
}
