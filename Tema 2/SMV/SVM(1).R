rm(list = ls())
library(MASS)
library(e1071)
#a)--------------------------------------------------
x1<-c(0,4)
x2<-c(0,4)
y<-c(1,-1)
Data1<-data.frame(x1,x2,y)

x<-cbind(Data1$x1, Data1$x2)
y <- Data1$y
n0 <- sum(y=='1')
n1 <- sum(y=='-1')
colores <- c(rep("green",n0), rep("red",n1))
pchn <- 21

# Diagrama de dispersion
plot(x, pch = pchn, bg = colores)

#Formula, SVM y datos importantes
formula<-as.factor(y)~.
svm.lineal1 <- svm (formula, data=Data1,kernel = "linear", cost = 1000, scale = FALSE)
x.svm <-x [svm.lineal1$index,]
w <- crossprod(x.svm, svm.lineal1$coefs)
w0 <- svm.lineal1$rho


#Vectores Soporte
cat("Vectores Soporte (W):\n")
text<-sprintf("(%s)\n",paste(x.svm[1,], collapse=", "))
cat(text)
text<-sprintf("(%s)\n",paste(x.svm[2,], collapse=", "))
cat(text)

#Valores del kernel
cat("Valores del kernel\n")
show( x.svm%*%t(x.svm) )

#Ancho de banda
text<-sprintf("Ancho de banda: %.5f\n",2/norm(w,type = '2'))
cat(text)

#Vector de pesos normal al hiperplano
text<-sprintf("Vector de pesos normal al hiperplano (W): (%s)\n",paste(w, collapse=", "))
cat(text)

# Termino independiente 
text<-sprintf("Termino independiente (B): %.2f\n",w0)
cat(text)

#Ecuaciones de hiperplano
text<-sprintf("Hiperplanos:\n%.5fx+%.5fy+%.2f=0\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=-1\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=1\n",w[1],w[2],-w0)
cat(text)

lim<-x+c(-2,2)

plot(x, pch = pchn, bg = colores,xlim = c(min(lim[,1]), max(lim[,1])), ylim =c(min(lim[,2]), max(lim[,2])) )

abline(w0/w[2], -w[1]/w[2]) 
abline((w0 - 1) / w[2], -w[1] / w[2], lty = 2)
abline((w0 + 1) / w[2], -w[1] / w[2], lty = 2)

#Puntos
c1<-c(5,1)
c2<-c(6,-4)
Test1<-data.frame(c1,c2)
names(Test1)<-c("x1","x2");

#Prediccion de los puntos
cat("Predicción de puntos:\n")
pred<-predict(svm.lineal1,Test1)
text<-sprintf("(%s) => %s\n",paste(Test1[1,], collapse=", "),pred[1])
cat(text)
text<-sprintf("(%s) => %s\n",paste(Test1[2,], collapse=", "),pred[2])
cat(text)

readline(prompt="Press [enter] to continue")

#b)--------------------------------------------------
x1<-c(2,0,1)
x2<-c(0,0,1)
y<-c(1,-1,-1)
Data2<-data.frame(x1,x2,y)

x<-cbind(Data2$x1, Data2$x2)
y <- Data2$y
n0 <- sum(y=='1')
n1 <- sum(y=='-1')
colores <- c(rep("green",n0), rep("red",n1))
pchn <- 21

# Diagrama de dispersion
plot(x, pch = pchn, bg = colores)

#Formula, SVM y datos importantes
formula<-as.factor(y)~.
svm.lineal2 <- svm (formula, data=Data2,kernel = "linear", cost = 1000, scale = FALSE)
x.svm <-x [svm.lineal2$index,]
w <- crossprod(x.svm, svm.lineal2$coefs)
w0 <- svm.lineal2$rho


#Vectores Soporte
cat("Vectores Soporte (W):\n")
text<-sprintf("(%s)\n",paste(x.svm[1,], collapse=", "))
cat(text)
text<-sprintf("(%s)\n",paste(x.svm[2,], collapse=", "))
cat(text)

#Valores del kernel
cat("Valores del kernel\n")
show( x.svm%*%t(x.svm) )

#Ancho de banda
text<-sprintf("Ancho de banda: %.5f\n",2/norm(w,type = '2'))
cat(text)

#Vector de pesos normal al hiperplano
text<-sprintf("Vector de pesos normal al hiperplano (W): (%s)\n",paste(w, collapse=", "))
cat(text)

# Termino independiente 
text<-sprintf("Termino independiente (B): %.2f\n",w0)
cat(text)

#Ecuaciones de hiperplano
text<-sprintf("Hiperplanos:\n%.5fx+%.5fy+%.2f=0\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=-1\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=1\n",w[1],w[2],-w0)
cat(text)

lim<-x+c(-2,2)

plot(x, pch = pchn, bg = colores,xlim = c(min(lim[,1]), max(lim[,1])), ylim =c(min(lim[,2]), max(lim[,2])) )

abline(w0/w[2], -w[1]/w[2]) 
abline((w0 - 1) / w[2], -w[1] / w[2], lty = 2)
abline((w0 + 1) / w[2], -w[1] / w[2], lty = 2)

#Puntos
c1<-c(5,1)
c2<-c(6,-4)
Test2<-data.frame(c1,c2)
names(Test2)<-c("x1","x2");

#Prediccion de los puntos
cat("Predicción de puntos:\n")
pred<-predict(svm.lineal2,Test2)
text<-sprintf("(%s) => %s\n",paste(Test2[1,], collapse=", "),pred[1])
cat(text)
text<-sprintf("(%s) => %s\n",paste(Test2[2,], collapse=", "),pred[2])
cat(text)

readline(prompt="Press [enter] to continue")

#c)--------------------------------------------------
x1<-c(2,2,-2,-2,2,2,-2,-2,1,1,-1,-1)
x2<-c(2,-2,-2,2,2,-2,-2,2,1,-1,-1,1)
y<-c(1,1,1,1,1,1,1,1,-1,-1,-1,-1)
Data3<-data.frame(x1,x2,y)
x<-cbind(Data3$x1, Data3$x2)
y <- Data3$y
n0 <- sum(y=='1')
n1 <- sum(y=='-1')
colores <- c(rep("green",n0), rep("red",n1))
pchn <- 21

# Diagrama de dispersion
plot(x, pch = pchn, bg = colores)

#Formula, SVM y datos importantes
formula<-as.factor(y)~.
#Como no es lineal, utilizaremos otro kernel
svm.pol <- svm (formula, data=Data3,kernel = "radial", cost = 1000, scale = FALSE)
x.svm <-x [svm.pol$index,]
w <- crossprod(as.matrix(x.svm),as.matrix(svm.pol$coefs)) 
w0 <- svm.pol$rho

#Vectores Soporte
cat("Vectores Soporte (W):\n")
for(i in 1:nrow(x.svm)){
  text<-sprintf("(%s)\n",paste(x.svm[i,], collapse=", "))
  cat(text)
}

#Valores del kernel
cat("Valores del kernel\n")
show( x.svm%*%t(x.svm) )

#Ancho de banda
text<-sprintf("Ancho de banda: %.5f\n",2/norm(w,type = '2'))
cat(text)

#Vector de pesos normal al hiperplano
text<-sprintf("Vector de pesos normal al hiperplano (W): (%s)\n",paste(w, collapse=", "))
cat(text)

# Termino independiente 
text<-sprintf("Termino independiente (B): %.2f\n",w0)
cat(text)

#Ecuaciones de hiperplano
cat("Hiperplanos:\n")
text<-sprintf("%.5fx+%.5fy+%.2f=0\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=-1\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=1\n",w[1],w[2],-w0)
cat(text)

lim<-x+c(-2,2)

plot(svm.pol,Data3,xlim = c(min(lim[,1]), max(lim[,1])), ylim =c(min(lim[,2]), max(lim[,2])) )

readline(prompt="Press [enter] to continue")

#d)--------------------------------------------------
#Transformation
x1<-c(2,2,-2,-2,2,2,-2,-2,1,1,-1,-1)
x2<-c(2,-2,-2,2,2,-2,-2,2,1,-1,-1,1)
y<-c(1,1,1,1,1,1,1,1,-1,-1,-1,-1)
for(i in 1:length(x1)){
  if(sqrt(x1[i]^2+x2[i]^2)>2){
    x1[i]<-4-x2[i]+abs(x1[i]-x2[i])
    x2[i]<-4-x1[i]+abs(x1[i]-x2[i])
  }
}

Data4<-data.frame(x1,x2,y)

x<-cbind(Data4$x1, Data4$x2)
y <- Data4$y
n0 <- sum(y=='1')
n1 <- sum(y=='-1')
colores <- c(rep("green",n0), rep("red",n1))
pchn <- 21

# Diagrama de dispersion
plot(x, pch = pchn, bg = colores)

#Formula, SVM y datos importantes
formula<-as.factor(y)~.
svm.lineal3 <- svm (formula, data=Data4,kernel = "linear", cost = 1000, scale = FALSE)
x.svm <-x [svm.lineal3$index,]
w <- crossprod(x.svm, svm.lineal3$coefs)
w0 <- svm.lineal3$rho


#Vectores Soporte
cat("Vectores Soporte (W):\n")
for(i in 1:nrow(x.svm)){
  text<-sprintf("(%s)\n",paste(x.svm[i,], collapse=", "))
  cat(text)
}

#Valores del kernel
cat("Valores del kernel\n")
show( x.svm%*%t(x.svm) )

#Ancho de banda
text<-sprintf("Ancho de banda: %.5f\n",2/norm(w,type = '2'))
cat(text)

#Vector de pesos normal al hiperplano
text<-sprintf("Vector de pesos normal al hiperplano (W): (%s)\n",paste(w, collapse=", "))
cat(text)

# Termino independiente 
text<-sprintf("Termino independiente (B): %.2f\n",w0)
cat(text)

#Ecuaciones de hiperplano
text<-sprintf("Hiperplanos:\n%.5fx+%.5fy+%.2f=0\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=-1\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=1\n",w[1],w[2],-w0)
cat(text)

lim<-x+c(-2,2)

plot(x, pch = pchn, bg = colores,xlim = c(min(lim[,1]), max(lim[,1])), ylim =c(min(lim[,2]), max(lim[,2])) )

abline(w0/w[2], -w[1]/w[2]) 
abline((w0 - 1) / w[2], -w[1] / w[2], lty = 2)
abline((w0 + 1) / w[2], -w[1] / w[2], lty = 2)

readline(prompt="Press [enter] to continue")


#e)--------------------------------------------------
x1<-c(3,3,6,6,1,0,0,-1)
x2<-c(1,-1,1,-1,0,1,-1,0)
y<-c(1,1,1,1,-1,-1,-1,-1)
Data5<-data.frame(x1,x2,y)

x<-cbind(Data5$x1, Data5$x2)
y <- Data5$y
n0 <- sum(y=='1')
n1 <- sum(y=='-1')
colores <- c(rep("green",n0), rep("red",n1))
pchn <- 21

# Diagrama de dispersion
plot(x, pch = pchn, bg = colores)

#Formula, SVM y datos importantes
formula<-as.factor(y)~.
svm.lineal4 <- svm (formula, data=Data5,kernel = "linear", cost = 1000, scale = FALSE)
x.svm <-x [svm.lineal4$index,]
w <- crossprod(x.svm, svm.lineal4$coefs)
w0 <- svm.lineal4$rho


#Vectores Soporte
cat("Vectores Soporte (W):\n")
for(i in 1:nrow(x.svm)){
  text<-sprintf("(%s)\n",paste(x.svm[i,], collapse=", "))
  cat(text)
}

#Valores del kernel
cat("Valores del kernel\n")
show( x.svm%*%t(x.svm) )

#Ancho de banda
text<-sprintf("Ancho de banda: %.5f\n",2/norm(w,type = '2'))
cat(text)

#Vector de pesos normal al hiperplano
text<-sprintf("Vector de pesos normal al hiperplano (W): (%s)\n",paste(w, collapse=", "))
cat(text)

# Termino independiente 
text<-sprintf("Termino independiente (B): %.2f\n",w0)
cat(text)

#Ecuaciones de hiperplano
text<-sprintf("Hiperplanos:\n%.5fx+%.5fy+%.2f=0\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=-1\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=1\n",w[1],w[2],-w0)
cat(text)

lim<-x+c(-2,2)

plot(x, pch = pchn, bg = colores,xlim = c(min(lim[,1]), max(lim[,1])), ylim =c(min(lim[,2]), max(lim[,2])) )

abline(w0/w[2], -w[1]/w[2]) 
abline((w0 - 1) / w[2], -w[1] / w[2], lty = 2)
abline((w0 + 1) / w[2], -w[1] / w[2], lty = 2)

#Puntos
c1<-c(4)
c2<-c(5)
Test5<-data.frame(c1,c2)
names(Test5)<-c("x1","x2");

#Prediccion de los puntos
cat("Predicción de puntos:\n")
pred<-predict(svm.lineal4,Test5)
text<-sprintf("(%s) => %s\n",paste(Test2[1,], collapse=", "),pred[1])
cat(text)

readline(prompt="Press [enter] to continue")

#f)--------------------------------------------------
data(iris)
x <- subset(iris, select=-Species)
y <- iris$Species

n0 <- sum(y=='setosa')
n1 <- sum(y=='versicolor')
n2 <- sum(y=='virginica')
colores <- c(rep("green",n0), rep("red",n1),rep("purple",n2))
pchn <- 21

# Diagrama de dispersion
plot(x, pch = pchn, bg = colores)

#Formula, SVM y datos importantes
svm.lineal5 <- svm (x,y,kernel = "linear")
x.svm <-x [svm.lineal5$index,]
w <- crossprod(as.matrix(x.svm),as.matrix(svm.lineal5$coefs)) 
w0 <- svm.lineal5$rho


#Vectores Soporte
for(i in 1:nrow(x.svm)){
  text<-sprintf("(%s)\n",paste(x.svm[i,], collapse=", "))
  cat(text)
}
#Valores del kernel
cat("Valores del kernel\n")
xs<-as.matrix(x.svm)
show( xs%*%t(xs) )

#Ancho de banda
text<-sprintf("Ancho de banda: %.5f\n",2/norm(w,type = '2'))
cat(text)

#Vector de pesos normal al hiperplano
text<-sprintf("Vector de pesos normal al hiperplano (W): (%s)\n",paste(w, collapse=", "))
cat(text)

# Termino independiente 
text<-sprintf("Termino independiente (B): %.2f\n",w0)
cat(text)

#Ecuaciones de hiperplano
cat("Hiperplanos:\n")
text<-sprintf("%.5fx+%.5fy+%.2f=0\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=-1\n",w[1],w[2],-w0)
cat(text)
text<-sprintf("%.5fx+%.5fy+%.2f=1\n",w[1],w[2],-w0)
cat(text)

readline(prompt="Press [enter] to continue")
plot(x$Petal.Length, x$Petal.Width, pch = pchn, bg = colores)
abline(w0/w[2], -w[1]/w[2]) 
abline((w0 - 1) / w[2], -w[1] / w[2], lty = 2)
abline((w0 + 1) / w[2], -w[1] / w[2], lty = 2)
#Ejemplo
svm.lineal5 <- svm(Species ~ Petal.Length + Petal.Width, data = iris, kernel = "linear")
plot(svm.lineal5, iris, Petal.Width ~ Petal.Length, 
     slice = list(sepal.width = 1, sepal.length = 2))


