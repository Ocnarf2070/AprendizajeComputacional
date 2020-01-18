#a)

x1=rnorm(1000)
x2=rnorm(1000)
y=2*x1+.7*x2+rnorm(1000)
df=data.frame(y,x1,x2,x3=rnorm(1000),x4=rnorm(1000),x5=rnorm(1000))
library(randomForest)
rf1 <- randomForest(y~., data=df, mtry=2,ntree=50,importance=TRUE)
importance(rf1,type=1)

#Se nos presenta un programa en el cual primeramente en x1 y x2 se introducen valores aleatorios
#que siguen una gausiana de media=0 y varianza=1. Despues y es igual que el doble de x1 mas el septuple de x2 
#mas valores random. A todo esto se lo metemos a un data frame, el cual contiene además los valores 
#aleatorios de x3, x4 y x5. Entrando en lo mas importante, se hace un random forest con 50 arboles y que nos mueste
#la importancia de los datos. Por ultimo nos muestra la importancia en el random forest que hemos hecho, el cual
#nos sale que el valor x1 tiene una gran impotancia , igual que x2, aunque x2 sea menor. Para los demas, tiene poca importancia,
#ya que se acercan a 0 los valores o son muy pequeños.

#b)-------------------------------
#Para que sea equiprobable, relativamente nadie tiene que depender
#de nadie, es decir, que sea independiente todas las variable. Por lo que 
#habra que cambiar la y, ya que depende de x1 e x2 en el codigo anterior.

x1=rnorm(1000)
x2=rnorm(1000)
y=rnorm(1000)
df=data.frame(y,x1,x2,x3=rnorm(1000),x4=rnorm(1000),x5=rnorm(1000))
library(randomForest)
rf1 <- randomForest(y~., data=df, mtry=2,ntree=50,importance=TRUE)
importance(rf1,type=1)

#Si lo ejecutamos otra vez, vemos que el importance nos devuelvge que todas las variables tienen una importancia baja, que es
#lo que queriamos conseguir
              