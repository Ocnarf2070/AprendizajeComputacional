# VALIDACION CRUZADA

# PACKAGE Y DATA SET
# ----------------------------------------------------------------------------- 
library(C50) 
library(rpart) 
data(churn)
Variables  <- c(4, 7, 16, 19, 17, 20)  
datos      <- churnTrain[, Variables]  
datos      <- rbind(datos, churnTest[, Variables]) 


# FOLDS
# ------------------------------------------------------------------------------- 
set.seed(1)
Folds         <- 10            
datos$kfold   <- sample(1:Folds, nrow(datos), replace = T)


# ITERACION MODELOS + PREDICCION
# -------------------------------------------------------------------------------- 
Iter   <- data.frame(iteracion = NULL, aciertos = NULL)
for (i in 1:Folds)
{
  Test          <- subset(datos, kfold  == i)
  Entrenamiento <- subset(datos, !kfold == i) 
  Modelo        <- rpart(churn ~ .,data = Entrenamiento)       
  Prediccion    <- predict(Modelo, Test, type = "class")  
  MC            <- table(Test[, "churn"],Prediccion)           
  Aciertos      <- MC[1, 1] / (MC[1, 1] + MC[2, 1])
  Iter          <- rbind(Iter, data.frame(Iter = i, acierto = Aciertos))  
}


# GRAFICO
# -------------------------------------------------------------------------------- 
promedio  <- format(mean(Iter$acierto, na.rm=TRUE)*100,digits = 4)
plot(Iter,type = "b", main = "% Prediccion en Cada Iteracion",,  
     cex.axis = .7,cex.lab = .7,cex.main = .8, 
     xlab ="No. de Iteraciones", ylab="% Prediccion")
abline(h = mean(Iter$acierto), col = "blue", lty = 2)
legend("topright", legend = paste("Eficiencia de Prediccion =", promedio, "%"),
       col = "blue", lty = 2, lwd = 1, cex=.7, bg=NULL)

