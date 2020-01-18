rm(list = ls())
library(rpart)
datos<- read.csv("datos_icb.txt", sep="")
# Generamos Ã¡rboles sobre el dataset
inicio = 0.6
limite = 0.95
pasos = 6
tamTrain=0.8
clasificadores = list();
for(i in 0:5){
  set.seed(15*i);
  ind<- sample(500,500)
  idt <- ind[1:25]
  cat(idt)
  cat("\n")
  dtrain <- datos[-idt,]
  dtest <- datos[idt,]
  clasificadores[[i+1]] = rpart(recid ~., data = dtrain, control = rpart.control(minsplit = 10))
}
set.seed(Sys.Date());
# Elegimos una instancia al azar
instancia <- datos[sample(nrow(datos), 1), ]

# Predecimos con cada clasificador
predict_i = function(i){ 
  predict(clasificadores[[i]],newdata=instancia,type="prob")
}
predicciones <- list()
for(i in 1:pasos){
predicciones[[i]] = (predict_i)(i)
}

show(predicciones[])

# Mostramos las predicciones
numClases = ncol(predicciones[[i]])
probs <- array(0,dim=c(0,numClases))
for(j in 1:numClases){
  probs[j] = 0
}

for(i in 1:pasos){
  for(j in 1:numClases){
    probs[j] = probs[j] + predicciones[[i]][j]
  }
}

# Devolvemos el Ã???ndice de la clase
cat(which.max(probs))
