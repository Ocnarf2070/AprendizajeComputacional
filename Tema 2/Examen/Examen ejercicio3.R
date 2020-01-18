#El conjunto de datos Kyphosis (disponible con el paquete Rpart) nos indica si, 
#tras una operación en la columna vertebral, una muestra de 81 niños presentan 
#o no deformaciones en la columna vertebral (columna Kyphosis). El resto de 
#columnas son:
 # Age : Año en meses.
#Number: Numero de la vértebra involucrada.
#Start : Numero de la primera vértebra involucrada

#Compara la precisión (accuracy) del conjunto de datos anterior usando validación 
#cruzada ( 10% para test el resto para entrenamiento). Obtén los conjuntos 
#aleatoriamente) mediante árboles de decisión (rpart) y perceptrón multicapas 
#(usa 5 capas).

#Inicialización de los datos
library(rpart)
str(kyphosis)

#Validación cruzada
data_size <- dim(kyphosis)[1]
test_size <- 8 #Representa el 10% de los datos

set.seed(100)
samples <- sample(data_size, data_size, replace=FALSE)
indexes <- samples[1:test_size]
train <- kyphosis[-indexes,]
test <- kyphosis[indexes,]

# Rpart
#Entrenamiento
arbol <- rpart(formula=Kyphosis~., data=train)

#Predicción
rpart.prediccion <- predict(object = arbol, newdata = test, type = "class")

#Matriz de confusión
rpart.matriz_confusion <- table(rpart.prediccion, test$Kyphosis)
rpart.matriz_confusion

#Acuracy
rpart.suma_matriz_confusion <- sum(rpart.matriz_confusion)
rpart.suma_aciertos <- sum(diag(rpart.matriz_confusion))
rpart.accuraccy <- rpart.suma_aciertos/ rpart.suma_matriz_confusion
rpart.accuraccy

#Perceptron multicapa
library(nnet) 
neuron <- 5 #number of units in the hidden layer
maximum_iterations = 1000

#Entrenamiento
nn.fit <- nnet(formula=Kyphosis~., data=train, size=neuron, maxit=maximum_iterations, decay=5e-4)

#Prediction
nn.prediccion <- predict(object = nn.fit, newdata = test, type = "class")

#Matriz de confusión
nn.matriz_confusion <- table(nn.prediccion, test$Kyphosis)
nn.matriz_confusion

#Acuracy
nn.suma_matriz_confusion <- sum(nn.matriz_confusion)
nn.suma_aciertos <- sum(diag(nn.matriz_confusion)) 
nn.accuraccy <- nn.suma_aciertos/nn.suma_matriz_confusion
nn.accuraccy


#¿Es posible encontrar un árbol de decisión Rpart y un perceptrón muticapa tal 
#que sus accuracy sean próximos modificando las parámetros CP y numero de neuronas,
#respectivamente?
#Lo que podríamos hacer es podar el arbol rpart con diferentes cp, probar
#perceptrones con diferentes número de neuronas, y comparar y decicir si
#hay algún arbol que se parezca a algún perceptrón de todos los que hemos probado.

