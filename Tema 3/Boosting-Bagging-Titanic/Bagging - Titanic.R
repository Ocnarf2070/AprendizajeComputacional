library(adabag);
library(rpart);
library(C50);
datos <- read.csv("train.csv", header=T, quote="\"")

dsize <- dim(datos)[1]
dsize
dt_size <- ceiling(0.2*dsize)
dt_size

set.seed(200)
samples <- sample(dsize, dsize, replace=FALSE)
indexes <- samples[1:dt_size]
dtrain <- datos[-indexes,]
dtest <- datos[indexes,]

Training  <- dtrain
Training$Survived  <- factor(Training$Survived)
Test <- dtest
set.seed(110) 

Modelo_AdaBag  <- bagging(formula = Survived~PassengerId+Pclass+Sex+Age+Parch+Fare+SibSp+Embarked, 
                          data=Training, 
                          na.action = na.omit,
                          mfinal=9,
                          control=rpart.control(cp = 0.001, minsplit=7))

Forecast <- predict(Modelo_AdaBag,Test,type="class")$class
MC <-table(Test[, "Survived"],Forecast);MC
TruePos <-MC[1,2]/(MC[1,1]+MC[1,2]);TruePos
Scores <- sum(diag(MC))
Accuracy <- Scores/(sum(MC));Accuracy

