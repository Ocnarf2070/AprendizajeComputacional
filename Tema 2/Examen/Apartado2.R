rm(list = ls())
library(rpart)
train <- read.csv("D:/Users/franc/Google Drive/Ingenieria de Informatica/Curso2018-19/Aprendizaje Computacional/Tema 2/Examen/Apartado2Train.csv",header = TRUE)
names(train)<-c("x","y","z")
myFormula1<-x~.
myFormula2<-y~.
myFormula3<-z~.
tree1<-rpart(myFormula1,data = train);
tree2<-rpart(myFormula2,data = train);
tree3<-rpart(myFormula3,data = train);
plot(tree1,main= myFormula1)
text(tree1,use.n=TRUE)
plot(tree2,main= myFormula1)
text(tree2,use.n=TRUE)
plot(tree3,main= myFormula1)
text(tree3,use.n=TRUE)
test <- read.csv("D:/Users/franc/Google Drive/Ingenieria de Informatica/Curso2018-19/Aprendizaje Computacional/Tema 2/Examen/Apartado2Test.csv",header = TRUE)
names(test)<-c("x","y","z")
pred1 <- predict(tree1,newdata = test);
MC<- table(test$x,pred1)           
MC[1, 1] / (MC[1, 1] + MC[2, 1])
pred2 <- predict(tree2,newdata = test);
MC<- table(test$y,pred2)           
MC[1, 1] / (MC[1, 1] + MC[2, 1])
pred3 <- predict(tree3,newdata = test);
MC<- table(test$z,pred3)           
MC[1, 1] / (MC[1, 1] + MC[2, 1])

opt <- which.min(tree1$cptable[,"xerror"])
cp <- tree1$cptable[opt, "CP"];cp
ptree1 <- prune(tree1, cp = cp)
opt <- which.min(tree2$cptable[,"xerror"])
cp <- tree2$cptable[opt, "CP"];cp
ptree2 <- prune(tree2, cp = cp)
opt <- which.min(tree3$cptable[,"xerror"])
cp <- tree3$cptable[opt, "CP"];cp
ptree3 <- prune(tree3, cp = cp)
plot(ptree1,main= myFormula1)
text(ptree1,use.n=TRUE)
plot(ptree2,main= myFormula1)
text(ptree2,use.n=TRUE)
plot(ptree3,main= myFormula1)
text(ptree3,use.n=TRUE)
pred1 <- predict(ptree1,newdata = test);
MC<- table(test$x,pred1)           
MC[1, 1] / (MC[1, 1] + MC[2, 1])
pred2 <- predict(ptree2,newdata = test);
MC<- table(test$y,pred2)           
MC[1, 1] / (MC[1, 1] + MC[2, 1])
pred3 <- predict(ptree3,newdata = test);
MC<- table(test$z,pred3)           
MC[1, 1] / (MC[1, 1] + MC[2, 1])
