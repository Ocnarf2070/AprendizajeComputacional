rm(list = ls())
a1<- c(0, 10,2, 6,4,1,8,10,6)
b1 <-c(250,150,90,78,20,170,160,180,200)
c1<-c(36,34,10,8,1,70,41,38,45)
d1<-c("H", "M","H", "M", "M", "H",  "M",  "H", "H")




simpsons<- data.frame(a1, b1, c1, d1)
names(simpsons)<- c("Pelo", "Peso", "Edad", "Genero")


a1<- c(8 ,9)
b1 <-c(29,135)
c1<-c(38,32)
d1<-c("H","M")

library(rpart)
myFormula <- Pelo~.
simpsons_rpart <- rpart(myFormula, data = simpsons)
attributes(simpsons_rpart)
print(simpsons_rpart$cptable)
print(simpsons_rpart)
plot(simpsons_rpart)
text(simpsons_rpart, use.n=TRUE)
opt <- which.min(simpsons_rpart$cptable[,"xerror"])
cp <- simpsons_rpart$cptable[opt, "CP"]
simpsons_prune <- prune(simpsons_rpart, cp = cp)
print(simpsons_prune)
