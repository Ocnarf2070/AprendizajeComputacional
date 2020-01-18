library(rpart)
library(party)
library(pROC)
set.seed(1234)
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

#rpart

ind <- sample(150,150)
accuracy<-vector()

#Perform 10 fold cross validation
for(i in 1:10){
  #idt <- ind[10*(i-1)+1:10*i]
  ii <- 15*(i-1)+1
  is <- 15*i
  idt <- ind[ii : is]
  dtrain <- iris[-idt,]
  dtest <- iris[idt,]
  fit <- rpart(myFormula, dtrain)
  # summary(fit)
  # plot(fit, compress=TRUE)
  # text(fit, use.n=TRUE)  
  testPred <- predict(fit, newdata = dtest, type="class")
  matrizconfusion<-table(testPred, dtest$Species)
  accuracy[i]<-sum(diag(matrizconfusion))/sum(matrizconfusion)
  
}
cat("mean rpart: ",mean (accuracy))
cat("min rpart: ",min (accuracy))
cat("max rpart: ",max (accuracy))




#party
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.8, 0.2))
dtrain <- iris[ind==1,]
dtest <- iris[ind==2,]
fit2 <- ctree(myFormula, dtrain)
summary(fit2)
plot(fit2, compress=TRUE)

testPred <- predict(fit2, newdata = dtest)
matrizconfusion<-table(testPred, dtest$Species)
accuracy<-sum(diag(matrizconfusion))/sum(matrizconfusion)
cat("Rpart: ",accuracy)

