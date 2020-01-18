FIT=list()
library(randomForest)
for(i in 1:n) {
FIT[[i]] = rpart(Z~X+Y,data=df[-i,])
}
predict_i = function(i){ 
  predict(FIT[[i]],newdata=df[i,],type="prob")[,2]
}
S = Vectorize(predict_i)(1:n)
  