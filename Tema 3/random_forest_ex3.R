rm(list = ls())
x1=rnorm(1000)
x2=rnorm(1000) 
y=2*x1+.7*x2+rnorm(1000)
df=data.frame(y,x1,x2,x3=rnorm(1000),x4=rnorm(1000),x5=rnorm(1000)
)
# run the randomForest implementation
library(randomForest)
rf1 <- randomForest(y~., data=df, mtry=2, ntree=100,
                    importance=T)
importance(rf1,type = 1)
