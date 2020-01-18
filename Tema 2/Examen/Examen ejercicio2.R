library(rpart)
a<- c(0,10,20,30,40,50)
b<- c(4,22,44,60,82,89)

train<- data.frame(a,b)
names(train)<- c("In", "Signal")
train
fit <- lm(formula = Signal ~ In, data = train)
fit


c<- c(5,15,25,35,45)
test<- data.frame(c)
names(test)<- c("In")
prediccion <- predict.lm(object = fit, newdata = test )
prediccion

fit$coefficients[[1]]
fit$coefficients[[2]]

plot(train$In , train$Signal) 
plot(test$In , prediccion) 

#
fit <- lm(formula = Signal ~ In + I(In^2), data = train)
fit

prediccion <- predict.lm(object = fit, newdata = test )
prediccion

fit$coefficients[[1]]
fit$coefficients[[2]]
fit$coefficients[[3]]

plot(train$In , train$Signal) 
plot(test$In , prediccion) 

#El segundo modelo se ajusta mejor, pues es mas preciso
