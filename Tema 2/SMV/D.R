rm(list = ls())
## density-estimation

# create 2-dim. normal with rho=0:
X <- data.frame (a=rnorm (1000), b=rnorm (1000))
attach (X)

# traditional way:
m <- svm (X)

# formula interface:
m <- svm(~., data = X, gamma = 0.1)
# or:
m <- svm(~ a + b, gamma = 0.1)

# test:
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)

# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)


for(i in 1:nrow(Data3)){
  show(Data3$x1[i])
  show(Data3$x2[i])
  if(sqrt(Data3$x1[i]^2+Data3$x2[i]^2)>2){
    cat("YES\n")
  }
}
