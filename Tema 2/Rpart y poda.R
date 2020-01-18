library(rpart)
Recidiva <- read.table("C:\\datos_icb.txt", header=TRUE, quote="\"")
formularecidiva<- recid ~.
arbolrecidiva<-rpart (formularecidiva, Recidiva)
print(arbolrecidiva$cptable)
plot(arbolrecidiva)
text(arbolrecidiva, use.n=TRUE)
opt <- which.min(arbolrecidiva$cptable[,"xerror"])
cp1 <- arbolrecidiva$cptable[opt, "CP"]
arbolpodado<-prune (arbolrecidiva, cp=cp1)
plot(arbolpodado)
text(arbolpodado, use.n=TRUE)
     