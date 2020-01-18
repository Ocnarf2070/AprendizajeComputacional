rm(list = ls())
n<-as.numeric(readline(prompt = "How many observations have?\n"))
d<-rep(1,n)*1/n;
t<-readline(prompt = "How many iterations have?\n")
for (i in 1:t){
  y_h<-rep(1,n)
  index <- strsplit(readline(prompt = "What observations are wrong?")," ")
  cat(sprintf("d%d:\t",i))
  cat(sprintf("%.4f\t",d))
  index <- strtoi(index[[1]])
  y_h[index]=-1
  error <- sum(d[index])
  cat(sprintf(" error%d: %.4f",i,error));
  alpha=(1/2)*log((1-error)/error);
  cat(sprintf(" alpha%d: %.4f\n",i,alpha));
  e<-exp(-alpha*y_h)
  cat("e:\t")
  cat(sprintf("%.4f\t",e))
  cat("\n")
  d_e<-d*e
  z<-sum(d_e)
  cat(sprintf("d%de:\t",i))
  cat(sprintf("%.4f\t",d_e))
  d<-d_e/z
  cat(sprintf(" Z%d: %.4f",i,z));
}


