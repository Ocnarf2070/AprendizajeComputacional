rm(list = ls())
year <- rep(2008:2010, each = 4)
quarter <- rep(1:4, 3)
cpi <- c(162.2, 164.6, 166.5, 166, 166.2, 167, 168.6, 169.5, 171,
         172.1, 173.3, 174)
plot(cpi, xaxt = "n", ylab = "CPI", xlab = "")
# draw x-axis, where   las=3 makes text vertical
axis(1, labels = paste(year, quarter, sep = "Q"), at = 1:12, las = 3)

## correlation between CPI and year / quarter
cor(year, cpi)
## [1] 0.9096316
cor(quarter, cpi)
## [1] 0.3738028
## build a linear regression model with function lm()
fit <- lm(cpi ~ year + quarter)
fit
#cpi = c0 + c1*year + c2*quarter
# What will the CPI be in 2011?
cpi2011 <- fit$coefficients[[1]] +
  fit$coefficients[[2]] * 2011 +
  fit$coefficients[[3]] * (1:4)
cpi2011
fit$coefficients

data2011 <- data.frame(year = 2011, quarter = 1:4)
cpi2011 <- predict(fit, newdata = data2011)
cpi2011
style <- c(rep(1,12), rep(2,4))
plot(c(cpi, cpi2011), xaxt="n", ylab="CPI", xlab="", pch=style, col=style)
axis(1, at=1:16, las=3,
       labels=c(paste(year,quarter,sep="Q"), "2011Q1", "2011Q2", "2011Q3", "2011Q4"))

