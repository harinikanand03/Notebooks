
######   Assignment 5   ##########


##############################################
#   Problem 1

da=read.table("m-PastorStambaugh.txt",header=T)
head(da)
plot(da[,2],type='l');grid(col="gray")
plot(da[,3],type='l');grid(col="gray")
acf(da[,2]);grid(col="gray")
acf(da[,3]);grid(col="gray")
pacf(da[,2]);grid(col="gray")
m2 <- arima(da[,2],order=c(5,0,0))
m2
tsdiag(m2)
which.min(m2$residuals)
dim(da)
i303 <- rep(0,605)
i303[303] <- 1
m3 <- arima(da[,2],order=c(5,0,0),xreg=i303)
m3
tsdiag(m3)
c1 <- c(NA,NA,NA,0,NA,NA,NA)
m3 <- arima(da[,2],order=c(5,0,0),xreg=i303,fixed=c1)
m3
tsdiag(m3)


##############################################
#   Problem 2

da <-mread.table("m-FamaBlissdbndyields.txt",header=T)
dim(da)
head(da)
y3t <- da[,3]
xt <- log(y3t+0.5)
acf(xt)
acf(diff(xt))
acf(diff(diff(xt),4))

# your model, supply p, d, and q from the acf and the pacf
m4 <- arima(xt,c=())
m4
tsdiag(m4,gof=20)

m5 <- arima(xt,order=c(0,1,1),seasonal=list(order=c(0,0,1),period=4))
m5
tsdiag(m5,gof=20)
source("backtest.R")
backtest(m4,xt,600,1)
backtest(m5,xt,600,1)


##############################################
#   Problem 3

da <- read.table("m-FamaBlissdbndyields.txt",header=T)
dim(da)
head(da)
y1t <- da[,2]; y3t <- da[,3]

m1 <- lm(y3t~y1t)
summary(m1)
acf(m1$residuals)

d1t <- diff(y1t); d3t=diff(y3t)
m2 <- lm(d3t~-1+d1t)    # linear regression, remove intercept => force through zero
summary(m2)
acf(m2$residuals);grid(col="gray")
pacf(m2$residuals);grid(col="gray")

m3 <- ar(m2$residuals,method="mle")   # find p for model m4
m3$order

m4 <- arima(d3t,order=c(5,0,0),xreg=d1t,include.mean=F)
m4
tsdiag(m4,gof=24)  ## saved into a file

c1 <- c(NA,NA,0,NA,NA,NA)
m5 <- arima(d3t,order=c(5,0,0),xreg=d1t,include.mean=F,fixed=c1)
m5
tsdiag(m5,gof=24)  ### very close to that of the above AR(5) model.


##############################################
#   Problem 4

m6 <- arima(y3t,order=c(6,0,0),xreg=y1t)
m6
tsdiag(m6,gof=24)

c2 <- c(NA,0,NA,NA,0,NA,NA,NA)
m7 <- arima(y3t,order=c(6,0,0),xreg=y1t,fixed=c2)
m7
tsdiag(m7,gof=24)

p1 <- c(1,-m7$coef[1:6])
s1 <- polyroot(p1)
s1
Mod(s1)
1/Mod(s1)

