
#########   Assignment 6    ###########
rm(list=ls())
library("fGarch") 
library(forecast)
library(ggplot2)
library(gridExtra)
require(fpp)
require(fUnitRoots)

#########   Problem 1    ###########

da <- read.table("d-msft3dx0113.txt",header=T)
head(da)
str(da)

par(mfrow=c(1,2))
hist(da$msft,main="Histogram of MSFT simple returns")
boxplot(da$msft,main="Boxplot of MSFT simple returns",col="green",horizontal=TRUE)
basicStats(da$msft)

par(mfrow=c(1,1))
plot(da$msft,ylab='simple returns',type='l',main="MSFT daily simple returns"); grid(col="gray")

X <- ts(da,start=c(2001,1),frequency=12)
decomp <- stl(X[,3],s.window="periodic")
plot(decomp)
seasonplot(X[,3],year.labels=TRUE,col=rainbow(6))
p1 <- ggplot(da, aes(x=da[,3])) + geom_histogram(aes(y=..density..),col="green") + geom_density(alpha=0.6)+ggtitle("Density plot of MSFT daily simple returns") + theme(plot.title = element_text(hjust = 0.5))
p2 <- ggplot(da) + geom_qq(aes(sample = da[,3]),col="green")+ggtitle("Normal QQ plot") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p2)
t.test(da$msft)
tm3 <- skewness(da$msft)/sqrt (6/length(da$msft))
tm3
(p <- 2 * (1 - pnorm(abs(tm3))))
tk <- kurtosis(da$msft)/sqrt(24/length(da$msft))
tk
(p <- 2 * (1 - pnorm(abs(tk))))

acf(da$msft,main="ACF of msft daily simple returns series");grid(col="gray")
pacf(da$msft,main="PACF of msft daily simple returns series");grid(col="gray")
Box.test(da$msft,lag=10,type="Ljung")
adfTest(da$msft,lags=10)
####################
####################
msft=log(da$msft+1)
par(mfrow=c(1,2))
hist(msft,main="Histogram of MSFT log-returns")
boxplot(msft,main="Boxplot of MSFT log-returns",col="blue",horizontal=TRUE)
basicStats(msft)

par(mfrow=c(1,1))
plot(msft,type='l',main="Plot of the msft log-returns")

df <- cbind(da$date, msft)
df <- as.data.frame(df)
colnames(df) <- c("date","msft")

X <- ts(df,start=c(2001,1),frequency=12)
decomp <- stl(X[,2],s.window="periodic")
plot(decomp)
seasonplot(X[,2],year.labels=TRUE,col=rainbow(6))

p1 <- ggplot(df, aes(x=df[,2])) + geom_histogram(aes(y=..density..),col="green") + geom_density(alpha=0.6)+ggtitle("Density plot of MSFT daily log-returns") + theme(plot.title = element_text(hjust = 0.5))
p2 <- ggplot(df) + geom_qq(aes(sample = df[,2]),col="green")+ggtitle("Normal QQ plot") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p2)

t.test(df$msft)
tm3 <- skewness(df$msft)/sqrt (6/length(df$msft))
tm3
(p <- 2 * (1 - pnorm(abs(tm3))))
tk <- kurtosis(df$msft)/sqrt(24/length(df$msft))
tk
(p <- 2 * (1 - pnorm(abs(tk))))


acf(log(da$msft+1))
pacf(log(da$msft+1))
Box.test(df$msft,lag=10,type='Ljung')
adfTest(df$msft,lags=10)
t.test(msft)



m1 <- arima(msft,order=c(0,0,2),include.mean=F)
m1
tsdiag(m1)
par(mfrow=c(1,2))
hist(m1$residuals, main="Histogram of MODEL1 residuals")
qqnorm(m1$residuals, pch = 1, frame = FALSE,main="Normal Q-Q plot for MODEL 1 diagnostics")
qqline(m1$residuals, col = "steelblue", lwd = 2)

Box.test(m1$residuals,lag=10,type='Ljung')
Box.test(m1$residuals^2,lag=10,type='Ljung')

par(mfrow=c(1,2))
acf(m1$residuals, lag.max=10)
acf(m1$residuals^2, lag.max=10)


m2 <- garchFit(~arma(0,2)+garch(1,1),data=msft,trace=F)
summary(m2)
m2 <- garchFit(~arma(0,1)+garch(1,1),data=msft,trace=F)
summary(m2)
par(mfrow=c(1,1))
plot(m2)

m3 <- garchFit(~arma(0,1)+garch(1,1),data=msft,trace=F,cond.dist="std")
summary(m3)
plot(m3)

pm3 <- predict(m3,5)
pm3


source("Igarch.R")
m4 <- Igarch(msft)
str(m4)
sigma.t <- m4$volatility
resi <- msft/sigma.t
acf(resi)
acf(resi^2)
Box.test(resi,lag=10,type='Ljung')
Box.test(resi^2,lag=10,type='Ljung')
length(msft)

v1 <- (1-0.973)*msft[length(msft)]^2+.973*sigma.t[length(msft)]^2
sqrt(v1)
pred <- c(msft,sqrt(v1))

m4 <- Igarch(pred)
str(m4)
sigma.t=m4$volatility
v1 <- (1-0.973)*pred[length(pred)]^2+.973*sigma.t[length(pred)]^2
sqrt(v1)
pred <- c(pred,sqrt(v1))

m4 <- Igarch(pred)
str(m4)
sigma.t=m4$volatility
v1 <- (1-0.973)*pred[length(pred)]^2+.973*sigma.t[length(pred)]^2
sqrt(v1)
pred <- c(pred,sqrt(v1))

m4 <- Igarch(pred)
str(m4)
sigma.t=m4$volatility
v1 <- (1-0.973)*pred[length(pred)]^2+.973*sigma.t[length(pred)]^2
sqrt(v1)
pred <- c(pred,sqrt(v1))

par(mfrow=c(1,2))
qqnorm(resi, pch = 1, frame = FALSE,main="Normal Q-Q plot for resi of MODEL 4")
qqline(resi, col = "steelblue", lwd = 2)
hist(resi, main="Histogram of the MODEL 4's standardized residuals")

par(mfrow=c(1,1))
plot(resi,type='l',main="plot of standardized residuals")
# etc

source("garchM.R")
m5 <- garchM(msft)
sigma.t <- m5$sigma.t
resi <- m5$residuals/sigma.t

par(mfrow=c(1,1))
plot(resi,type='l',main="MODEL 5's standardized residuals")
acf(resi)
acf(resi^2)
Box.test(resi,lag=10,type='Ljung')
Box.test(resi^2,lag=10,type='Ljung')
par(mfrow=c(1,2))
qqnorm(resi, pch = 1, frame = FALSE,main="Normal Q-Q plot for MODEL 5 resi")
qqline(resi, col = "steelblue", lwd = 2)
hist(resi, main="Histogram of the MODEL 5's standardized residuals")


#########   Problem 2    ###########

####  EDA #####

da1=read.table("m-ba3dx6113.txt",header=T)
head(da1)

par(mfrow=c(1,2))
hist(da1$ba,main="Histogram of BA monthly returns")
boxplot(da1$ba,main="Boxplot of BA monthly returns",col="green",horizontal=TRUE)
basicStats(da1$ba)

par(mfrow=c(1,1))
plot(da1$ba,ylab='Monthly returns',type='l',main="BA monthly returns"); grid(col="gray")

X <- ts(da1,start=c(1961,1),frequency=12)
decomp <- stl(X[,3],s.window="periodic")
plot(decomp)
seasonplot(X[,3],year.labels=TRUE,col=rainbow(6))
p1 <- ggplot(da1, aes(x=da1[,3])) + geom_histogram(aes(y=..density..),col="green") + geom_density(alpha=0.6)+ggtitle("Density plot of BA monthly returns") + theme(plot.title = element_text(hjust = 0.5))
p2 <- ggplot(da1) + geom_qq(aes(sample = da1[,3]),col="green")+ggtitle("Normal QQ plot") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p2)

t.test(da1$ba)
tm3 <- skewness(da1$ba)/sqrt (6/length(da1$ba))
tm3
(p <- 2 * (1 - pnorm(abs(tm3))))
tk <- kurtosis(da1$ba)/sqrt(24/length(da1$ba))
tk
(p <- 2 * (1 - pnorm(abs(tk))))

acf(da1$ba,main="ACF of ba monthly returns series");grid(col="gray")
acf(da1$ba^2)
pacf(da1$ba,main="PACF of ba monthly returns series");grid(col="gray")
Box.test(da1$ba,lag=10,type="Ljung")
adfTest(da1$ba,lags=10)

######################################

ba=log(da1$ba+1)
par(mfrow=c(1,2))
hist(ba,main="Histogram of BA log-returns")
boxplot(ba,main="Boxplot of BA log-returns",col="blue",horizontal=TRUE)
basicStats(ba)

par(mfrow=c(1,1))
plot(ba,type='l',main="Plot of the ba log-returns")

df <- cbind(da1$date, ba)
df <- as.data.frame(df)
colnames(df) <- c("date","ba")

X <- ts(df,start=c(1961,1),frequency=12)
decomp <- stl(X[,2],s.window="periodic")
plot(decomp)
seasonplot(X[,2],year.labels=TRUE,col=rainbow(6))

p1 <- ggplot(df, aes(x=df[,2])) + geom_histogram(aes(y=..density..),col="green") + geom_density(alpha=0.6)+ggtitle("Density plot of BA log-returns") + theme(plot.title = element_text(hjust = 0.5))
p2 <- ggplot(df) + geom_qq(aes(sample = df[,2]),col="green")+ggtitle("Normal QQ plot") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p2)

t.test(ba)
tm3 <- skewness(ba)/sqrt (6/length(ba))
tm3
(p <- 2 * (1 - pnorm(abs(tm3))))
tk <- kurtosis(ba)/sqrt(24/length(ba))
tk
(p <- 2 * (1 - pnorm(abs(tk))))


acf(log(da1$ba+1))
pacf(log(da1$ba+1))
Box.test(df$ba,lag=10,type='Ljung')
Box.test(df$ba^2,lag=10,type='Ljung')
adfTest(df$ba,lags=10)
t.test(msft)













t.test(ba)
Box.test(ba,lag=12,type='Ljung')

at=ba-mean(ba)
Box.test(at^2,lag=12,type='Ljung')








n1 <- garchFit(~garch(1,1),data=ba,trace=F)
summary(n1)
plot(n1)

n2 <- garchFit(~garch(1,1),data=ba,trace=F,cond.dist="std")
summary(n2)
plot(n2)
n2 <- garchFit(~garch(1,1),data=ba,trace=F,cond.dist="sstd")
summary(n2)
plot(n2)

source("garchM.R")
tt <- (.888-1)/.06
tt
n3 <- garchM(ba)

source("Tgarch11.R")
n4 <- Tgarch11(ba)


