
############################################
# Consumer sentiment
# Problem 1

library(fUnitRoots)

X <- read.csv("m-umcsent.csv", header=T)
str(X)
gdp <- log(X[,3])

# gdp EDA here

adfTest(gdp, lags=10, type="c")

# first difference EDA here

adfTest(diff(gdp), lags=10, type="c")


############################################
# Duration
# Problem 2

da <- read.table("m-unempmean.txt",header=T)
str(da)
yr <- da$Year
A <- data.matrix(da[,-1])
colnames(A) <- NULL
summary(A)
v <- A[1,]
for (i in 2:nrow(da)) { v <- c(v,A[i,]) }
v <- v[-length(v)]
length(v)
rm(A)
y <- rep(yr, each=12)
y <- y[-length(y)]
z <- rep(seq(12), times=(max(y)-min(y)+1))
z <- z[-length(z)]
tdx <- y + z/100
dur <- v
rm(yr,y,z)


plot(tdx,v,type='l',xlab='year',ylab='dur',main='Mean duration of unemp');grid(col="gray")
dur <- ts(v)
pacf(dur);grid(col="gray")
pacf(diff(dur));grid(col="gray")
m1 <- ar(diff(dur),method="mle")
m1$order

require(fUnitRoots)
help(adfTest)  ### Unit-root test. H0: root exists
adfTest(dur,lags=m1$order,type="c")

ddur <- diff(dur)
t.test(ddur)

m2 <- arima(ddur,order=c(12,0,0),include.mean=F)
m2

tsdiag(m2,gof=24)   # use Box.test for independence

m4 <- arima(ddur,order=c(2,0,1),seasonal=list(order=c(1,0,1),period=12),include.mean=F)
m4

tsdiag(m4,gof=24)

source("backtest.R")

backtest(m2,ddur,750,1,inc.mean=F)  # about 2 minutes or so of compute time
backtest(m4,ddur,750,1,inc.mean=F)


############################################
# Oil Prices
# Problem 3

da <- read.csv("DCOILWTICO.csv",header=T,stringsAsFactors=F)
str(da);summary(da)
da$DCOILWTICO <- as.numeric(da$DCOILWTICO)
da <- na.omit(da)
Y <- da

Y$y <- as.numeric(substr(da$DATE,1,4))
Y$m <- as.numeric(substr(da$DATE,6,7))

# Obtain monthly averages
X <- aggregate(DCOILWTICO ~ y + m, Y, FUN = mean)
str(X)
names(X)[3] <- "coil"
rm(Y)

X$tdx <- X$y + X$m/100;summary(X)
X <- X[order(X$tdx),]
head(X,n=30)

plot(X$tdx, X$coil, xlab='year', ylab='price', type='l', main="Weekly Crude Oil Prices"); grid(col="gray")
plot(diff(X$coil),type='l');grid(col="gray")
plot(diff(log(X$coil)),type='l');grid(col="gray")

rtn <- diff(log(X$coil))
t.test(rtn)

Box.test(rtn,lag=10,type='Ljung')

acf(rtn)
m1 <- ar(rtn,method="mle")
m1$order
m1 <- arima(rtn,order=c(11,0,0))
m1

c1 <- c(NA,NA,NA,NA,0,0,NA,NA,0,0,NA)
m2 <- arima(rtn,order=c(11,0,0),include.mean=F,fixed=c1)
m2

tsdiag(m2,gof=20)
m3 <- arima(rtn,order=c(3,0,2),include.mean=F)
m3

tsdiag(m3,gof=20)   # Use Box.test for independence


