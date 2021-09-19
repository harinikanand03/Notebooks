
#############################################
# Problem 1 ###

da <- read.table("d-nflx3dx0913.txt",header=T)
head(da)
X <- da  # hold original data

require(fBasics)
basicStats(X$nflx)
basicStats(X$vwretd)
basicStats(X$ewretd)
basicStats(X$sprtrn)

d1 <- density(X$nflx)
d2 <- density(X$sprtrn)
par(mfcol=c(1,2))
plot(d1$x,d1$y,xlab='returns',ylab='density',main='Netflix',type='l')
plot(d2$x,d2$y,xlab='returns',ylab='density',main='SP',type='l')

rtn <- log(X[,3:6]+1)   ### Compute log returns and hold data
names(rtn) <- c("lnnflx","lnvwretd","lnewretd","lnsprtrn")
X <- cbind(X,rtn)    # X was raw data but now has both rqw and log returns
str(X)

basicStats(X$lnnflx)
basicStats(X$lnvwretd)
basicStats(X$lnewretd)
basicStats(X$lnsprtrn)
t.test(X$lnnflx)

d1 <- density(X$lnnflx)
d2 <- density(X$lnsprtrn)
par(mfcol=c(1,2))
plot(d1$x,d1$y,xlab='returns',ylab='density',main='Netflix',type='l')
plot(d2$x,d2$y,xlab='returns',ylab='density',main='SP',type='l')


#############################################
# Problem 2 ###

nflx <- X$lnnflx
tm3 <- skewness(nflx)/sqrt(6/length(nflx))
tm3
tk <- kurtosis(nflx)/sqrt(24/length(nflx))
tk
t.test(nflx)


#############################################
# Problem 3

require(fpp)

# https://www.ncdc.noaa.gov/cag/global/time-series
# Units: Degrees Celsius
# Base Period: 1901-2000
# Missing: -999
main <- "Global Land and Ocean Temperature Anomalies, November"

X <- read.csv("ClimateChangeAnnual201910.csv",header=T)
str(X)
# convert years into decades
decade <- rep(1:14,each=10)
X$Year <- decade + X$Year/10000
# make X a time series
X <- ts(X, start=X[1,1], frequency=10)  # cycles by decade
str(X)

plot(X[,2], type="o", xlab="Decade", ylab="Annual Temperature (C)",main=main); grid(col="gray")

# Make s.window as large as possible while keeping trend smooth
decomp <- stl(X[,2], s.window="periodic")
plot(decomp)

# Seasonally adjusted data with damped trend (ets)
sa <- seasadj(decomp)
fit <- holt(sa, h=10)
plot(fit); grid(col="gray")

# RMSE
accuracy(fit)

# Method parameters
fit$model
# Small beta means the slope isn't changing much over time
# Large alpha means the level (intercept) is changing quickly.

# Reseasonalize forecasts:
lastyear <- rep(decomp$time.series[136:140,"seasonal"],2)
fc <- fit$mean + lastyear
plot(X[,2],xlim=c(1,16))
lines(fc, col="blue")
grid(col="gray")

