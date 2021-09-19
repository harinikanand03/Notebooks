
################################################
# Problem 1
# UM Consumer Sentiment

require(fUnitRoots)
source('backtest.R')

da <- read.csv("m-umcsent.csv",header=T)
str(da)

csent <- da$Index
tdx <- da[,1]+da[,2]/12
plot(tdx,csent,xlab='year',ylab='sentiment',type='l',main="UM Consumer Sentiment"); grid(col="gray")

acf(csent)
pacf(csent)
pacf(diff(csent))  ### select lag = 5

# Unit roots. H0: roots vs. Ha: no roots
adfTest(csent,lags=5,type="c")
adfTest(csent,lags=5,type="ct")
adfTest(csent,lags=5,type="nc")


################################################
# Problem 2

chg <- diff(csent)
t.test(chg)    # H0: mean = 0 vs. Ha mean != 0
Box.test(chg,lag=12,type='Ljung')    # H0: independent vs. Ha: not independent

m1 <- ar(chg,method="mle")
m1$order
acf(chg)


################################################
# Problem 3

(m2 <- arima(chg,order=c(5,0,0),include.mean=F))

# Don't trust the Ljung-Box plot. Use Box.test
tsdiag(m2) ### Model checking


#This function implements the k = … equation from Tsay p. 42 (the one with a and b, not phi).
#c is a complex number
# Curtesy of Mark Heiple

source('BusCycle.R')

#build polynomial (1 + the negative of the coefficients)

(p2 <- c(1,-m2$coef))
(s2 <- polyroot(p2))

#complex roots are in pairs, so use unique() 
#using round() because there may be very small differences

(z = unique(round(sapply(all_complex(s2),period),digits=3)))


m2p <- predict(m2,4)   ### prediction 1 to 4-step ahead
names(m2p)
m2p$pred
m2p$se

(lcl <- m2p$pred-1.96*m2p$se)
(ucl <- m2p$pred+1.96*m2p$se)



################################################
# Problem 4

# t test for coefficient significance
se <- sqrt(diag(m2$var.coef))
(t <- m2$coef/se)

len = length(chg)
df = len - 1 - length(t)
(pval = (1-pt(abs(t),df=df))*2)


c1 <- c(0,NA,NA,0,NA)
(m3 <- arima(chg,order=c(5,0,0),include.mean=F,fixed=c1))


tsdiag(m3,gof=24)  ### Model checking. Use Box.test for independence

(p3 <- c(1,-m3$coef))
(s3 <- polyroot(p3))
(z = unique(round(sapply(all_complex(s3),period),digits=3)))


backtest(m2,chg,380,1,inc.mean=F)

