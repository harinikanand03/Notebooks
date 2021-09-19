
#########   Assignment 10   ###############

###########################################
#     Random Forest Time Series
###########################################

# DTLoad is a time series representing electricity usage. Data are sampled every 30 minutes.
# There are 2 seasonal periods, daily (period=48) and weekly (period=336)
# Why wouldn't one just use an ARIMA model? ARIMA models with long seasons are extremely slow -
# The number of seasonal variables = the length of the season, which in this case would be at least 336 variables.

source("RFfunctions.R")

df <- read.csv("DTload.csv")

# this data set has 2 seasons
df <- msts(as.vector(df$value),seasonal.periods = c(48,336))

# build model, specifying a 1 day forecast (the shortest season)
mrf <- tsrf(train = df, hf = 48, ntree = 500, mtry = 3, nodesize = 3, KK = 2)

# build forecast
f <- forecast(mrf)

# show forecast
f

# plot forecast
plot(f);grid(col="black")

# plot again, but zoom into forecast
tx <- time(f$x)
tf <- time(f$mean)
end <- tf[length(tf)]

# last 2 weeks
start <- tx[length(tx)-48*14]
plot(f,xlim=c(start,end));grid(col="black")

#last week
start = tx[length(tx)-48*7]
plot(f,xlim=c(start,end));grid(col="black")


###########################################
#     Neural Network Time Series
###########################################

# Shumueli, Section 9.7, p 201

require(forecast)
require(zoo)
require(caret)

wine <- read.csv("AustralianWines.csv")

# Just time and fortified wine
X <- wine[,1:2]

# Create ts object. tr - train, te - test
   x <- ts(X$Fortified, start = c(1980,1), end = c(1994,12), freq = 12) # Slice the sets
x.tr <- window(x, start = c(1980,1), end = c(1993,12))
x.te <- window(x, start = c(1994,1), end = c(1994,12))

# Train the neural net
tr.nn <- nnetar(x.tr,p = 11)
summary(tr.nn$model[[1]])
x.tr.pred <- forecast(tr.nn,h = 12)
accuracy(x.tr.pred, x.te)

# plot results
plot(x.tr, ylim = c(-200,5500), ylab = "Fortified", xlab = "Time", bty = "l", xlim = c(1980,1994), lty = 1)
lines(x.tr.pred$fitted, lwd = 1, col = "blue")
lines(x.tr.pred$residuals, col = "red")
lines(x.tr.pred$mean, lwd = 1, col = "blue", lty = 2)
lines(x.te)
grid(col="gray")

# ETS
ets.tr <- ets(x.tr)
summary(ets.tr)

ets.tr.pred <- forecast(ets.tr, h = 2)

accuracy(x.tr.pred, x.te)
accuracy(ets.tr.pred, x.te)


###########################################
#     HMM Time Series
###########################################

# Analyze for regime changes
# Similar to
# http://gekkoquant.com/2014/09/07/hidden-markov-models-examples-in-r-part-3-of-4/

# Import the necessary packages and set
# random seed for replication consistency
# install.packages('depmixS4')
# install.packages('quantmod')
require('depmixS4')
require('quantmod')


(infile <- "BullBear.csv")
     X <- data.frame(read.csv(infile, header=TRUE))
summary(X)

# Fit a Hidden Markov Model
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=X)
hmm.fit <- fit(hmm, verbose = FALSE)
post.probs <- posterior(hmm.fit)

# True regimes and the posterior probabilities of the regimes
layout(1:2)
plot(post.probs$state, type='s', main='True Regimes', xlab='', ylab='Regime')
matplot(post.probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='topleft', c('Bull','Bear'), fill=1:2, bty='n')

# Fetch S&P500 data from 2004 on
getSymbols( "^GSPC", from="2004-01-01" )
summary(GSPC)
gspc <- diff( log( Cl( GSPC ) ) )
returns <- as.numeric(gspc)

# Fit S&P500 with a two state hmm
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns))
hmm.fit <- fit(hmm, verbose = FALSE)
post.probs <- posterior(hmm.fit)

# Plot returns and posterior probabilities
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post.probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='topleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')

# Fit a 3-state hmm to the S&P500
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 3, data=data.frame(returns=returns))
hmm.fit <- fit(hmm, verbose = FALSE)
post.probs <- posterior(hmm.fit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post.probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')


###########################################
#     SPRT Monitoring
###########################################

require(SPRT)
options(digits=4)

# SPRT on a Poisson distributed random variable
X <- read.csv("BTC-USD.csv",header=T)
summary(X)
X$Date <- as.character(X$Date)
X$year <- substr(X$Date,1,4)
mon    <- substr(X$Date,6,7)
X$mon  <- ifelse(nchar(mon)==2, mon, paste0("0",mon) )
rm(mon)
day    <- substr(X$Date,9,10)
X$day  <- ifelse(nchar(day)==2, day, paste0("0",day))
rm(day)
X$tdx  <- as.numeric(paste0(X$year,X$mon,X$day))
X <- X[order(X$tdx),]
str(X)
plot(X$tdx,X$Close,xlab='yea',ylab='price',type='l',main="Weekly Crude Oil Prices");grid(col="black")
plot(diff(X$Close),type='l');grid(col="black")
plot(diff(log(X$Close)),type='l');grid(col="black")
H <- X   # Hold all the data
X <- X[X$tdx>=20182000,]
str(X)


# first diffence of log transform

y <- diff(log(X$Close))
plot.ts(y);grid(col="gray")

sprt <- SPRT(distribution = "normal", type1 = 0.05, type2 = 0.20, h0 = 0, h1 = 0.2, values = y )
sprt
str(sprt)

# Sequential log-likelihood ratio vs Wald's A and B constants
#sprt$data.llr
plot(sprt, log = "y", type = "h");grid(col="gray")


# test time series one value at a time and report exciptions
H0 <- -2; Ha <- 2.5
t1 <- 0.05; t2 <- 0.2
i <- 1
s <- SPRT(distribution = "normal", type1 = t1, type2 = t2, h0 = H0, h1 = Ha, values = y[i] )
A <- cbind(s$data.llr,t=s$data.llr$n+i-1,s$interpretation)
A
repeat {
	while ((s$interpretation=="Continue testing") && (nrow(A)<=length(y))) {
		i <- i + 1
		if (i > length(y)) {break}
		s <- SPRT(distribution = "normal", type1 = t1, type2 = t2, h0 = H0, h1 = Ha, values = y[i] )
		A <- rbind(A,cbind(s$data.llr,t=i,s$interpretation))
		}
	print(A)	# Sequential log-likelihood ratio vs Wald's A and B constants
	plot.ts(A$llr)
	abline(h=A$wald.B)
	abline(h=A$wald.A)
	grid(col="gray")
	if (i > length(y)) {break}
	invisible(readline(prompt="Press [enter] to continue, [esc] to stop."))
	i <- i + 1
	s <- SPRT(distribution = "normal", type1 = t1, type2 = t2, h0 = H0, h1 = Ha, values = y[i] )
	A <- rbind(A,cbind(s$data.llr,t=i,s$interpretation))
}
# plot(s, log = "y");grid(col="gray")
plot.ts(A$llr)
abline(h=A$wald.B)
abline(h=A$wald.A)
grid(col="gray")


# Put your ARIMA, volatility model, or both here and use the residuals in the SPRT code above.

H0 <- -2; Ha <- 2.5   # Adjust to fit your model
t1 <- 0.05; t2 <- 0.2 # Adjust if you wish
i <- 1
s <- SPRT(distribution = "normal", type1 = t1, type2 = t2, h0 = H0, h1 = Ha, values = y[i] )
A <- cbind(s$data.llr,t=s$data.llr$n+i-1,s$interpretation)
A
repeat {
	while ((s$interpretation=="Continue testing") && (nrow(A)<=length(y))) {
		i <- i + 1
		if (i > length(y)) {break}
		s <- SPRT(distribution = "normal", type1 = t1, type2 = t2, h0 = H0, h1 = Ha, values = y[i] )
		A <- rbind(A,cbind(s$data.llr,t=i,s$interpretation))
		}
	print(A)	# Sequential log-likelihood ratio vs Wald's A and B constants
	plot.ts(A$llr)
	abline(h=A$wald.B)
	abline(h=A$wald.A)
	grid(col="gray")
	if (i > length(y)) {break}
	invisible(readline(prompt="Press [enter] to continue, [esc] to stop."))
	i <- i + 1
	s <- SPRT(distribution = "normal", type1 = t1, type2 = t2, h0 = H0, h1 = Ha, values = y[i] )
	A <- rbind(A,cbind(s$data.llr,t=i,s$interpretation))
}
# plot(s, log = "y");grid(col="gray")
plot.ts(A$llr)
abline(h=A$wald.B)
abline(h=A$wald.A)
grid(col="gray")


########################################

