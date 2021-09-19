
require(forecast)
require(randomForest)

# Adapted by Mark Heiple from code written by Peter Laurinec, Time Series Data Mining in R. Bratislava, Slovakia.
# https://petolau.github.io/Ensemble-of-trees-for-forecasting-time-series/

# This uses a random forest to model seasonal time series data.
# It first detrends the series, using seasonal decomposition.
# The trend is modeled using auto.arima(), the seasonal and remainder is modeled using randomForest()
# ----------------
# Parameters:
# train - The time series to fit. This must be either a ts (single season) or msts (multiple seasons) object
# hf - The forecast horizon. This is needed because at least this much data from train must be reserved.
#      It should be at most the size of the smallest seasonal period. Data is held back in integer multiples
#      of the smallest season. Larger values will cause the the random forest to be fitted using data more
#      than 1 season in the past.
#
# ntree - randomForest() ntree parameter
# mtry - randomForest() mtry parameter
# nodesize - randomForest() nodesize parameter
# KK - fourier() K parameter

tsrf = function(train, hf, ntree, mtry, nodesize, KK) {
  
  #first check if this is an msts object, which supports multiple seasons
  periods = attr(train,"msts")
  if( is.null(periods) ) {
    #not a msts object, so assume it is a ts object
    periods = frequency(train)
  }
  nPeriods = length(periods)
  
  #the window size is the amount of data to be held back for the test matrix
  #use the minimum period to build training and test matrix
  window_size = min(as.numeric(periods))

  #must be a multiple of periods > hf
  if(hf>0) {
    window_size = window_size * ceiling(hf/window_size)
  }

  #select max period for decomposition
  period = max(periods)
  
  #length of training set
  N <- length(train)
  
  #turn oject into an msts object for creating fourier series
  train_msts = msts(as.vector(train),seasonal.periods = periods)
  
  #turn object into a ts object for STL decomposition
  train_ts = ts(as.vector(train),frequency=period)
  
  #calculate fourier components. This is an input for randomForest
  KK = rep(KK,nPeriods)
  fuur <- fourier(train_msts, K = KK)
  
  #decomposition so that we can detrend
  decomp_ts <- stl(train_ts, s.window = "periodic", robust = TRUE)
  
  #season component
  y_season = decomp_ts$time.series[,1]
  
  #detrended series (season + remainder)
  y_detrended <- rowSums(decomp_ts$time.series[, c(1,3)])
  
  #trend component
  y_trend <- ts(decomp_ts$time.series[,2])
  
  #fit a model for trend. It can be most any modeling method, using auto.arima here
  trend_fit <- auto.arima(y_trend, seasonal = FALSE)
  
  #reduce training data by 1 window size (the windows size amount is being held back)
  N_lag = N-window_size
  
  #the lagged seasonal values
  lag_seas <- y_season[1:N_lag]

  #training matrix for random forest
  #we are trying to forecast the detrended series, so Y = the the detrended series, minus the oldest N=window_size values
  #the X variables into the matrix are the corresponding fourier components and lagged seasonal component
  matrix_train <- data.frame(y = tail(y_detrended, N_lag),
                             tail(fuur, N_lag),
                             Lag = lag_seas)
  
  #train the random forest
  tree <- randomForest( y ~ ., data = matrix_train,
                        ntree = ntree, mtry = mtry, nodesize = nodesize, importance = TRUE)
  
  #calculate combined fitted and residuals
  tree_fitted = tree$predicted
  
  y_detrended = ts(y_detrended)
  tsp(y_detrended) <- tsp(train_ts)
  
  z = time(y_detrended)
  tree_fitted = ts(tree$predicted,start=z[window_size+1],frequency=frequency(y_detrended))
  trend_fitted = ts(trend_fit$fitted,start=z[window_size+1],frequency=frequency(y_detrended))
  
  fitted = tree_fitted + trend_fitted
  residuals = train_ts - fitted
  
  #build a model object
  ret_obj = list(
    x = train,
    hf = hf,
    fitted = fitted,
    residuals = residuals,
    trend_fit = trend_fit,
    ntree = ntree,
    rf = tree,
    mtry = mtry,
    KK = KK,
    nodesize = nodesize,
    y = train_ts,
    periods = periods,
    period = period,
    window_size = window_size,
    y_detrended = y_detrended,
    y_trend = y_trend,
    y_season = y_season,
    fuur = fuur,
    matrix_train = matrix_train
  )
  
  class(ret_obj) = c(class(ret_obj),"tsrf")
  
  return(ret_obj)
}

#This uses bootstrapping to calculate prediction intervals
#xx = original time series
#rr = residuals
#ff = forecast
pi_bootstrap = function(xx, rr, ff) {
  
  npaths = 1000
  h = length(ff$mean)
  
  sim <- matrix(NA, nrow = npaths, ncol = h)
  
  #residuals should have a mean of 0
  rr_samples = rr-mean(rr,na.rm = TRUE)
  level=c(80,95)
  nint = length(level)
  
  if( sum(is.na(rr_samples)) == length(rr_samples) ) {
    browser()
  }
  
  for (i in 1:npaths) {
    sim[i,] <- sample(rr_samples, h, replace=TRUE)
  }
  
  lower <- apply(sim, 2, quantile, 0.5 - level / 200, type = 8, na.rm=TRUE)
  upper <- apply(sim, 2, quantile, 0.5 + level / 200, type = 8, na.rm=TRUE)
  
  lower = apply(lower, 1, function(col) { ff$mean+col})
  upper = apply(upper, 1, function(col) { ff$mean+col})
  
  #must convert back to a matrix if forecast horizon was 1
  if(h==1) {
    lower = t(as.matrix(lower))
    upper = t(as.matrix(upper))
  }
  
  labels = paste(level, "%", sep = "")
  colnames(lower) <- colnames(upper) <- labels
  
  lower <- ts(lower)
  upper <- ts(upper)
  tsp(lower) <- tsp(upper) <- tsp(ff$mean)
  
  ff$lower = lower
  ff$upper = upper
  ff$level = level
  
  return(ff)
}

#m - object returned from tsrf
#forecast horizon was defined in tsrf
forecast.tsrf = function(m) {
  
  #get forecast horizon
  h = m$hf
  
  #the holdout size
  x = m$x
  window_size = m$window_size
  periods = m$periods
  
  N = length(x)
  N_lag = N - window_size
  
  #forecast trend h steps ahead
  trend_for = forecast(m$trend_fit, h=h)
  
  #forecast fourier series for h steps ahead
  fuur_test <- as.data.frame(fourier(msts(x,seasonal.periods = periods), K = m$KK, h = h))
  
  #lagged seasonal component
  test_lag <- m$y_season[(N_lag+1):(N_lag+h)]
  
  #test matrix for random forest prediction (no Y values)
  matrix_test <- data.frame(fuur_test,
                            Lag = test_lag)
  
  #combine detrended forecast and trend forecast
  pred_tree <- predict(m$rf, matrix_test) + trend_for$mean
  
  #now build forecast object to return
  out <- list(x=x,series=x,method="tsrf",fitted = m$fitted, residuals=m$residuals)
  
  #calculate times of forecast
  tspx <- tsp(x)
  if(!is.null(tspx)) {
    start.f <- tspx[2] + 1/frequency(out$x)
  } else {
    start.f <- length(out$x)+1
  }
  
  #create forecast series
  out$median = out$mean <- ts(pred_tree,frequency=frequency(x),start=start.f)
  
  #bootstrap to get prediction intervals
  out = pi_bootstrap(m$y, m$residuals, out)
  
  class(out) <- "forecast"
  return(out)
}


