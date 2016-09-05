#'Create a Custom Time Series
#'
#'Create an xts time series using an vector of values and a vector of time intervals.
#'
#'@param values A numeric vector of values for the time series
#'@param intervals A numeric vector of time intervals for the time series. This should be one
#'item shorter than values. Intervals are set in seconds where 60 would equal 1 minute.
#'
#'@return An xts time series
#'@export
CreateCustomTimeSeries <- function(values, intervals){

  if(missing(intervals)){
    intervals = sample(60 : 60*60*60, length(values)-1)
  }
  else{
    stopifnot(length(intervals) == length(values)-1)
  }

  time = Sys.time()
  timeIndex <- c(time)
  for(i in 1:length(intervals)){
    time = time + intervals[i]
    timeIndex = append(timeIndex, time)
  }

  xtsObj = xts::xts(values, order.by = timeIndex)

  return(xtsObj)
}





#'Plot the PDF of Variances for Windows of a Given Length in a Time Series
#'
#'Scans through the provided time series with a window of window.length size, and records the
#'variances at each offset. Then finds the probability density function for said variances
#'and plots it.
#'
#'Assuming d.fun <- GetWindowVariancePDFlot() the PDF function using
#'curve(d.fun$density.fun(x), from= min(d.fun$density.points$x), to=max(d.fun$density.points$x) )
#'
#'@param timeseries The time series
#'@param window.length The length of the window with which to scan through the timeseries. Length is in number of points.
#'@return A list containing (1) a numeric representing the variance with the highest probability
#'(2) The density function, which may be plotted using the curve function. (3) The density
#'object itself, which contains 512 estimated (variance, probability) pairs.
#'@export
GetWindowVariancePDF <- function(timeseries, window.length){
 #TODO: add tests for this
  variances <- vector("numeric")
  for(i in window.length:length(timeseries)){
    window <- timeseries[(i-window.length):i]
    variances <- c(variances, var(window))
  }
  variances.density <- density(variances)
  variances.density.fun <- approxfun(variances.density$x, variances.density$y)

  results <- list()
  results[["max.prob.var"]] <- variances.density$x[which(variances.density$y==max(variances.density$y))]
  results[["density.fun"]] <- variances.density.fun
  results[["density.points"]] <- variances.density
  return(results)
}
