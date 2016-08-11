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
