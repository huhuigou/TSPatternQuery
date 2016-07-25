#'Query A Time Series Using a Sliding Window Approach
#'
#'Queries a given time series using a sliding window and Spearman Ranking Correlation Coefficient for
#'similarity assessment between each window and a given pattern.
#'
#'PROVIDE CITATION
#'
#'@param timeseries The xts time series to be queried for the pattern
#'@param pattern.template The xts time series that represents a template of the pattern being searched for
#'@param window.length A numeric length (in seconds) for the sliding window, which the pattern.template
#'will be matched against. Defaults to 1.2 times the length of the template pattern.
#'@param spearmans.rho.threshold The numeric threshold used for the Spearman's rho similarity coefficient.
#'This values should above 0 and less than 1. Setting this values closer to 1 ensures that only
#'very similar time series segments will match. Arbitrarily defaults to 0.7.
#'@return The number of matched patterns in the series
#'@import xts
#'@export
Query <- function(timeseries,
                  pattern.template,
                  window.length = 1.2*GetTimeLength(pattern.template),
                  spearmans.rho.threshold = 0.7
                  ) {
  library(xts)
  stopifnot(is.xts(timeseries))
  stopifnot(is.xts(pattern.template))
  stopifnot(spearmans.rho.threshold > 0)
  stopifnot(spearmans.rho.threshold < 1)


  num.patterns.found <- 0


  i <- 1
  while(i<length(timeseries)-length(pattern.template)){
    window.time.subset <- paste(time(timeseries[i]), "/" ,time(timeseries[i])+window.length, sep="" )
    window <- timeseries[window.time.subset]
    pips <-GetPIPs(window, length(pattern.template))
    matches <- MatchPattern(pips, pattern.template, spearmans.rho.threshold)
    if(matches){
      num.patterns.found <- num.patterns.found+1
      i <- i+length(window)
    }
    else{
      i <- i+1
    }
  }

  return(num.patterns.found)
}

#' Returns the Length of a Time Series in Seconds
#'
#' A helper method for Query, returns the length of an xts timeseries in seconds
#'
#' @param timeseries An xts timeseries
#' @return The numeric length of the time series in seconds.
GetTimeLength <- function(timeseries){
  t1.num <- as.numeric(time(timeseries[1]))
  t2.num <- as.numeric(time(timeseries[length(timeseries)]))
  diff <- t2.num - t1.num
  return(diff)
}



