#'Query A Time Series Using a Sliding Window Approach
#'
#'Queries a given time series using a sliding window and Spearman Ranking Correlation Coefficient for
#'similarity assessment between each window and a given pattern.
#'
#'@param timeseries The time series to be queries for the pattern
#'@param pattern.template The time series that represents a template of the pattern being searched for
#'@param window.length The length of the sliding window, which the pattern.template will be matched against.
#'Defaults to 1.2 times the length of the template pattern.
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


  num.patterns.found <- 0
  timeseries.length <- length(timeseries)

  i <- 1
  while(i<timeseries.length-length(pattern.template)){
    window.time.subset <- paste(time(timeseries[i]), "/" ,time(timeseries[i])+window.length, sep="" )
    window <- timeseries[window.time.subset]
    print(window)
    pips <-GetPIPs(window, length(pattern.template))
    matches <- MatchPattern(pips, pattern.template, spearmans.rho.threshold)
    print(matches)
    if(matches){
      plot(timeseries[window.time.subset])
      num.patterns.found <- num.patterns.found+1
      i <- i+length(window)
    }
    else{
      i <- i+1
    }
  }

  return(num.patterns.found)
}

#' Returns the Length of a Time Series
GetTimeLength <- function(timeseries){
  t1.num <- as.numeric(time(timeseries[1]))
  t2.num <- as.numeric(time(timeseries[length(timeseries)]))
  diff <- t2.num - t1.num
  return(diff)
}



