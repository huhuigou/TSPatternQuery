#'Query A Time Series Using a Sliding Window Approach
#'
#'Queries a given time series using a sliding window and Spearman Ranking Correlation Coefficient for
#'similarity assessment between each window and a given pattern.
#'
#'@references
#'Zhe Zhang, Jian Jiang, Xiaoyan Liu, Ricky Lau, Huaiqing Wang, and Rui Zhang. A real time hybrid pattern matching scheme for stock time series. In Proceedings of the Twenty-First Australasian Conference on Database Technologies - Volume 104, ADC ’10, pages 161–170, Darlinghurst, Australia, Australia, 2010. Australian Computer Society, Inc.
#'
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
  if(var(pattern.template)==0){
    stop("The variance of the pattern.template cannot be 0 (e.g., all values cannot be identical).
         Please choose a pattern.template that is not completely flat .")
  }


  num.patterns.found <- 0
  num.errors <- 0

  i <- 1
  while(i<length(timeseries)-length(pattern.template)){
    window.time.subset <- paste(time(timeseries[i]), "/" ,time(timeseries[i])+window.length, sep="" )
    window <- timeseries[window.time.subset]

    pips <- tryCatch(
      pips <- GetPIPs(window, length(pattern.template)),
      error=function(e){
        num.errors <- num.errors + 1
        return(e)
        }
    )
    if(inherits(pips, "error")){
      i <- i+1
      next()
    }



    matches <- MatchPattern(pips, pattern.template, spearmans.rho.threshold)
    if(matches){
      plot(window)
      num.patterns.found <- num.patterns.found+1
      i <- i+length(window)
    }
    else{
      i <- i+1
    }
  }

  return( list( c( paste("Patterns found=",num.patterns.found), paste("Errors=",num.errors)) )  )
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



