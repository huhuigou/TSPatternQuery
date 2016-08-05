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
