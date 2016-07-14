
#'Select a Time Series Segment
#'
#'Specify a segment of the time series in the future using seconds from the current point
#'@param index The index of the current point in the series
#'@param timeseries The time series as xts
#'@param time1 The beginning of the segment specified by the number of seconds from the current point.
#'Use a negative (-) number to specify that the segment begins in the past.
#'@param time2 The end of the semgent specified by the number of seconds from the current point.
#'Use a negative (-) number to specify that the segment ends in the past.
#'@return A subset of the xts time series containing all points that fall within the segment.
#'@export
selectSegment <- function(index, timeseries, time1, time2){
  stopifnot(time1 < time2)

  time1 = time(timeseries[index]) + time1
  time2 = time(timeseries[index]) + time2
  return(timeseries[paste(time1,"/",time2, sep="")])
}





