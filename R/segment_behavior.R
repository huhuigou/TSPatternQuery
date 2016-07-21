#Segment Behavior Functions

#might change to just "behavior" class because having this work for both points and segments
#might be useful, and also point_value_comparisons will prob need to be deleted.





#' Rising Point
#'
#' Returns TRUE if the time series is rising on average. If an index is provided, then it returns true
#' if the current (indexed) point is less than the next point.
#'
#'@param timeseries The time series that either contains the point referenced by index, or will be evaluated
#'as rising or not on average itself.
#'@param index The index to a point within the time series.
#'@export
#'@import xts
rising <- function(timeseries, index)
{
  #if missing an index, calculate whether the timeseries segment is rising on average or not.
  if(missing(index)){
    diff = vector(length = length(timeseries))
    for(i in 2:length(timeseries)){
      diff[i] = timeseries[[i]] - timeseries[[i-1]]
    }
    return(sum(diff) > 0)
  }

  stopifnot(index < length(timeseries))
  point = timeseries[[index]]

  nextPoint = timeseries[[index + 1]]
  return(point < nextPoint)
}

#'Returns true if the each point in the time series is larger than the previous one.
#'
#'This differs from rising in that it is absolute. Each point must be higher than the previous one in order
#'for this to return true.
#'
#'@param timeseries A time series.
#'@return TRUE if each point in the timeseries provied is higher than the previous point, FALSE otherwise.
#'@export
all.rising <- function(timeseries){
  for(i in 2:length(timeseries)){
    return(timeseries[[i]]>timeseries[[i-1]])
  }
}













gradient <- function(segment){}#gradient of linear regression line

stable <- function(segment, varThreshold){} #optional variance threshold that defaults to something arbitrary

variance <- function(segment){}



#from SDL, may be cleaner/easier without since theres some overlap between this ans rising/falling...
#utility of having a static up/down definition is questionable...
up <- function(segment){}
Up <- function(segment){}
down <- function(segment){}
Down <- function(segment)


#stuff that I may or may not use

appears <- function(segment){}#zero to nonzero

disappears <- function(segment){}#nonzero to zero
