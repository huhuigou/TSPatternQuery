#Sample Patterns

#'plasma-Ca
#'
#'Contains a Drop Phase
#'
#'@param index The index of the current point
#'@param timeseries The time series
#'@export
dropPhase <- function(timeseries, index){
  point = index
  while(point < length(timeseries)){
    if(!rising(point, timeseries)){
      point = point + 1
    }
  }
  if(timeseries[[index]] - timeseries[[point]] > 1){
   return(TRUE)
  }
}





recoveryPhase <- function(index, timesieres){}
