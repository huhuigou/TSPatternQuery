#'Test Case for query
#'@import RUnit, xts, TSTestDataUtil
library(RUnit)
library(TSTestDataUtil)





test.query <- function() {
  wrongTS1 = createCustomTimeSeries(c(2.0, 2.9, 6.0))
  wrongTS2 = createCustomTimeSeries(c(6.0, 7.0, 2.7, 2.9, 8.0, 3.0))
  correctTS = createCustomTimeSeries(c(4.5, 2.7, 2.9, 6.0, 6.6))

  p1 = function(index, timeseries){
    if(index-1 < 1){
      return(FALSE)
    }
    point = timeseries[[index]]
    previousPoint <- timeseries[[index-1]]
    return(point==2.9 && previousPoint == 2.7)
  }

  p2 = function(index, timeseries){
      if(index-1 < 1){
        return(FALSE)
      }
      point = timeseries[[index]]
      nextPoint <- timeseries[[index+1]]
      return(nextPoint==6.0 && point == 2.9)
    }

  patterns = c(p1, p2)
  checkTrue( !query(wrongTS1, patterns = patterns) )
  checkTrue( !query(wrongTS2, patterns = patterns) )
  checkTrue( query(correctTS, patterns = patterns) )
}





