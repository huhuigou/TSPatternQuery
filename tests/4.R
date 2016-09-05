#'Test Case for create_time_series.R
#'@import RUnit, xts
library(RUnit)
library(TSPatternQuery)

test.CreateCustomTimeSeries <- function() {
  ts = CreateCustomTimeSeries(
    c(1,2,3,4,5),
    c(1,2,3,4)
  )

#Check the object creates is xts
checkTrue(xts::is.xts(ts))

#Check one of the values and intervals in the time series to ensure they are correct.
#Also check it is the correct length.
checkTrue(ts[[2]]==2)

checkTrue(time(ts[2]) - time(ts[1]) == 1)

checkTrue(length(ts)==5)

#Check that exceptions are thrown when length(intervals) != length(values)-1
checkException(CreateCustomTimeSeries(
  c(1,2,3,4,5),
  c(1,2,3)
))

checkException(CreateCustomTimeSeries(
  c(1,2,3,4,5),
  c(1,2,3,4,5)
))

checkException(CreateCustomTimeSeries(
  c(1,2,3,4,5),
  c(1,2,3,4,5,6)
))

}
