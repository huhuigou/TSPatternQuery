#'Test Case for query
#'@import RUnit, xts, TSTestDataUtil
library(RUnit)
library(TSTestDataUtil)
test.createTimeSeries <- function() {
  ts <- createTimeSeries(10, 1, 2, 1, 5, 1)
}
