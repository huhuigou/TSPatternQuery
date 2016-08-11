#'Tests for get_pips.R
#'@import RUnit, xts, TSTestDataUtil
library(RUnit)
library(TSTestDataUtil)

test.GetPIPs <- function() {
  ts <- CreateCustomTimeSeries(
    c(1,-5,0,4,0,1,-6,1,0,0),
    c(20,20,20,20,20,20,20,20,20)
    )
  pips.1 <- GetPIPs(ts, 1)
  pips.2 <- GetPIPs(ts, 2)
  pips.3 <- GetPIPs(ts, 3)
  pips.6 <- GetPIPs(ts, 6)
  pips.10 <- GetPIPs(ts, 10)

  #Check that first and last pip are always endpoints
  checkEquals(ts[[1]], pips.1[[1]])
  checkEquals(ts[[10]], pips.1[[2]])
  checkEquals(ts[[1]], pips.2[[1]])
  checkEquals(ts[[10]], pips.2[[2]])
  checkEquals(ts[[1]], pips.3[[1]])
  checkEquals(ts[[10]], pips.3[[3]])
  checkEquals(ts[[1]], pips.6[[1]])
  checkEquals(ts[[10]], pips.6[[6]])
  checkEquals(ts[[1]], pips.10[[1]])
  checkEquals(ts[[10]], pips.10[[10]])

  print(pips.3)
  print(pips.6)

  #check exception thrown when num.pips > length(timeseries)
  checkException(GetPIPs(ts, 11))

  #Check pips identified in correct order
  checkEquals(ts[[7]], pips.3[[2]])

  test.6 <- as.vector(pips.6)
  actual.6 <- c(
    ts[[1]],
    ts[[2]],
    ts[[4]],
    ts[[7]],
    ts[[8]],
    ts[[10]]
    )
  checkEquals(actual.6, test.6)
}

test.GetPerpDist <- function() {
  three.points <- CreateCustomTimeSeries(c(0,2,0), c(2,2))
  actual.perp.dist <- 2

  test.perp.dist <-GetPerpDist(three.points[2], three.points[1], three.points[3])
  RUnit::checkEquals(actual.perp.dist, test.perp.dist, msg = "The perp.dist calculation is incorrect for a simple example.")
}
