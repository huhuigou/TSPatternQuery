#'Test Case for GetPIPs
#'@import RUnit, xts, TSTestDataUtil
library(RUnit)
library(TSTestDataUtil)

test.GetPerpDist <- function() {
  three.points <- createCustomTimeSeries(c(0,2,0), c(2,2))
  three.points.tilted <- createCustomTimeSeries(c(0, 3, 2), c(1, 3))
  actual.perp.dist <- 2
  actual.perp.dist.tilted <- 2.36 # Double check that this is right

  test.perp.dist <-GetPerpDist(three.points[2], three.points[1], three.points[3])
  test.perp.dist.tilted <-GetPerpDist(three.points.tilted[2], three.points.tilted[1], three.points.tilted[3])
  RUnit::checkEquals(actual.perp.dist, test.perp.dist, msg = "The perp.dist calculation is incorrect for a simple example.")
  RUnit::checkEquals(actual.perp.dist.tilted, test.perp.dist.tilted,
                     msg = "The perp.dist calculation is incorrect for a simple example, when the line between p1 and p2 was
                     tilted.")
}

test.GetPIPs <- function() {
  ts <- createCustomTimeSeries(c(1,2,10,2,1,-11,2,2,12,2,1,10,3,2))


}




