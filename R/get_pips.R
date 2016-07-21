#'Retrieve Perceptually Important Points (PIPS) Within a Time Series
#'
#'Perceptually Important Points (PIPS) are points that are perceptually important for the identification
#'of patterns in time series data. Each PIP is identified in this function by the choosing the point with
#'the maximum perpendicular distance to a line drawn between adjacent PIPS. Endpoints of the time series
#'are chosen as the first PIPS.
#'
#'PROVIDE CITATION
#'
#'@param timeseries The (xts) time series within which PIPS will be identified
#'@param num.pips A numeric for the number of PIPs to be identified.
#'@return A vector containing the indexes of the pips in the timeseries provided
#'@export
GetPIPs <- function(timeseries, num.pips) {
  timeseries.length <- length(timeseries)
  is.pip <- vector(mode = "logical", length = timeseries.length)

  #Set first 2 PIPs to be endpoints
  is.pip[1] <- TRUE
  is.pip[timeseries.length] <- TRUE

  #Determine which points are PIPs, and set is.pip[i] <- TRUE, where i is the index of those points
  i <- 1
  while (i < num.pips - 2) {
    perp.dist <- vector(mode = "numeric", length = timeseries.length - 2)
    print(i)
    print(num.pips-2)
    #Enumerate perp.dist vector with perpendicular distances
    j = 2
    while (j < timeseries.length) {
      if (is.pip[j]) {
        j <- j + 1
        next()
      }
      k <- j
      while (!is.pip[k]) {
        k <- k + 1
      }
      right.pip <- timeseries[k]

      k <- j
      while (!is.pip[k]) {
        k <- k - 1
      }
      left.pip <- timeseries[k]

      perp.dist[j-1] <- GetPerpDist(timeseries[k], left.pip, right.pip)
      j <- j + 1
    }
    index.of.pip <- which.max(perp.dist) + 1
    is.pip[index.of.pip] <- TRUE
    print(perp.dist)
    print(is.pip)
    i <- i + 1
  }

  #Find the indexes of the points where is.pip[i]==TRUE, and return those indexes.
  pip.indexes <- which(is.pip)
  pips <- timeseries[pip.indexes]
  return(pips)
}




#'Gets the perpendicular distance between point and the line connecting left.point and right.point.
#'
#'This is a helper function
#'
#'PROVIDE CITATION FOR EQUATION
#'
#'@param point The point in the middle. Distance will be calculated between this point and the line.
#'@param left.point The left point that will constitute the left endpoind of the line
#'@param right.point The right point that will constitute the right endpoint of the line
#'@return The perpendicular distance
GetPerpDist <- function(point, left.point, right.point) {
  x1 <- as.numeric(time(left.point))
  y1 <- left.point[[1]]
  x2 <- as.numeric(time(right.point))
  y2 <- right.point[[1]]
  x3 <- as.numeric(time(point))
  y3 <- point[[1]]

  slope <- (y2 - y1) / (x2 - x1)

  numerator <- x3 + slope * y3 + ((slope ^ 2) * x2) - slope * y2
  denominator <- 1 + slope ^ 2
  xc <- numerator / denominator - x3 ^ 2

  yc <- slope * xc - slope * x2 + y2

  pd <- sqrt((xc - x3) ^ 2 + (yc - y3) ^ 2)
  return(pd)
}
