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
    perp.dist <- EnumeratePerDistVector(timeseries.length, is.pip, timeseries, perp.dist)
    index.of.pip <- which.max(perp.dist) + 1
    is.pip[index.of.pip] <- TRUE
    i <- i + 1
  }
  #Find the indexes of the points where is.pip[i]==TRUE, and return those indexes.
  pip.indexes <- which(is.pip)
  pips <- timeseries[pip.indexes]
  return(pips)
}

#'Enumerate the per.dist Vector with Perpendicular Distances
#'
#'This is a helper method meant to make the code more readable,
#'and is thus highly coupled to GetPIPs
EnumeratePerDistVector <- function(timeseries.length, is.pip, timeseries, perp.dist) {
  j = 1
  while (j < timeseries.length) {
    if (is.pip[j]) {
      j <- j + 1
      next()
    }
    left.pip.index <- GetAdjacentPIPIndex(j, is.pip, side="left")
    right.pip.index <- GetAdjacentPIPIndex(j, is.pip, side="right")
    perp.dist[j-1] <- GetPerpDist(timeseries[j], timeseries[left.pip.index], timeseries[right.pip.index])
    j <- j + 1
  }
  print(perp.dist)
  return(perp.dist)
}


GetAdjacentPIPIndex <- function(index, is.pip, side) {
  stopifnot(side=="right" | side=="left")
  k <- index
  if(side=="right"){
    while (!is.pip[k]) {

      k <- k + 1
    }
  }
  if(side=="left"){
    while (!is.pip[k]) {

      k <- k - 1
    }
  }
  adj.pip.index <- k
  return(adj.pip.index)
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
#'@export
GetPerpDist <- function(point, left.point, right.point) {
  x1 <- as.numeric(time(left.point))
  y1 <- left.point[[1]]
  x2 <- as.numeric(time(right.point))
  y2 <- right.point[[1]]
  x3 <- as.numeric(time(point))
  y3 <- point[[1]]

  numerator <- abs( (y2-y1)*x3 - (x2-x1)*y3 + x2*y1 - y2*x1 )
  denominator <- sqrt( (y2-y1)^2 + (x2-x1)^2 )

  return(numerator/denominator)
}
