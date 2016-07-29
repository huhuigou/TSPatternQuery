#'Retrieve Perceptually Important Points (PIPS) from a Time Series
#'
#'Perceptually Important Points (PIPs) are points that are perceptually important for the identification
#'of patterns in time series data. Each PIP is identified in this function by choosing the point with
#'the maximum perpendicular distance to a line drawn between adjacent PIPs. Endpoints of the time series
#'are chosen as the first PIPS, and the process is repeated iteratively until the desired number of
#'PIPs are found.
#'
#'@references
#'Zhe Zhang, Jian Jiang, Xiaoyan Liu, Ricky Lau, Huaiqing Wang, and Rui Zhang. A real time hybrid pattern matching scheme for stock time series. In Proceedings of the Twenty-First Australasian Conference on Database Technologies - Volume 104, ADC ’10, pages 161–170, Darlinghurst, Australia, Australia, 2010. Australian Computer Society, Inc.
#'
#'@param timeseries The xts time series within which PIPs will be identified
#'@param num.pips A numeric for the number of PIPs to be identified
#'@return An xts time series containing the pips from the timeseries provided
#'@export
GetPIPs <- function(timeseries, num.pips) {
  stopifnot(num.pips <= length(timeseries))
  is.pip <- vector(mode = "logical", length = length(timeseries))
  #Set first 2 PIPs to be endpoints
  is.pip[1] <- TRUE
  is.pip[length(timeseries)] <- TRUE
  #Determine which points are PIPs, and set is.pip[i] <- TRUE, where i is the index of those points
  perp.dist <- vector(mode = "numeric", length = length(timeseries) - 2)
  i <- 1
  while (i < num.pips - 1) {
    perp.dist <- EnumeratePerDistVector(is.pip, timeseries, perp.dist)
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
#'and is thus highly coupled to GetPIPs. It simply enumerates
#'the perp.dist vector with perpendicular distances between each
#'non-PIP point and the line connecting said point's adjacent
#'PIPs.
#'
#'@param is.pip A logical vector, which is TRUE at the index of each
#'point (from timeseries) which has been identified as a PIP
#'@param timeseries The time series from from which PIPs are being
#'identified
#'@param perp.dist The numeric perpendicular distance vector which this
#'function is filling with perpendicular distances
#'@return The perp.dist vector enumerated with newly calculated
#'perpendicular distances.
EnumeratePerDistVector <- function(is.pip, timeseries, perp.dist) {
  j = 1
  while (j < length(timeseries)) {
    #sets per.dist of pips to -1 so they will not be selected as a pip twice by the which.max function
    if (is.pip[j]) {
      perp.dist[j-1] <- -1
      j <- j + 1
      next()
    }
    left.pip.index <- GetAdjacentPIPIndex(j, is.pip, side="left")
    right.pip.index <- GetAdjacentPIPIndex(j, is.pip, side="right")
    perp.dist[j-1] <- GetPerpDist(timeseries[j], timeseries[left.pip.index], timeseries[right.pip.index])
    j <- j + 1
  }
  return(perp.dist)
}

#'Get one of the Adjacent Perceptually Important Points (PIPs) to the Point Provided
#'
#'Retreives a the nearest PIP to the point designated by index on the side designated
#'by side
#'
#'@param index The numeric index of the point in is.pip, for which the adjacent pip
#'will be retrieved
#'@param is.pip A logical vector of points of the same length of the time series
#'in question. TRUE, indicates that the point is a pip.
#'@param side May be either "left" or "right". Designated the side on which the adjacent
#'pip is to be found
#'@return The index of the closes pip on the designated side of the given point.
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

#'Get the Perpendicular Distance Between Point and the line connecting left.point and right.point.
#'
#'This is a helper function for EnumeratePerDist, which in turn is a helper function for
#'GetPIPs. Thus, these functions are highly coupled, and exist simply to make the code more
#'readable. This function is made available to the user because it could be useful on its own.
#'
#'@references
#'Wikipedia. Distance from a point to a line — wikipedia, the free encyclopedia, 2016. [Online; accessed 26-July-2016].
#'
#'@param point The point (xts object with length=1) in the middle. Distance will be calculated between this point and the line.
#'@param left.point The left point (xts object with length=1) that will constitute the left endpoind of the line
#'@param right.point The right point (xts object with length=1) that will constitute the right endpoint of the line
#'@return The numeric perpendicular distance
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
