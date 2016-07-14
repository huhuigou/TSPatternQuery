#'Query A Time Series
#'@import xts
#'@param timeseries An xts timeseries
#'@param df A Distinctive Feature of the pattern that the query is searching for.
#'This function should take an xts object of length 1.
#'Must be of the exact form function(index, timeseries), where index is numeric and timeseries is
#'xts. This function should return true if the desired pattern at "index" is found within
#'"timeseries".
#'@param patterns A vector of functions of the same exact form as df: function(index, timeseries),
#'where index is numeric and timeseries is xts. Each function should return true if the desired
#'pattern at "index" is found within "timeseries".
#'@export
library(xts)
query <- function(timeseries, df, patterns) {
  stopifnot(xts::is.xts(timeseries))
  stopifnot(is.vector(patterns))
  stopifnot(length(patterns) > 0)

  #TODO find a way to validate the names of the arguments. Also do this for df.

  if (!missing(df)) {
    stopifnot(formals(df) == 2)
    stopifnot(is.function(df))
    patterns = c(df, patterns)
  }

  for (i in 1:length(timeseries)) {
    p = 1
    while (patterns[[p]](index = i, timeseries = timeseries)) {
      p = p + 1
      if (p > length(patterns)) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}
