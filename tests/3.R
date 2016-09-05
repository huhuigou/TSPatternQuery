#'Tests for query.R
#'@import RUnit, xts, TSTestDataUtil
library(RUnit)
library(TSPatternQuery)

test.Query <- function(){

  timeseries.long.intervals <- CreateCustomTimeSeries(
    c(1,2,3,4,5,6,7),
    c(60*60, 60*60, 60*60, 60*60, 60*60, 60*60)
  )
  timeseries.short.intervals <- CreateCustomTimeSeries(
    c(1,2,3,4,5,6,7),
    c(10, 10, 10, 10, 10, 10)
  )
  timeseries.var.zero <- CreateCustomTimeSeries(
    c(2,2,2,2,2,2,2),
    c(10, 10, 10, 10, 10, 10)
  )
  pattern <- CreateCustomTimeSeries(
    c(1, 8, 5, 11, 5, 8, 1),
    c(10, 10, 10, 10, 10, 10)
  )
  #Errors that should be caught: window too small and window var=0
  checkTrue(Query(timeseries.long.intervals, pattern.template = pattern)[[1]] == 0)
  checkTrue(Query(timeseries.var.zero, pattern.template = pattern)[[1]] == 0)


  #Errors that should be thrown: timeseries not xts, template.pattern not xts, spearmans.rho.threshold > 1,
  #spearmans.rho.threshold < 0, template.pattern var=0
  checkException(Query(c(1,2,3,4), pattern))
  checkException(Query(timeseries.short.intervals, c(1,2,3,4)))
  checkException(Query(timeseries.short.intervals, pattern, spearmans.rho.threshold = 1.1))
  checkException(Query(timeseries.short.intervals, pattern, spearmans.rho.threshold = -1))
  checkException(Query(timeseries.short.intervals, timeseries.var.zero))


  #Output that should be returned: data.frame with number of patterns and erros when
  #return.matched.patterns == FALSE, list of matched windows when return.matched.patterns == TRUE
  timeseries.with.two.patterns <- CreateCustomTimeSeries(
    c(0.9, 1.1, -0.99, 1.11, 2.1, 5.7, 4.3, 6.2, 4.1, 5.5, -2,
      -.022, 0.1, 2, 2, 10.3, 5, 13.7, 6.2, 9.99, -2, 1, 3, 3.1),
    c(7, 9, 9, 5, 8, 2, 10, 4, 7, 5, 7, 6, 7, 6, 4, 4, 5, 6, 10, 1, 8, 7, 3)
  )

  checkEquals(
    2,
    length(
      Query(timeseries.with.two.patterns, pattern, window.length = 50, return.matched.patterns = TRUE)
    )
  )

  #Matches should also match ruleset if provided: A ruleset that always returns TRUE should have no effect.
  #A ruleset that always returns FALSE should result in 0 matches. A rulset that selects head and shoulders
  #patterns where the middle peak is >10 should only select the window that fits that criteria.
  ruleset.always.TRUE <- function(window, pips){
    return(TRUE)
  }
  ruleset.always.FALSE <- function(window, pips){
    return(FALSE)
  }
  ruleset.middle.peak.over.10 <- function(window, pips){
    if(window[[4]]>10){
      return(TRUE)
    }
    return(FALSE)
  }
  checkEquals(
    2,
    Query(
      timeseries.with.two.patterns,
      pattern,
      window.length = 50,
      ruleset = ruleset.always.TRUE
    )[[1]]
  )
  checkEquals(
    0,
    Query(
      timeseries.with.two.patterns,
      pattern,
      window.length = 50,
      ruleset = ruleset.always.FALSE
    )[[1]]
  )
  checkEquals(
    1,
    Query(
      timeseries.with.two.patterns,
      pattern,
      window.length = 50,
      ruleset = ruleset.middle.peak.over.10
    )[[1]]
  )

  #Matches should also match distinctive.feature if provided, using the same conditions as for the
  #ruleset parameter (above)
  df.always.TRUE <- function(window){
    return(TRUE)
  }
  df.always.FALSE <- function(window){
    return(FALSE)
  }
  df.middle.peak.over.10 <- function(window){
    if(window[[4]]>10){
      return(TRUE)
    }
    return(FALSE)
  }
  checkEquals(
    2,
    Query(
      timeseries.with.two.patterns,
      pattern,
      window.length = 50,
      distinctive.feature = df.always.TRUE
    )[[1]]
  )
  checkEquals(
    0,
    Query(
      timeseries.with.two.patterns,
      pattern,
      window.length = 50,
      distinctive.feature = df.always.FALSE
    )[[1]]
  )
  checkEquals(
    1,
    Query(
      timeseries.with.two.patterns,
      pattern,
      window.length = 50,
      distinctive.feature = df.middle.peak.over.10
    )[[1]]
  )
  #Errors in the distinctive feature function should be caught because there is no gaurantee of
  #enough points being present within the window for the user's function to work, given that time
  #series may be irregular.
  df.error <- function(window){
    stop("This is an error")
  }

  checkEquals(
    0,
    Query(
      timeseries.with.two.patterns,
      pattern,
      window.length = 50,
      distinctive.feature = df.error
    )[[1]]
  )

  #Errors in the rulset function should not be caught, because the number of pips has already been verified
  #as sufficient for the template pattern by the GetPIPs function.
  ruleset.error <- function(window, pips){
    stop("This is an error")
  }

  checkException(
    Query(
      timeseries.with.two.patterns,
      pattern,
      window.length = 50,
      ruleset = ruleset.error
    )
  )

  #Query should not crash when distinctive.feature returns NA
  return.NA <- function(window){
    return(NA)
  }

  checkEquals(
    0,
    Query(
      timeseries.with.two.patterns,
      pattern,
      window.length = 50,
      distinctive.feature = return.NA
    )[[1]]
  )
}
