#'Query A Time Series Using a Sliding Window Approach
#'
#'Queries a given time series using a sliding window and Spearman Ranking Correlation Coefficient for
#'similarity assessment between each window and a given pattern.
#'
#'@references
#'Zhe Zhang, Jian Jiang, Xiaoyan Liu, Ricky Lau, Huaiqing Wang, and Rui Zhang. A real time hybrid pattern matching scheme for stock time series. In Proceedings of the Twenty-First Australasian Conference on Database Technologies - Volume 104, ADC ’10, pages 161–170, Darlinghurst, Australia, Australia, 2010. Australian Computer Society, Inc.
#'
#'
#'@param timeseries The xts time series to be queried for the pattern
#'@param pattern.template The xts time series that represents a template of the pattern being searched for
#'@param ruleset Optional argument. A function of the form function(xts), which returns TRUE
#'if the xts object matches the ruleset and false otherwise.
#'@param window.length A numeric length (in seconds) for the sliding window, which the pattern.template
#'will be matched against. Defaults to 1.2 times the length of the template pattern.
#'@param spearmans.rho.threshold The numeric threshold used for the Spearman's rho similarity coefficient.
#'This values should above 0 and less than 1. Setting this values closer to 1 ensures that only
#'very similar time series segments will match. Arbitrarily defaults to 0.7.
#'@param return.matched.patterns A logical value. TRUE makes the algorithm return each window that matched
#'as a list of xts objects. FALSE returns a 1 x 2 data frame with columns "Patterns" = Number of patterns
#'found, and "Errors" = number of exceptions thrown by the GetPIPs function (e.g., incidences of there
#'being too few data points found in the window to identify enough PIPs).
#'Defaults to FALSE.
#'@return Either a list of matched windows or a data frame containing the number of matches and
#'errors, depending on the return.matched.patterns parameter.
#'@import xts
#'@export
Query <- function(timeseries,
                  pattern.template,
                  ruleset,
                  window.length = 1.2*GetTimeLength(pattern.template),
                  spearmans.rho.threshold = 0.7,
                  return.matched.patterns = FALSE
                  ) {
  library(xts)
  stopifnot(is.xts(timeseries))
  stopifnot(is.xts(pattern.template))
  stopifnot(spearmans.rho.threshold > 0)
  stopifnot(spearmans.rho.threshold < 1)
  if(!missing(ruleset)){
    stopifnot(is.function(ruleset))
  }
  if(var(pattern.template)==0){
    stop("The variance of the pattern.template cannot be 0 (e.g., all values cannot be identical).
         Please choose a pattern.template that is not completely flat .")
  }


  num.patterns.found <- 0
  num.errors <- 0
  patterns <- list()

  i <- 1
  while(i<length(timeseries)-length(pattern.template)){
    window.time.subset <- paste(time(timeseries[i]), "/" ,time(timeseries[i])+window.length, sep="" )
    window <- timeseries[window.time.subset]

    pips <- tryCatch(
      pips <- GetPIPs(window, length(pattern.template)),
      error=function(e){
        num.errors <- num.errors + 1
        return(e)
        }
    )
    if(inherits(pips, "error")){
      i <- i+1
      next()
    }

    matches <- MatchPattern(pips, pattern.template, spearmans.rho.threshold)
    if(!missing(ruleset) && !ruleset(window)){
      matches <- FALSE
    }
    if(matches){
      num.patterns.found <- num.patterns.found+1
      if(return.matched.patterns) {
        patterns[[num.patterns.found]] <- window

      }
      i <- i+length(window)
    }
    else{
      i <- i+1
    }
  }
  if(return.matched.patterns){
    return(patterns)
  }
  return(
      data.frame("Patterns" = num.patterns.found, "Errors" = num.errors)
    )
}

#' Returns the Length of a Time Series in Seconds
#'
#' A helper method for Query, returns the length of an xts timeseries in seconds
#'
#' @param timeseries An xts timeseries
#' @return The numeric length of the time series in seconds.
GetTimeLength <- function(timeseries){
  t1.num <- as.numeric(time(timeseries[1]))
  t2.num <- as.numeric(time(timeseries[length(timeseries)]))
  diff <- t2.num - t1.num
  return(diff)
}

#' TODO: Make sure the below is properly cited, not sure if just copying documentation is enoug:
#' @references
#' demo(error.catching)
#'
##================================================================##
###  In longer simulations, aka computer experiments,		         ###
###  you may want to		                                         ###
###  1) catch all errors and warnings (and continue)		         ###
###  2) store the error or warning messages			                 ###
###							                                                 ###
###  Here's a solution	(see R-help mailing list, Dec 9, 2010):	 ###
##================================================================##

##' Catch *and* save both errors and warnings, and in the case of
##' a warning, also keep the computed result.
##'
##' @title tryCatch both warnings (with value) and errors
##' @param expr an \R expression to evaluate
##' @return a list with 'value' and 'warning', where
##'   'value' may be an error caught.
##' @author Martin Maechler;
##' Copyright (C) 2010-2012  The R Core Team
tryCatch.W.E <- function(expr)
{
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}

