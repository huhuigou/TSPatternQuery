

#'Check if a Template Pattern Matches a Timeseries
#'
#'If the template pattern matches the timeseries (passing a threshold for Spearman's
#'Rank Correlation Coefficient), then the timeseries is returned. This function only
#'considers the y value (i.e. not the time intervals) for matching purposes. It is assumed
#'that the timeseries covers a similar length of time as the pattern.template.
#'
#'PROVIDE CITATION (HYBRID PAPER)
#'
#'@param timeseries The xts time series to be checked agains the pattern.template to see if it
#'contains the pattern. Must have the same number of points as the pattern.template
#'@param pattern.template An xts time series representation of a pattern. Must have the same
#'number of points as the timeseries.
#'@param threshold The threshold for the spearman's rho. Default is arbitrarily set to 0.7
#'@return TRUE if the spearman ranking correlation coefficient is higher than the set threshold.
#'@export
MatchPattern <- function(timeseries, pattern.template, threshold=0.7){
  stopifnot(length(timeseries) == length(pattern.template))

  ts <- as.vector(timeseries)
  pt <- as.vector(pattern.template)
  if(var(ts,pt) == 0)
    return(TRUE)
  rho <- cor.test(ts, pt, method="spearman")[[4]]
  return( as.logical(rho > threshold) )
}
