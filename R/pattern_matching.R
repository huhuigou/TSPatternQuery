

#'Check if a Template Pattern Matches a Timeseries
#'
#'Returns TRUE the template pattern matches the timeseries (by passing a threshold for Spearman's
#'Rank Correlation Coefficient). This function only considers the y value (i.e. not the time intervals)
#'for matching purposes. It is assumed that the timeseries covers a similar length of time as the
#'pattern.template. Returns FALSE if the variance of the timeseries parameter is 0, since
#'cor.test(..., method="spearman") cannot handle that.
#'
#'@references
#'Zhe Zhang, Jian Jiang, Xiaoyan Liu, Ricky Lau, Huaiqing Wang, and Rui Zhang. A real time hybrid pattern matching scheme for stock time series. In Proceedings of the Twenty-First Australasian Conference on Database Technologies - Volume 104, ADC ’10, pages 161–170, Darlinghurst, Australia, Australia, 2010. Australian Computer Society, Inc.
#'
#'@param timeseries The xts time series to be checked agains the pattern.template to see if it
#'exhibits the pattern. Must have the same number of points as the pattern.template
#'@param pattern.template An xts time series representation of a pattern. Must have the same
#'number of points as the timeseries.
#'@param threshold The threshold for the spearman's rho. Default is arbitrarily set to 0.7
#'@return TRUE if the spearman ranking correlation coefficient is higher than the set threshold.
#'@export
MatchPattern <- function(timeseries, pattern.template, threshold=0.7){
  stopifnot(length(timeseries) == length(pattern.template))
  if(var(pattern.template)==0){
    stop("The pattern.template must not be flat (i.e. variance = 0). Please use a pattern.template
         with values that can be ranked for Spearman's rho.")
  }
  if(var(timeseries)==0){
    return(FALSE)
  }
  ts <- as.vector(timeseries)
  pt <- as.vector(pattern.template)

  tryCatch.W.E(rho<-cor.test(ts, pt, method="spearman")[[4]])
  return( as.logical(rho > threshold) )
}
