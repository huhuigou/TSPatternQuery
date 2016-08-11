#'Tests for pattern_matching.R
#'@import RUnit, xts, TSTestDataUtil
library(RUnit)
library(TSTestDataUtil)

test.MatchPattern <- function(){

  template.hns <- CreateCustomTimeSeries(
    c(1,10,5,15,5,10,1),
    c(5,5,5,5,5,5)
    )
  skewed.hns <- CreateCustomTimeSeries(
    c(1,4,3,5.1,3,4,2),
    c(5,5,5,5,5,5)
    )
  ones <- CreateCustomTimeSeries(
    c(1,1,1,1,1,1,1),
    c(5,5,5,5,5,5)
  )

  #Idealized head and shoulders pattern matches itself when threshold is set very high
  checkTrue(MatchPattern(template.hns, template.hns, 0.999))

  #Idealized head and shoulders pattern matches skewed one at default threshold
  checkTrue(MatchPattern(skewed.hns, template.hns))

  #Idealized head and shoulders pattern does not match skewed one when threshold set to very high
  checkTrue(!MatchPattern(template.hns, skewed.hns, 0.999))

  #FALSE is returned when the timeseries is flat (i.e. variance=0), regardless of threshold
  checkTrue(!MatchPattern(ones, template.hns, threshold = 0))

  #Exception is thrown when the pattern.template is flat (i.e. variance=0)
  checkException(MatchPattern(skewed.hns, ones))

}
