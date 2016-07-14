#'Test Case for selectSegment
#'@import RUnit, xts
library(RUnit)

test.selectSegment <- function(){
  ts <- createCustomTimeSeries(c(1,2,3,4,5,6), c(60,30,30,60,30))

  seg1 = selectSegment(3, ts, -60, 60)
  seg2 = selectSegment(3, ts, 30, 60)
  seg3 = selectSegment(3, ts, -60, -30)
  seg4 = selectSegment(3, ts, -60*60*60, 60)
  seg5 = selectSegment(3, ts, 60, 60*60*60)

  checkTrue(length(seg1)==3)
  checkTrue(length(seg2)==1)
  checkTrue(length(seg3)==1)
  checkTrue(length(seg4)==4)
  checkTrue(length(seg5)==2)


  checkException(selectSegment(3, ts, 60, 60), "selectSegment allowed time1 == time2. time1
                 should < time2")
  checkException(selectSegment(3, ts, 70, 60), "selectSegment allowed time1 > time2. time1
                 should < time2")
}
