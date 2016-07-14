#Segment Behavior Functions


#overload to take point or segment
rising <- function(segment){


    point = timeseries[[index]]
    if(index+1<length(timeseries)){
      nextPoint = timeseries[[index+1]]
      return(nextPoint > point)
    }
    else{
      prevPoint = timeseries[[index-1]]
      return(prevPoint < point)
    }

}#rising on avg

all.rising <- function(segment){}#All points increasing from previous.is the . between all and rising okay?

falling <- function(segment){}

all.falling <- function(segment){}

gradient <- function(segment){}#gradient of linear regression line

stable <- function(segment, varThreshold){} #optional variance threshold that defaults to something arbitrary

variance <- function(segment){}



#from SDL, may be cleaner/easier without since theres some overlap between this ans rising/falling...
#utility of having a static up/down definition is questionable...
up <- function(segment){}
Up <- function(segment){}
down <- function(segment){}
Down <- function(segment)


#stuff that I may or may not use

appears <- function(segment){}#zero to nonzero

disappears <- function(segment){}#nonzero to zero
