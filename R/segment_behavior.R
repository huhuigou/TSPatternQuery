#Segment Behavior Functions



rising <- function(segment){}#rising on avg

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
