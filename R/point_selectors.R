
future <- function(distance, before, after){}#overload to take distance in points or time interval

past <- function(distance, before, after){}

#Can I think of better names for these (next is already taken for flow control in loops)

#overload above functions to take distance in points or POSix time.All arguments are optiona (default)
#to distance=1 point. Before and after return vectors. distance returns a vector if given a POSix time.
#if only before is given, will probably have to default after or something. Might be a better way to do
#it than a bunch of if statements or switch. Need to look into polymorphic functionf for R.
