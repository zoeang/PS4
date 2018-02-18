#Getting Started
montyhall<-function(pick){ #I took away the second door argument because the user shouldn't be allowed to decide that
  pick<-sample(1:3, 1)
  car<-sample(1:3, 1)
  if (pick==car){ 
    x<-TRUE 
  } else {
    x<-FALSE 
    }
  x
}
montyhall()
# Should return a TRUE if these samples are equal and
# a false if they are not