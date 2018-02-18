#Getting Started-------------------------------------------------------------------
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

#Moving On-----------------------------------------------------------------------

#Create new S4 class door

#Constructor Function
setClass(Class="door",
         representation = representation(
           chosenDoor="numeric", #class numeric
           carDoor="numeric",  #class numeric
           switch="logical"  #class logical
         ),
         prototype= prototype(
           chosenDoor=sample(1:3,1), #data for the items
           carDoor=sample(1:3,1),
           switch=sample(c(T,F), 1)
         )
)

#Set validity

setValidity("door", function(x){
  testChosen<-any(x@chosenDoor==1 | x@chosenDoor==2 |x@chosenDoor==3) #object must be 1,2, or 3
  testCar<-any(x@carDoor==1 | x@carDoor==2 |x@carDoor==3)
  testSwitch<-any(x@switch==T | x@switch==F)
  if(testChosen==F){
    print("chosenDoor value not valid")
  }
  if(testCar==F){
    print("carDoor value not valid")
  }
  if(testSwitch==F){
    print("switch must be a logical")
  }
}
)

#Do this because reasons
setMethod("initialize", "door", function(.Object, ...){
  value=callNextMethod() 
  validObject(value)
  return(value)
})

new("door") ##########When I set the class, this will "randomly" create the same data each time.
            # If I reset the class, different data will repeat

new("door", chosenDoor="m", carDoor="w", switch="P") #test the representation ; this will throw an error 
new("door", chosenDoor=9, carDoor="w", switch=T) #test the validity; this will throw an error
new("door", chosenDoor=2, carDoor=4, switch=T) #test the validity; this will throw an error
new("door", chosenDoor=2, carDoor=1, switch="A") #test the representation; this will throw an error









