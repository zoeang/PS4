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

#Create new S4 class door--------------------------------------------------------

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



#Create new S4 Method ---------------------------------------------------------

#Create generic
setGeneric("PlayGame", function(x) { #set the name of the argument
  standardGeneric("PlayGame")
})


#New method

setMethod("PlayGame", "door",function(x){ #the x must be the same as the genernic
  #x is a number 1, 2, or 3 chosen by the player
  car<-sample(1:3,1) #this will randomly assign a number to car
  pick<-sample(1:3,1)
  trial<-new("door", chosenDoor=pick, carDoor=car) #Assign the sample correctly
  # ^this should meet the "false" condition where pick is stored in the chosenDoor slot
  #The if loop will overwrite the chosenDoor slot
  if (trial@switch==T){
    revealDoor<-c(1:3) #value of all doors
    revealDoor2 <- revealDoor[!revealDoor %in% car] #Remove the door with the car
    revealDoor3 <- revealDoor2[!revealDoor2 %in% pick] #remove the door originally picked
    trial@chosenDoor<-sample(revealDoor3,1)
  }
  if (trial@chosenDoor==trial@carDoor){
    winner<-T
  } else {
    winner<-F
    }
  winner
}
)
debug(PlayGame)

PlayGame()


#Simulation----------------------------------------------------------------
simF<-rep(NA,1000)
simT<-c(NA, 1000)
simF1000<-apply(simF, 1, PlayGame(switch=F))
sum(simF1000) #Winner=T if the person wins. Sum counts TRUE as 1. Sum will tell how many times the player won

simT1000<-apply(simT, 1, PlayGame(switch=F))
sum(simT1000)



