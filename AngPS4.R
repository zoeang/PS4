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
           chosenDoor= c(), #data for the items
           carDoor=c(),
           switch=c()
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
new("door", chosenDoor=2, carDoor=1, switch="A") #test the representation; this will throw an error

play1 <- new("door", chosenDoor=2, carDoor=3, switch=T) #test the validity; this will work

#Create new S4 Method ---------------------------------------------------------

#Create generic
setGeneric("PlayGame", function(x) { #set the name of the argument
  standardGeneric("PlayGame")
})


#New method

setMethod("PlayGame", "door",function(x){ #the x must be the same as the genernic
  #x is an object of class door
  #the indented things shouldn't be in the function, but seemed relevant at some point
        #car<-sample(1:3,1) #this will randomly assign a number to car
        #pick<-sample(1:3,1)
        # switchdoor<-sample(c(T,F),1)
       # trial<-new("door", chosenDoor=pick, carDoor=car, switch=switchdoor) #Assign the sample correctly
        # ^this should meet the "false" condition where pick is stored in the chosenDoor slot
  #The if loop will overwrite the chosenDoor slot
  if (x@switch==T){
    revealDoor<-c(1:3) #value of all doors
    revealDoor2 <- revealDoor[!revealDoor %in% x@carDoor] #Remove the door with the car
    revealDoor3 <- revealDoor2[!revealDoor2 %in% x@chosenDoor] #remove the door originally picked
    x@chosenDoor<-sample(revealDoor3,1)
  } 

  if (x@chosenDoor==x@carDoor){
    winner<-T
    print("Congratulations; you won a car!")
  } else {
    winner<-F
    print("Have a goat.")
    }
  return(winner)
}
)
debug(PlayGame)
undebug(PlayGame)

PlayGame(play1)


#Simulation----------------------------------------------------------------

#Write a function that does this 1000 times-----------------------
play3<-new("door")
PlayGame(play3)
#############
#For True
repeat.PlayGameT<-function(i){
  playing<-new("door", chosenDoor=sample(c(1,2,3),1), carDoor=sample(c(1,2,3), 1),switch=T)
  output<-PlayGame(playing)
  return(output)
}
final <- sapply(X=c(1:1000), FUN = repeat.PlayGameT)
sum(final)/1000

#For False
repeat.PlayGameF<-function(i){
  playing<-new("door", chosenDoor=sample(c(1,2,3),1), carDoor=sample(c(1,2,3), 1),switch=F)
  output<-PlayGame(playing)
  return(output)
}
final <- sapply(X=c(1:1000), FUN = repeat.PlayGameF)
sum(final)/1000

#-----------------------------------------------------------------




