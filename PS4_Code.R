#PS4 
#Getting Started...

myfunction <- function(doorChoice, doorCar){ #Function that takes in variables for door choice and door with car
  if (doorChoice == doorCar){ #logical to test if picks align
    x <- TRUE 
  } else {
    x <- FALSE
  }
  print(x) #print x to see if choices align
  return(c(doorChoice, doorCar)) #return door numbers to ensure logical works correctly
}
myfunction(sample(1:3, 1), sample(1:3, 1)) #call function with the samples

#Moving on...
#1
setClass(Class = "door", #use setclass to establish class "Door" with a numeric vector x as an available slot
         representation = representation(
           chosenDoor = "integer",
           carDoor = "integer",
           switch = "logical",
           winner = "logical"
         ),
         prototype = prototype(
           chosenDoor = c(),
           carDoor = c(),
           switch = c(),
           winner = c(NA)
         )
)

#2
setValidity("door", function(object){ #set the validity so that objects in class door that are integer will return a fail statement
  if(!is.integer(object@chosenDoor)){
    return("The object is not of type integer")
  } else if(!is.integer(object@carDoor)){
    return("The object is not of type integer")
  } else if(!is.logical(object@switch)){
    return("The object is not of type logical")
  } else if(!is.logical(object@winner)){
    return("The object is not of type logical")
  } else if(object@chosenDoor < 1 | object@chosenDoor > 3){
    return("You must choose a door number between 1 and 3")
  } else if(object@carDoor < 1 | object@carDoor > 3){
    return("The car must be hidden in a door numbered between 1 and 3")
  }
}
)


setGeneric("PlayGame", def = function(object){ #set my generic PlayGame function
  standardGeneric("PlayGame")
}
)

setMethod("PlayGame", signature("door"), function(object){ #set the specific method
  carDoor <- as.integer(sample(1:3, 1)) #a sample of 1-3 for where the car is located
  firstDoor <- as.integer(sample(1:3, 1))
  if(switch == FALSE){
    chosenDoor <- firstDoor
  } else {
    openDoor <- c(as.integer(1:3))
    openDoor <- openDoor[-c(carDoor, firstDoor)]
    newDoorSelection <- c(as.integer(1:3))
    chosenDoor <- newDoorSelection[-c(firstDoor, openDoor)]
  } 
  if(identical(as.integer(object@chosenDoor), as.integer(object@carDoor))) { #if the user picks the car door, they return a congrats statement
    print("Congratulations, you have chosen the correct door!")
    print(car)
  } else { #otherwise...
    print("Sorry, you have chosen the incorrect door")
    print(car)
  }
}
)


carDoor <- sample(1:3, 1) #a sample of 1-3 for where the car is located
firstDoor <- sample(1:3, 1)
carDoor
firstDoor
openDoor <- c(1:3)
openDoor <- openDoor[-c(carDoor, firstDoor)]







