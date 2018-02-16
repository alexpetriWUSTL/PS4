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
           winner = c()
         )
)

#2
setValidity("door", function(object){ #set the validity so that objects in class door that are integer will return a fail statement
  if(!is.integer(object@chosenDoor)){
    return("The object is not of type integer")
  }
  if(!is.integer(object@carDoor)){
    return("The object is not of type integer")
  }
  if(!is.logical(object@switch)){
    return("The object is not of type logical")
  }
  if(!is.logical(object@winner)){
    return("The object is not of type logical")
  }
  if(object@chosenDoor < 1 | object@chosenDoor > 3){
    return("You must choose a door number between 1 and 3")
  }
  if(object@carDoor < 1 | object@carDoor > 3){
    return("The car must be hidden in a door numbered between 1 and 3")
  }
}
)

test()







