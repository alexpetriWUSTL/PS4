#PS4 
#Getting Started...

doorFunction <- function(doorChoice, doorCar){ #Function that takes in variables for door choice and door with car
  if (doorChoice == doorCar){ #logical to test if picks align
    win <- TRUE 
  } else {
    win <- FALSE
  }
  print(win) #print x to see if choices align
  return(c(doorChoice, doorCar)) #return door numbers to ensure logical works correctly
}
doorFunction(sample(1:3, 1), sample(1:3, 1)) #call function with the samples

#Moving on...
#1
setClass(Class = "door", #use setclass to establish class "door" with appropriate slots stated in problem set
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
setValidity("door", function(object){ #set the validity so that objects in class door do not return a value that is not their predefined value
  if (!is.integer(object@chosenDoor)){  
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

setMethod("initialize", "door", function(.Object, ...){ #initialize
  value = callNextMethod()
  validObject(value)
  return(value)
})


setGeneric("PlayGame", def = function(object){ #set my generic PlayGame function
  standardGeneric("PlayGame")
}
)

setMethod("PlayGame", signature("door"), function(object){ #set the specific method
  object@carDoor <- as.integer(sample(1:3, 1)) #a sample of 1-3 for where the car is located
  firstDoor <- as.integer(sample(1:3, 1)) #a sample of 1-3 where the door is chosen first
  if(object@switch == FALSE){ #when there is no swtich
    object@chosenDoor <- firstDoor #the chosen door is the first door
    if(identical(as.integer(object@chosenDoor), as.integer(object@carDoor))) { #if the user picks the car door, fills winner slot with appropriate boolean
      object@winner <- TRUE
    } else { #otherwise...
      object@winner <- FALSE
    }
  } else {
    openDoor <- c(as.integer(1:3)) 
    openDoor <- openDoor[-c(object@carDoor, firstDoor)] #the open door is the non-car door and not the first door
    if (length(openDoor) == 2){ #but if the first door was the car door
      openDoor <- sample(openDoor, 1) #we must take a sample because then the open door winds up being two doors
    }
    newDoorSelection <- c(as.integer(1:3))
    object@chosenDoor <- newDoorSelection[-c(firstDoor, openDoor)] #the candidate then must pick the new door
    if(identical(as.integer(object@chosenDoor), as.integer(object@carDoor))) { #if the user picks the car door, fill slot with appropriate boolean
      object@winner <- TRUE
    } else { #otherwise...
      object@winner <- FALSE
    }
  } 
}
)

doorNumNoSwitch <- new("door", carDoor = sample(1:3, 1), chosenDoor = sample(1:3, 1), switch = FALSE) #set up a new object for each switch possibility
doorNumSwitch <- new("door", carDoor = sample(1:3, 1), chosenDoor = sample(1:3, 1), switch = TRUE)

outcomeNoSwitch <- replicate(1000, PlayGame(doorNumNoSwitch), simplify = "array") #the outcomes for not switching
outcomeSwitch <- replicate(1000, PlayGame(doorNumSwitch), simplify = "array") #the outcomes for switching
head(outcomeNoSwitch) 
(length(outcomeNoSwitch[outcomeNoSwitch == TRUE])/1000)*100 #finds percentages
(length(outcomeSwitch[outcomeSwitch == TRUE])/1000)*100 #finds percentage

#usually not switching has a 33% chance of winning, but the better choice is to switch with 66% effectiveness



