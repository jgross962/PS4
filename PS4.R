# Jonathan Gross
# Pol Sci 5625
#PS 4

rm(list = ls())

validDoors = as.integer(c(1,2,3))


# S4 Methodology

#Create door class, that accepts numeric as input
setClass(
  Class = "door",
  representation = representation (
    chosenDoor = "integer",
    carDoor = "integer",
    switch = "logical",
    winner = "logical"
  ),
  prototype = prototype(
    chosenDoor = c(),
    carDoor = as.integer(floor(runif(1,1,4))),
    switch = FALSE,
    winner = FALSE
    
  )
)
#Check User has properly created door, by checking class of input
setValidity("door",function(object){
  if (class(object@chosenDoor) != "integer" | (!x %in% validDoors) ){
    return("@door is not a valid value")
  }
})

#Check validity when a new door is created
setMethod("initialize","door", function(.Object,...){
  value = callNextMethod()
  validObject(value)
  return(value)
})

#Test - create a working door and a not working door

goodDoor1 = new("door",chosenDoor = as.integer(2))
goodDoor1

goodDoor = new("door",chosenDoor = as.integer(2), switch = FALSE)


badDoor = new("Door", DoorNumber = "test")

















#Create Generic PlayGame Function that take's users door as input
setGeneric("PlayGame", 
           function(object = "Door"){
             standardGeneric("PlayGame")
           })

#Create plyGame method -- randomly picks value 1 2 or 3 and compares against user value
setMethod("PlayGame","Door",
          function(object){
            #Create the winning door with random value
            WinningDoor = new("Door",DoorNumber = floor(runif(1,1,4)))
            #Print Appropriate Output
            if(WinningDoor@DoorNumber == object@DoorNumber){
              return("Congratulations You've Won A Car")
            }else{
              return("I'm Sorry You Didn't Win")
            }
          })

#Test PlayGame Method multiple times to ensure you win sometimes
PlayGame(goodDoor)
PlayGame(goodDoor)
PlayGame(goodDoor)
PlayGame(goodDoor)
PlayGame(goodDoor)
