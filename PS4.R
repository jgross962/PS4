# Jonathan Gross
# Pol Sci 5625
#PS 4

rm(list = ls())



# S4 Methodology

#Create door class, that accepts numeric as input
setClass(
  Class = "Door",
  representation = representation (
    DoorNumber = "numeric"
  ),
  prototype = prototype(
    DoorNumber = c()
    
  )
)
#Check User has properly created door, by checking class of input
setValidity("Door",function(object){
  if (class(object@DoorNumber) != "numeric"){
    return("@Door is not a valid value")
  }
})

#Check validity when a new door is created
setMethod("initialize","Door", function(.Object,...){
  value = callNextMethod()
  validObject(value)
  return(value)
})

#Test - create a working door and a not working door
goodDoor = new("Door",DoorNumber = 2)
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
