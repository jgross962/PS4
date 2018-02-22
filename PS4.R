# Jonathan Gross
# Pol Sci 5625
#PS 4

## Geting Started
# Fix Code

myFunction = function(doorthing, doorthing2){

}

##


rm(list = ls())



## Part 1
# S4 Door Object

#Create door class, that accepts integer as input


setClass(
  Class = "door",
  representation = representation (
    chosenDoor = "integer",
    carDoor = "integer",
    switch = "logical"
  ),
  prototype = prototype(
    chosenDoor = c(),
    carDoor = c(),
    switch = NULL
  )
)
winner = NULL
win = NULL


#Check User has properly created door, by checking class of input

setValidity("door",function(object){
  if (class(object@chosenDoor) != "integer" | (!object@chosenDoor %in% as.integer(c(1,2,3))) ){
    if(class(object@carDoor) != "integer" | (!object@carDoor %in% as.integer(c(1,2,3))) ){
     return("@door is not a valid value")
    }
  }
})

#Check validity when a new door is created
setMethod("initialize","door", function(.Object,...){
  value = callNextMethod()
  validObject(value)
  return(value)
})

#Test - create a working door and a not working door

new("door",as.integer(2))
new("door",chosenDoor = as.integer(2))
goodDoor1 = new("door",chosenDoor = as.integer(2))
goodDoor1

goodDoor = new("door",chosenDoor = as.integer(2), switch = FALSE)


goodDoor = new("door",chosenDoor = as.integer(2), carDoor = as.integer(1), switch = FALSE)

## Part 2

# Create Generic Function Play Game
setGeneric("PlayGame",
           function(object = "door"){
             standardGeneric("PlayGame")
           })
winner= logical()
#Create Method for PlayGame for Door Objecct
setMethod("PlayGame","door",
          function(object){
            #Randomly Choose (and overwrite) winning Door #
            object@carDoor = sample(3,1)
            #Randomly pick inital choosen door
            InitialChosenDoor =sample(3,1)

            if (object@switch==FALSE){
              #If switch is false, final chosen door is same as initial chosen door
              object@chosenDoor = InitialChosenDoor
            }else{
              # If switch true, remove a door that is not the car door
              RemoveDoorChoices = (c(1,2,3)[-object@carDoor][-InitialChosenDoor])
              RemoveDoor = RemoveDoorChoices[sample(length(RemoveDoorChoices),1)]
              AfterSwitchDoorChoices = c(1,2,3)[-RemoveDoor]
              object@chosenDoor = AfterSwitchDoorChoices[sample(length(AfterSwitchDoorChoices),1)]
            }
            if (object@chosenDoor == object@carDoor){
              winner<<- TRUE
              return(TRUE)
            } else{
              winner <<- FALSE
              return(FALSE)
            }

          })

PlayGame(goodDoor)








