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

#Create door class, that accepts integer inputs for doors and logical for switch
# @choosenDoor = door that the player picks
# @carDorr = the winning door
# @switch = boolean to determine whether or not a player switches doors
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

# Create empty winner Logical-- will be overwritten after playing game
winner= logical()


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
goodDoor = new("door",chosenDoor = as.integer(2), carDoor = as.integer(1), switch = FALSE)


## Part 2 -- Play Game Function

# Create Generic Function Play Game
setGeneric("PlayGame",
           function(object = "door"){
             standardGeneric("PlayGame")
           })


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
              # If switch true, remove a door that is not the car door or Iniitally Chosen Door
              RemoveDoorChoices = c(1,2,3)[-which(object@carDoor==c(1,2,3))]
              RemoveDoorChoices  =RemoveDoorChoices[-which(InitialChosenDoor==RemoveDoorChoices)]
              RemoveDoor = RemoveDoorChoices[sample(length(RemoveDoorChoices),1)]

              #Determine Which Doors are Remaining after removing one
              AfterSwitchDoorChoices = c(1,2,3)[-which(RemoveDoor == c(1,2,3))]
              # Determine final chosen door, by randomly choosing between remaining choices
              object@chosenDoor = AfterSwitchDoorChoices[sample(length(AfterSwitchDoorChoices),1)]
            }



            # Determine if Player won
            if (object@chosenDoor == object@carDoor){
              winner<<- TRUE
              #return(TRUE)
            } else{
              winner <<- FALSE
              #return(FALSE)
            }

          })

#Test Method PlayGame
PlayGame(goodDoor)
PlayGame(goodDoor)
PlayGame(goodDoor)


## Part 3 Simulation
# Simulate with Switch = True

debug(PlayGame)
