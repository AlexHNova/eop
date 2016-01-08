
# source("C:/Users/Alex H/Dropbox (Nova SA)/Rtemplates/objects.R")

setClass(Class = "standObj", 
         slots = list( standNumber = "character",
                       streetNumber = "character",
                       streetName = "character",
                       extension = "character",
                       subPlace = "character",
                       mainPlace = "character",
                       municipality = "character",
                       latitude = "numeric",
                       longitude = "numeric"))

setClass(Class = "houseHoldObj",
         slots = list( address = "standObj",
                       hhSurname = "character",
                       hhMainMember = "character",
                       contactNumber = "character"))

setClass(Class = "EOPhouseHold", 
         contains = "houseHoldObj", 
         slots = list(energyType = "character",
                      insulationType = "character",
                      elecMeterNumber = "ANY",
                      elecSupplier = "character",
                      stoveNumber = "integer",
                      elecData = "ANY",
                      coalLog = "ANY",
                      coalWeighingData = "ANY",
                      ibuttonData = "ANY",
                      DESdata = "ANY"))


new.EOPhouseHold <- function() {
  temp <- new(Class = "EOPhouseHold")
  
  temp@elecData <- list(meta_info = NULL, data_df = NULL)
  temp@coalLog = list(meta_info = NULL, data_df = NULL)
  temp@coalWeighingData = list(meta_info = NULL, data_df = NULL)
  temp@ibuttonData = list(meta_info = NULL, data_df = NULL)
  temp@DESdata = list(meta_info = NULL, data_df = NULL)
  
  return(temp)
}



