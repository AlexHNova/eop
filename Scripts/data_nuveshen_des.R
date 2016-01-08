# Date created: 19 Nov 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# Script to prepare the DES data for Nuveshan as requested by e-mail on 18 Nov 2015
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# declare script constants
VERBOSE <- FALSE
DEBUG <- TRUE
WOOD_UNIT_WEIGHT <- 35
N_WEEKS_WINTER <- 17
N_WEEKS_SUMMER <- 35
N_MONTHS_WINTER <- 4
N_MONTHS_SUMMER <- 8
# ---------------------------------------- #
# ---------------------------------------- #

# load the data
load(paste(rdadir, "houseHoldList.Rda", sep = ""))

statFrame <- data.frame(matrix(nrow = 3, ncol = 4, data = 0), stringsAsFactors = FALSE)
rownames(statFrame) <- c("none", "basic", "full")
colnames(statFrame) <- c("control", "coal", "lpg", "elec")

calculateAnnualAvg <- function(carrier = NULL) {
  statFrame_n_info <- statFrame
  statFrame_n_uses <- statFrame
  statFrame_total <- statFrame
  
  useVar <- switch(carrier,
                   "coal" = "energy_coal_use",
                   "lpg" = "energy_lpg_use", 
                   "wood" = "energy_wood_use",
                   "dung" = "energy_dung_use",
                   "elec" = "energy_electricity_use",
                   "paraffin" = "energy_paraffin_use") 
  
  for (hhi in 1:length(houseHoldList)) {
    hh <- houseHoldList[[hhi]]
    
    # check if DES data is available for the particular hh and if so, extract the (appropriate) DES df from the hh object
    isNull <- sapply(X = hh@DESdata$data_df, FUN = is.null)
    if (all(isNull)) {
      if (VERBOSE) {warning("No DES data found for stand ", hh@address@standNumber, ".")}
      next
    }
  
    statFrame_n_info[hh@insulationType,hh@energyType] <- statFrame_n_info[hh@insulationType,hh@energyType] + 1
    
    idxx <- which(!isNull)  
    if (length(idxx) > 1) {
      if (hh@energyType == "control") { dfDes <- hh@DESdata$data_df[[1]] } else { dfDes <- hh@DESdata$data_df[[2]] }
    } else { dfDes <- hh@DESdata$data_df[[idxx]] }
    
    # determine whether the hh uses the energy carrier in question
    if (is.na(dfDes[1, useVar])) {
      message("Uh-oh. NA detected in field '", useVar, "' for stand ", hh@address@standNumber, ".")
      next
    }
    if (dfDes[1, useVar] == "no") { next }
    
    # if they do, increment the total consumption for the group with the current hh's consumption
    statFrame_n_uses[hh@insulationType, hh@energyType] <- statFrame_n_uses[hh@insulationType, hh@energyType] + 1
    
    # determine consumption variable for summer season
    summerVar <- switch(carrier,
                   "coal" = "energy_coal_consumption_summercurrentunits",
                   "lpg" = "energy_lpg_comsumption_kgsummermonth", 
                   "wood" = "energy_wood_consumption_summercurrentunits",
                   "dung" = "energy_dung_volumne",
                   "elec" = "energy_cost_summer_electricity",
                   "paraffin" = "energy_paraffin_comsumption_litressummerweek")
    
    # multiplication factor for summer values
    summerMF <- switch(carrier,
                   "coal" = N_MONTHS_SUMMER * ifelse(dfDes$energy_coal_format == "small_bag_about_25_kg", 
                                                     25, 
                                                     ifelse(dfDes$energy_coal_format == "tin", 
                                                            17.5,
                                                            50)),
                   "lpg" = N_MONTHS_SUMMER, 
                   "wood" = N_MONTHS_SUMMER * WOOD_UNIT_WEIGHT,
                   "dung" = 8,
                   "elec" = N_MONTHS_SUMMER,
                   "paraffin" = N_WEEKS_SUMMER)
    
    # determine consumption variable for winter season
    winterVar <- switch(carrier,
                   "coal" = "energy_coal_consumption_wintercurrentunits",
                   "lpg" = "energy_lpg_comsumption_kgwintermonth", 
                   "wood" = "energy_wood_consumption_wintercurrentunits",
                   "dung" = "energy_dung_volumne",
                   "elec" = "energy_cost_winter_electricity",
                   "paraffin" = "energy_paraffin_comsumption_litreswinterweek")
    
    # multiplication factor for winter values
    winterMF <- switch(carrier,
                   "coal" = N_MONTHS_WINTER * ifelse(dfDes$energy_coal_format == "small_bag_about_25_kg", 
                                                     25, 
                                                     ifelse(dfDes$energy_coal_format == "tin", 
                                                            17.5,
                                                            50)),
                   "lpg" = N_MONTHS_WINTER, 
                   "wood" = N_MONTHS_WINTER * WOOD_UNIT_WEIGHT,
                   "dung" = 4,
                   "elec" = N_MONTHS_WINTER,
                   "paraffin" = N_WEEKS_WINTER)
    
    dfDes[[winterVar]] <- as.numeric(dfDes[[winterVar]])
    dfDes[[summerVar]] <- as.numeric(dfDes[[summerVar]])
    
    winterConsumption <- NA
    if (!is.na(dfDes[[winterVar]])) {winterConsumption <- dfDes[[winterVar]] * winterMF}
    summerConsumption <- NA
    if (!is.na(dfDes[[summerVar]])) {summerConsumption <- dfDes[[summerVar]] * summerMF}
    
    statFrame_total[hh@insulationType, hh@energyType] <- sum(statFrame_total[hh@insulationType, hh@energyType],
                                                           winterConsumption,
                                                           summerConsumption,
                                                           na.rm = TRUE)
    
    
    rm(hh)
  }
  
  statFrame_groupAvg <- statFrame_total / statFrame_n_info
  statFrame_avg <- statFrame_total / statFrame_n_uses
  rm(statFrame_total)
  
  print("n:")
  assign(x = "statFrame_n_info", value = statFrame_n_info, envir = .GlobalEnv)
  print(statFrame_n_info)
  
  print(" ", quote = FALSE)
  print("n_uses:")
  assign(x = "statFrame_n_uses", value = statFrame_n_uses, envir = .GlobalEnv)
  print(statFrame_n_uses)
  
  print(" ", quote = FALSE)
  print("avgs:")
  assign(x = "statFrame_avg", value = statFrame_avg, envir = .GlobalEnv)
  print(statFrame_avg)
  
  print(" ", quote = FALSE)
  print("group avgs:")
  assign(x = "statFrame_groupAvg", value = statFrame_groupAvg, envir = .GlobalEnv)
  print(statFrame_groupAvg)
}

# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##
