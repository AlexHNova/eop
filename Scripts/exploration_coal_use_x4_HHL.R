# Date created: 12 November 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# Script to explore the coal use of each household using the houseHoldlists
# parameters to compare: coal-use log, coal weighing data, ibutton data, DES data
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
library(doBy)
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #

load(paste(rdadir, "houseHoldList.Rda", sep = ""))

temphh <- houseHoldList[[7]]

# get the date range for which we have coal-log data, coal-weighing data and ibutton data for the particular household
mindate <- max(c(min(temphh@coalLog$date, na.rm = TRUE), min(temphh@coalWeighingData$date, na.rm = TRUE), min(temphh@ibuttonData$date, na.rm = TRUE)))
maxdate <- min(c(max(temphh@coalLog$date, na.rm = TRUE), max(temphh@coalWeighingData$date, na.rm = TRUE), max(temphh@ibuttonData$date, na.rm = TRUE)))

# extract the weighing dates from the coal weighing data which fall within the above-calculated date range
weighingDates <- temphh@coalWeighingData[which((temphh@coalWeighingData$date >= mindate) & (temphh@coalWeighingData$date <= maxdate)), "date"]

# construct a data frame to hold the parametric data desired for comparison
df_coal_history <- data.frame(matrix(nrow = length(weighingDates), ncol = 7),stringsAsFactors = FALSE)
colnames(df_coal_history) <- c("date", "current_coal_weight", "units_bought_since_lw", "no_of_fires_made_ib", "no_of_fires_made_log", "total_wood_used_log", "total_coal_used_log")
df_coal_history$date <- weighingDates

# make sure the dates are ordered
weighingDates <- sort(x = weighingDates, decreasing = FALSE)

# populate the data frame



for (d in 2:length(weighingDates)) {
  
  # extract data from the coal weighing set
  df_coal_history[d, "current_coal_weight"] <- temphh@coalWeighingData[which(temphh@coalWeighingData$date == weighingDates[d]), "current_coal_weight"]
  df_coal_history[d, "units_bought_since_lw"] <- temphh@coalWeighingData[which(temphh@coalWeighingData$date == weighingDates[d]), "units_bought_since_last_weighing"]
  
  startdate <- weighingDates[d-1]
  enddate <- weighingDates[d]
  
  # extract data from the coal log set
  logExtract <- temphh@coalLog[which((temphh@coalLog$date > startdate) & (temphh@coalLog$date <= enddate)),]
  weightWoodBatch <- unique(logExtract$weight_of_wood_batch_kg)
  weightCoalContainer <- unique(logExtract$weight_of_a_filled_container_kg)
  logExtract <- logExtract[, c("no_of_wood_batches", "no_of_coal_containers", "new_or_refill")]
  logExtract$new_or_refill <- as.character(logExtract$new_or_refill)
  logExtract$new_or_refill <- ifelse(test = logExtract$new_or_refill == "new", 1, 0)
  logExtractSums <- colSums(x = logExtract, na.rm = TRUE)
  
  df_coal_history[d, "no_of_fires_made_log"] <- logExtractSums[["new_or_refill"]]
  df_coal_history[d, "total_wood_used_log"] <- logExtractSums[["no_of_wood_batches"]] * weightWoodBatch
  df_coal_history[d, "total_coal_used_log"] <- logExtractSums[["no_of_coal_containers"]] * weightCoalContainer
  
  rm(logExtract, logExtractSums, weightWoodBatch, weightCoalContainer)
  
  # extract data from the ibutton data set
  ibExtract <- temphh@ibuttonData[which((temphh@ibuttonData$date > startdate) & (temphh@ibuttonData$date <= enddate)),]
  fires <- table((rle(ibExtract$fire))$values)
  if ("yes" %in% names(fires)) {
    df_coal_history[d, "no_of_fires_made_ib"] <- fires[["yes"]]
  } else {
      df_coal_history[d, "no_of_fires_made_ib"] <- 0L
    }
}

df_coal_history$mo <- month(df_coal_history$date)

summaries <- summaryBy(formula = units_bought_since_lw + no_of_fires_made_ib + no_of_fires_made_log + total_wood_used_log + total_coal_used_log ~ mo, data = df_coal_history, FUN = sum)

summaries$des_summer_units_coal <- median(temphh@DESdata$energy_coal_consumption_summercurrentunits, na.rm = TRUE)
summaries$des_winter_units_coal <- median(temphh@DESdata$energy_coal_consumption_wintercurrentunits, na.rm = TRUE)
summaries$des_summer_units_wood <- median(temphh@DESdata$energy_wood_consumption_summercurrentunits, na.rm = TRUE)
summaries$des_winter_units_wood <- median(temphh@DESdata$energy_wood_consumption_wintercurrentunits, na.rm = TRUE)

# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##
