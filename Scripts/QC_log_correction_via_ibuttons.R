# Date created: 25 November 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# Script to correct the coal use logs in light of the ibuttons, and vice versa
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# source additional, locally required functions and object definitions
source(paste(novafunctdir, "wees.vure.R", sep = ""))
source(paste(novafunctdir, "heg_vure.R", sep = ""))
source(paste(scriptdir, "EOPhhObj.R", sep = ""))
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
IB_INTERVAL <- 20 # in minutes
# ---------------------------------------- #
# define auxiliary functions
matchIbuttonAndLog <- function(hh = NULL) {
  
  if (is.null(hh)) {
    return(NULL)
  }
  if (is.null(hh@coalLog$data_df)) {
    warning("No coal-use log data found for stand ", hh@address@standNumber, ".")
    return(hh)
  }
  if (is.null(hh@ibuttonData$data_df)) {
    warning("No ibutton data found for stand ", hh@address@standNumber, ".")
    return(hh)
  }

  # get the coal log and ibutton data for the hh and test for emptiness
  dfCoalLog <- hh@coalLog$data_df
  if (nrow(dfCoalLog) < 1) {
    warning("No coal-use log data found for stand ", hh@address@standNumber, ".")
    return(hh)
  }
  dfIbutton <- hh@ibuttonData$data_df
  if (nrow(dfIbutton) < 1) {
    warning("No ibutton data found for stand ", hh@address@standNumber, ".")
    return(hh)
  }
  
  # determine the date range where the coal-use log data and the ibutton-data overlap
  mindate <- max(min(dfCoalLog$date, na.rm = TRUE), min(dfIbutton$date, na.rm = TRUE))
  maxdate <- min(max(dfCoalLog$date, na.rm = TRUE), max(dfIbutton$date, na.rm = TRUE))
  dfCoalLog <- dfCoalLog[which(between(x = dfCoalLog$date, left = mindate, right = maxdate)),]
  dfIbutton <- dfIbutton[which(between(x = dfIbutton$date, left = mindate, right = maxdate)),]

  # remove the records from the coal logs where the ibuttons 
  # were not operating due to reading periods
  days <- unique(yday(dfIbutton$date))
  dfCoalLog <- dfCoalLog[which(yday(dfCoalLog$date) %in% days),]
  
  # remove incomplete days (i.e. days for which we do not have at least 23 hours of data)
  ctch <- apply(X = as.array(days), MARGIN = 1, FUN = function(d) {
    idxx <- which(yday(dfIbutton$date) == d)
    if (length(idxx) < (23*60 / IB_INTERVAL)) {
      dfIbutton <<- dfIbutton[-idxx,]
      dfCoalLog <<- dfCoalLog[which(yday(dfCoalLog$date) != d),]
    }
    rm(idxx)
  })
  rm(days, ctch)
  
  # check for emptiness
  if (nrow(dfCoalLog) < 1 | nrow(dfIbutton) < 1) {
    warning("No overlapping date range found for stand ", hh@address@standNumber, ".")
    return(hh)
  }
  
  # ensure that both data frames are sorted chronologically
  dfCoalLog <- dfCoalLog[order(as.integer(dfCoalLog$date)),]
  rownames(dfCoalLog) <- c(1:nrow(dfCoalLog))
  dfIbutton <- dfIbutton[order(as.integer(dfIbutton$date)),]
  rownames(dfIbutton) <- c(1:nrow(dfIbutton))
  
  # join/remove orphan "fires"
  daysplits <- split(x = dfIbutton, f = yday(dfIbutton$date))
  daysplits <- lapply(X = daysplits, FUN = function(ddf) {
    
    ddf <- heg_vure(df = ddf, 
                    fireField = "fire", 
                    fireLabel = "yes", 
                    noFireLabel = "no", 
                    minGapLength = ceiling(60 / IB_INTERVAL))
    
    ddf <- wees_vure(df = ddf, 
                     fireField = "fire", 
                     fireLabel = "yes", 
                     noFireLable = "no", 
                     minFireLength = ceiling(2.5*60 / IB_INTERVAL))
    
    return(ddf)
  })
  dfIbutton <- do.call("rbind", daysplits)
  rm(daysplits)
  
  # first check if any ibutton fires were recorded for the hh; 
  # if not, no correction of the log can occur. 
  # Check if correction of the ibutton data is possible; 
  # if it is, attempt that, and then exist this function (returning)
  if (!("yes" %in% dfIbutton$fire)) {
    message("No ibutton fires recorded for stand ", hh@address@standNumber, "during the coal log period.")
    dfIbutton <- correctIbutton(dfib = dfIbutton, dfcl = dfCoalLog)
    hh@ibuttonData$data_df <- dfIbutton
    hh@coalLog$data_df <- dfCoalLog
    return(hh)
  }
  
  # add an ignition field to the ibutton data, 
  # marking the first record of each fire episode as the ignition record
  dfIbutton$ignition <- NA_character_
  nextYesIgnition <- TRUE
  
  for (r in 1:nrow(dfIbutton)) {
    if (is.na(dfIbutton[r, "fire"])) {next}
    if ((dfIbutton[r, "fire"] == "yes") & (nextYesIgnition)) {
      dfIbutton[r, "ignition"] <- "yes"
      nextYesIgnition <- FALSE
      next
    }
    if (dfIbutton[r, "fire"] == "no") {nextYesIgnition <- TRUE}
  }
  
  runLengths <- rle(dfIbutton$fire)
  runLengths <- data.frame(lngths = runLengths$lengths, 
                           vls = runLengths$values, stringsAsFactors = FALSE)
  runLengths <- runLengths[which(runLengths$vls == "yes"),] 

  # subset dfIbutton to only contain the records that mark the moment of 
  # ignition and then add the fire duration (run length) field to dfIbutton
  ignitionIdxx <- which(dfIbutton$ignition == "yes")
  dfIbutton_removedRows <- dfIbutton[-ignitionIdxx,]
  dfIbutton <- dfIbutton[ignitionIdxx,]
  if (nrow(dfIbutton) < 1) {
    warning("No ignitions found in the overlapping period for stand ", hh@address@standNumber)
    return(hh)
  }
  if (nrow(runLengths) != nrow(dfIbutton)) {
    stop("nrow(runLengths) [", nrow(runLengths), "] != nrow(dfIbutton)[", nrow(dfIbutton), "]")
  }
  dfIbutton$run_length <- runLengths$lngths
  rm(runLengths)
  
  # now check if each ibutton fire has a record in the coal use logs
  ## add an 'ib_assigned' field to the log to indicate its status
  ## add a 'log_fire_type' field to the ibutton to indicate whether this ibutton fire episode 
  # corresponds to a "new fire" in the log or to a "refill fire" or to an "ibutton added fire"
  dfCoalLog$ib_assigned <- FALSE
  dfCoalLog$ib_ignition_time <- NA
  dfIbutton$log_fire_type <- NA_character_
  dfIbutton$log_ignition_time <- NA
  
  for (r in 1:nrow(dfIbutton)) {
    
    # find possible coal log matches for the given ibutton ignition
    idxx <- which((dfCoalLog$date >= (dfIbutton[r, "date"] - dseconds(120*60))) & 
                    (dfCoalLog$date <= (dfIbutton[r, "date"] + dseconds(120*60))) & 
                    (dfCoalLog$ib_assigned == FALSE))
    
    # if exactly one record was found in the coal log, 
    # assign the coal log and ibutton ignitions to each other and move on
    if (length(idxx) == 1) {
      dfCoalLog[idxx, "ib_assigned"] <- TRUE
      dfCoalLog[idxx, "ib_ignition_time"] <- as.POSIXct(x = dfIbutton[r, "date"], 
                                                        tz = TIME_ZONE, 
                                                        origin = "1970-01-01 00:00:00")
      dfIbutton[r, "log_fire_type"] <- dfCoalLog[idxx, "new_or_refill"]
      dfIbutton[r, "log_ignition_time"] <- as.POSIXct(x = dfCoalLog[idxx, "date"], 
                                                      tz = TIME_ZONE, 
                                                      origin = "1970-01-01 00:00:00")
      next
    }
    
    # if more than one unassigned record were found in the log corresponding to the time period criteria, 
    # assign the most suitable one
    if (length(idxx) > 1) {
      ## (1) determine the absolute difference between the datetime of each of the 
      # potentially suitable log records and the datetime of the ibutton ignition
      timeDiffs <- apply(X = as.array(idxx), MARGIN = 1, FUN = function(ridx) {
        diff <- dfIbutton[r, "date"] - dfCoalLog[ridx, "date"]
        if (diff < 0) {diff <- diff * (-1)}
        return(diff)
      })
      
      ## (2) choose the one which has the smallest absolute difference between it and the datetime of the 
      ## ibutton ignition, then move on.
      idx <- which(timeDiffs == min(timeDiffs, na.rm = TRUE))
      if (length(idx) > 1) {idx <- idx[1]}
      dfCoalLog[idxx[idx], "ib_assigned"] <- TRUE
      dfCoalLog[idxx[idx], "ib_ignition_time"] <- as.POSIXct(x = dfIbutton[r, "date"], 
                                                        tz = TIME_ZONE, 
                                                        origin = "1970-01-01 00:00:00")
      dfIbutton[r, "log_fire_type"] <- dfCoalLog[idxx[idx], "new_or_refill"]
      dfIbutton[r, "log_ignition_time"] <- as.POSIXct(x = dfCoalLog[idxx[idx], "date"], 
                                                      tz = TIME_ZONE,
                                                      origin = "1970-01-01 00:00:00")
      next
    }
 
    # if no records were found, label the ib fire as "unmatched" and move on
    if (length(idxx) < 1) {
      dfIbutton[r, "log_fire_type"] <-  "unmatched"
    }

  }
  
  # just fix the formatting of some of the added fields
  dfIbutton$log_ignition_time <- as.POSIXct(x = dfIbutton$log_ignition_time, 
                                            tz = TIME_ZONE, 
                                            origin = "1970-01-01 00:00:00")
  dfCoalLog$ib_ignition_time <- as.POSIXct(x = dfCoalLog$ib_ignition_time, 
                                            tz = TIME_ZONE, 
                                            origin = "1970-01-01 00:00:00")
  
  # assign the corrected coal log back to the hh object (just sort it chronologically first)
  dfCoalLog <- dfCoalLog[order(dfCoalLog$date),]
  rownames(dfCoalLog) <- c(1:nrow(dfCoalLog))
  hh@coalLog$data_df <- dfCoalLog
  
  # for sake of interest, show the number of unassigned fires in the log
  message("Assignment status of log fires for stand ", hh@address@standNumber, ": ")
  print(table(dfCoalLog$ib_assigned, exclude = NULL))

  # put the ibutton data frame back together again and assign it back to the hh object
  dfIbutton <- rbind.fill(dfIbutton, dfIbutton_removedRows)
  rm(dfIbutton_removedRows)
  dfIbutton <- dfIbutton[order(dfIbutton$date),]
  rownames(dfIbutton) <- c(1:nrow(dfIbutton))
  hh@ibuttonData$data_df <- dfIbutton

  rm(dfCoalLog, dfIbutton)
  
  return(hh)
}

# @dfib ibutton data frame
# @dfcl coal log data frame
correctIbutton <- function(dfib = NULL, dfcl = NULL) {
  return(dfib)
}

sketchIbVsLog <- function(hh = NULL, day = NULL) {
  
  dfIbutton <- hh@ibuttonData$data_df
  dfCoalLog <- hh@coalLog$data_df
  
#   dfIbutton$ignition <- gsub(pattern = "yes", replacement = 1, x = dfIbutton$ignition)
#   dfIbutton$ignition <- as.numeric(dfIbutton$ignition) * 30
  dfIbutton$hod <- hour(dfIbutton$date)
  dfCoalLog$hod <- hour(dfCoalLog$date)
  dfCoalLog$var <- ifelse(dfCoalLog$new_or_refill == "new", 27.5, 22)
  
  ssib <- dfIbutton[which(yday(dfIbutton$date) == day),]
  sscl <- dfCoalLog[which(yday(dfCoalLog$date) == day),]
  if ((nrow(ssib) < 1) | (nrow(sscl) < 1)) {return(-1)}
    
  plt <- ggplot(data = ssib, mapping = aes(x = hod, y = c, colour = log_fire_type)) + 
    geom_line(mapping = aes(x = hod, y = w)) +
    ylim(0,45) +
    xlim(0,23) +
    geom_point(data = sscl, mapping = aes(x = hod, y = var, colour = new_or_refill)) +
    ggtitle(paste("Day: ", day, ". Stand: ", hh@address@standNumber, sep = ""))
  
  print(plt)
  return(0)
}

# does hh-specific fine-tuning
hhSpesPrep <- function(hhList = NULL) {
  trainingHhs <- hhList
  rm(hhList)
  
  ## for hh @ stand 3152
  hh3152 <- trainingHhs[["3152"]]
  dfCoalLog <- hh3152@coalLog$data_df
  idxx <- which(yday(dfCoalLog$date) == 236)
  if (length(idxx) > 1) {
    dfCoalLog <- dfCoalLog[-idxx[1],]
  }
  idxx <- which(yday(dfCoalLog$date) == 222)
  dfCoalLog[idxx[2], "new_or_refill"] <- "refill"
  hh3152@coalLog$data_df <- dfCoalLog
  trainingHhs[["3152"]] <- hh3152
  #sketchIbVsLog(hh = trainingHhs[["3152"]], day = 222)
  rm(hh3152)
  
  ## for hh @ stand 3313
  hh3313 <- trainingHhs[["3313"]]
  dfCoalLog <- hh3313@coalLog$data_df
  dfIbutton <- hh3313@ibuttonData$data_df
  # idxx <- which(yday(dfCoalLog$date) == 181)
  # sketchIbVsLog(hh = hh3313, day = 181)
  dfCoalLog <- dfCoalLog[which(yday(dfCoalLog$date) != 181),]
  dfIbutton <- dfIbutton[which(yday(dfIbutton$date) != 181),]
  idxx <- which(yday(dfCoalLog$date) == 183)
  # sketchIbVsLog(hh = hh3313, day = 183)
  dfCoalLog <- dfCoalLog[-idxx[1],]
  idxx <- which(yday(dfCoalLog$date) == 227)
  # sketchIbVsLog(hh = hh3313, day = 227)
  dfCoalLog <- dfCoalLog[-idxx[1],]
  idxx <- which(yday(dfCoalLog$date) == 238)
  # sketchIbVsLog(hh = hh3313, day = 238)
  dfCoalLog <- dfCoalLog[-idxx[1],]
  # sketchIbVsLog(hh = hh3313, day = 182)
  dfCoalLog <- dfCoalLog[which(yday(dfCoalLog$date) != 182),]
  dfIbutton <- dfIbutton[which(yday(dfIbutton$date) != 182),]
  hh3313@ibuttonData$data_df <- dfIbutton
  hh3313@coalLog$data_df <- dfCoalLog
  trainingHhs[["3313"]] <- hh3313
  rm(hh3313)
  
  ## for hh @ stand 178
  hh178 <- trainingHhs[["178"]]
  # sketchIbVsLog(hh = hh178, day = 180)
  dfCoalLog <- hh178@coalLog$data_df
  idxx <- which(yday(dfCoalLog$date) == 180)
  if (length(dfCoalLog) > 1) {
    dfCoalLog <- dfCoalLog[-idxx[1],]
  }
  # sketchIbVsLog(hh = hh178, day = 220)
  idxx <- which(yday(dfCoalLog$date) == 220)
  if (length(idxx) == 3) {
    dfCoalLog <- dfCoalLog[-idxx[3],]
  }
  idx <- which(yday(dfCoalLog$date) == 174)
  if (length(idx) == 1) {
    newRow <- dfCoalLog[idx, ]
    newRow$date <- as.POSIXct(x = "2015-06-23 15:30:00", 
                              tz = TIME_ZONE, 
                              origin = "1970-01-01 00:00:00")
    newRow$no_of_wood_batches <- 1
    newRow$no_of_coal_containers <- 1
    newRow$new_or_refill <- "refill"
    dfCoalLog <- rbind(dfCoalLog, newRow)
    dfCoalLog <- dfCoalLog[order(dfCoalLog$date),]
    rownames(dfCoalLog) <- c(1:nrow(dfCoalLog))
  }
  idxx <- which(yday(dfCoalLog$date) == 232)
  # sketchIbVsLog(hh = hh178, day = 232)
  if (length(idxx) == 3) {
    dte <- dfCoalLog[idxx[2], "date"]
    dfCoalLog[idxx[2],] <- dfCoalLog[idxx[1],]
    dfCoalLog[idxx[2], "date"] <- dte
    rm(dte)
    dfCoalLog <- dfCoalLog[-c(idxx[1], idxx[3]),]
  }
  idxx <- which(yday(dfCoalLog$date) == 231) 
  # sketchIbVsLog(hh = hh178, day = 231)
  if (length(idxx) == 2) {
    dfCoalLog <- dfCoalLog[-idxx[2],]
  }
  idxx <- which(yday(dfCoalLog$date) == 194)
  # sketchIbVsLog(hh = hh178, day = 194)
  if (length(idxx) == 2) {
    dfCoalLog[idxx[2], "new_or_refill"] <- "new"
    dfCoalLog <- dfCoalLog[-idxx[1],]
  }
  hh178@coalLog$data_df <- dfCoalLog
  trainingHhs[["178"]] <- hh178
  rm(hh178)
  
  ## for hh @ stand 434
  hh434 <- trainingHhs[["434"]]
  dfCoalLog <- hh434@coalLog$data_df
  # sketchIbVsLog(hh = hh434, day = 194)
  idxx <- which(yday(dfCoalLog$date) == 194)
  if (length(idxx) > 1) {
    dfCoalLog <- dfCoalLog[-idxx[1],]
  }
  idxx <- which(yday(dfCoalLog$date) == 192)
  if (length(idxx) == 2) {
    dfCoalLog <- dfCoalLog[-idxx[2],]
  }
  hh434@coalLog$data_df <- dfCoalLog
  trainingHhs[["434"]] <- hh434
  rm(hh434)
  
  ## for hh @ stand 383
  hh383 <- trainingHhs[["383"]]
  dfCoalLog <- hh383@coalLog$data_df
  idxx <- which(yday(dfCoalLog$date) == 188)
  if (length(idxx) == 3) {
    dfCoalLog[idxx[3], "new_or_refill"] <- "refill"
  }
  hh383@coalLog$data_df <- dfCoalLog
  trainingHhs[["383"]] <- hh383
  rm(hh383, dfIbutton, dfCoalLog, idxx)
  
  ## for hh @ stand 3791
  hh3791 <- trainingHhs[["3791"]]
  dfIbutton <- hh3791@ibuttonData$data_df
  dfCoalLog <- hh3791@coalLog$data_df
  idxx <- which(yday(dfCoalLog$date) == 275)
  if (length(idxx) > 0) {
    dfCoalLog <- dfCoalLog[-idxx,]
  }
  idxx <- which(yday(dfIbutton$date) == 275)
  if (length(idxx) > 0) {
    dfIbutton <- dfIbutton[-idxx,]
  }
  sweepDays <- c(276:280)
  ctch <- apply(X = as.array(sweepDays), MARGIN = 1, FUN = function(d) {
    dtetme <- dfCoalLog[which(yday(dfCoalLog$date) == d), "date"]
    dfIbutton[which(yday(dfIbutton$date) == d & dfIbutton$date < dtetme), "fire"] <<- "no"
  })
  rm(ctch, sweepDays)
  hh3791@ibuttonData$data_df <- dfIbutton
  hh3791@coalLog$data_df <- dfCoalLog
  trainingHhs[["3791"]] <- hh3791
  rm(hh3791, dfIbutton, dfCoalLog, dtetme)

  return(trainingHhs)
}

fineTune <- function(hh = NULL) {
  dfIbutton <- hh@ibuttonData$data_df
  dfCoalLog <- hh@coalLog$data_df
  
#   # ensure that both data frames are sorted chronologically
#   dfCoalLog <- dfCoalLog[order(as.integer(dfCoalLog$date)),]
#   rownames(dfCoalLog) <- c(1:nrow(dfCoalLog))
#   dfIbutton <- dfIbutton[order(as.integer(dfIbutton$date)),]
#   rownames(dfIbutton) <- c(1:nrow(dfIbutton))
  
  # in an ibutton-log ignition pair, take the earliest ignition time among the two 
  # and align the ignition time in both sets with the time interval pattern in the ib data
  for (r in 2:nrow(dfIbutton)) {
    if (is.na(dfIbutton[r, "ignition"])) {next}
    if (dfIbutton[r, "ignition"] != "yes") {next}
    if (dfIbutton[r, "log_fire_type"] == "unmatched") {next}
    if (dfIbutton[r, "log_ignition_time"] == dfIbutton[r, "date"]) {next}
    
    if (dfIbutton[r, "date"] > dfIbutton[r, "log_ignition_time"]) {
      
      diffSecs <- difftime(time1 = dfIbutton[r, "date"], 
                           time2 = dfIbutton[r, "log_ignition_time"], 
                           tz = TIME_ZONE, 
                           units = "secs")
      numIdxxToInclude <- ceiling(diffSecs / (IB_INTERVAL*60)) # this should never be less than 1
      
      startIdx <- (r - numIdxxToInclude)
      if (startIdx < 1) {startIdx <- 1}
      
      dfIbutton[startIdx, "ignition"] <- "yes"
      dfIbutton[r, "ignition"] <- NA
      
      dfIbutton[startIdx, "run_length"] <- dfIbutton[r, "run_length"] + numIdxxToInclude
      dfIbutton[r, "run_length"] <- NA
      
      dfIbutton[startIdx:(r-1),"fire"] <- "yes"
      
      dfIbutton[startIdx, "log_fire_type"] <- dfIbutton[r, "log_fire_type"]
      dfIbutton[r, "log_fire_type"] <- NA
      
      logIdx <- which(dfCoalLog$date == dfIbutton[r, "log_ignition_time"])
      dfCoalLog[logIdx, "date"] <- dfIbutton[startIdx, "date"]
      dfCoalLog[logIdx, "ib_ignition_time"] <- dfIbutton[startIdx, "date"]
      rm(logIdx)
      
      dfIbutton[startIdx, "log_ignition_time"] <- dfIbutton[startIdx, "date"]
      dfIbutton[r, "log_ignition_time"] <- NA
      rm(startIdx)
      
    } else {
      
      if (dfIbutton[r, "date"] < dfIbutton[r, "log_ignition_time"]) {
        logIdx <- which(dfCoalLog$date == dfIbutton[r, "log_ignition_time"])
        dfCoalLog[logIdx, "date"] <- dfIbutton[r, "date"]
        dfCoalLog[logIdx, "ib_ignition_time"] <- dfIbutton[r, "date"]
        dfIbutton[r, "log_ignition_time"] <- dfIbutton[r, "date"]
      }
    }
  }
  
  # sweep in front of all matched "new" ignitions
  sweepSize <- ceiling(1*60 / IB_INTERVAL)
  for (r in 2:nrow(dfIbutton)) {
    if (is.na(dfIbutton[r, "log_fire_type"])) {next}
    if (dfIbutton[r, "log_fire_type"] == "new") {
      if (r - sweepSize > 0) {
        dfIbutton[(r-sweepSize):(r-1), "fire"] <- "no"
      } else {
        dfIbutton[1:(r-1), "fire"] <- "no"
      }
    }
  }
  
  # propagate unmatched log ignitions (new or refill, both are handled here) to the ibutton set
  for (r in 1:nrow(dfCoalLog)) {
    if (dfCoalLog[r, "ib_assigned"]) {next}
    
    # if it is a new fire, it is necessary to sweep a bit in front of it in the 'fire' field, 
    # to ensure that it is not accidentally joined up with another, previously ignited fire, 
    # consequently falsely recording a refill. 
    # sweepSize (given in number of records) should equal at least 1 hour
    if (dfCoalLog[r, "new_or_refill"] == "new") {
      sweepSize <- ceiling(1*60 / IB_INTERVAL)
    } else {
      sweepSize <- 0
    }
    
    # see if there were any other fires later on the same day, 
    # either according to the ibuttons or according to the log
    ## check for the coal log
    timeOfNextLogFire <- NULL
    if ((yday(dfCoalLog[r+1, "date"]) == yday(dfCoalLog[r, "date"])) & 
        (dfCoalLog[r+1, "date"] > dfCoalLog[r, "date"]) &
        (r < nrow(dfCoalLog))) {
      timeOfNextLogFire <- dfCoalLog[r+1, "date"]
    } 
    if (VERBOSE) {
      message("dfCoalLog[r, 'date']: ", dfCoalLog[r, "date"])
      message("yday(dfCoalLog[r, 'date']): ", yday(dfCoalLog[r, "date"]))
      if (!is.null(timeOfNextLogFire)) {
        message("timeOfNextLogFire: ", timeOfNextLogFire)
        message("yday(timeOfNextLogFire): ", yday(timeOfNextLogFire))
      }
    }
    
    ## check for the ibuttons
    timeOfNextIbFire <- NULL
    idxx <- which((yday(dfIbutton$date) == yday(dfCoalLog[r, "date"])) & 
                  (dfIbutton$date > dfCoalLog[r, "date"]))
    if (length(idxx) > 0) {
      idx <- match(x = "yes", dfIbutton[idxx, "ignition"])
      if (!is.na(idx)) {
        timeOfNextIbFire <- dfIbutton[idxx[idx], "date"]
      } 
    }
    
    if (VERBOSE) {
      if (!is.null(timeOfNextIbFire)) {
        message("timeOfNextIbFire: ", timeOfNextIbFire)
        message("yday(timeOfNextIbFire): ", yday(timeOfNextIbFire))
      }
    }
    
    # if neither the log nor the ibuttons report another fire later on the same day, 
    # just make the run length of the fire to be added = 3 hours
    if (is.null(timeOfNextLogFire) & is.null(timeOfNextIbFire)) {
      if (VERBOSE) {message("Scenario A")}
      ## find the closest date match in the ibutton data as the starting point to match the new log fire on
      idx <- which((dfIbutton$date >= (dfCoalLog[r, "date"] - dseconds((IB_INTERVAL/2) * 60))) & 
                    (dfIbutton$date < (dfCoalLog[r, "date"] + dseconds((IB_INTERVAL/2) * 60))))
      if (length(idx) < 1) {
        stop("Uh-oh. No suitable ib date match found for log fire...0.0.")
      }
      if (length(idx) > 1) {
        stop("Uh-oh. More than one suitable ib date match found for log fire. Are you sure IB_INTERVAL is specified correctly?")
      }
      # reaching this point means length(idxx) is exactly one (which is what we want)
      
      # now add the log fire to the ibutton data over the determined indices
      durationHours <- ceiling(3*60 / IB_INTERVAL)
      dfIbutton[idx:(idx + durationHours), "fire"] <- "yes"
      dfIbutton[idx, "log_ignition_time"] <- dfIbutton[idx, "date"]
      dfCoalLog[r, "ib_assigned"] <- TRUE
      dfCoalLog[r, "date"] <- dfIbutton[idx, "date"]
      dfCoalLog[r, "ib_ignition_time"] <- dfIbutton[idx, "date"]
      
      # sweep, if necessary and then move on
      if (dfCoalLog[r, "new_or_refill"] == "new") {
        dfIbutton[idx, "log_fire_type"] <- "new"
        dfIbutton[(idx - sweepSize):(idx - 1), "fire"] <- "no"
        next
      }
      
      # reaching this point means it is a refill fire, 
      # so join it up with any previous fire and then move on
      if (dfCoalLog[r, "new_or_refill"] == "refill") {
        joinIdx <- idx - match(x = "yes", table = dfIbutton[(idx-1):1, "fire"])
        if (is.na(joinIdx)) {joinIdx <- 1}
        dfIbutton[idx, "log_fire_type"] <- "refill"
        dfIbutton[joinIdx:idx, "fire"] <- "yes"
        next
      }
    }
    
    # if the log (regardless of what the ib says) reports another 
    # fire on the same day (at dfCoalLog[r+1,]) and that fire is a refill,
    # insert this new log fire into the ibutton data, letting it run until the moment of that refill
    if (!is.null(timeOfNextLogFire)) {
      if (dfCoalLog[r+1, "new_or_refill"] == "refill") {
        if (VERBOSE) {message("Scenario B")}
        
        idxStart <- which((dfIbutton$date >= (dfCoalLog[r, "date"] - dseconds((IB_INTERVAL/2) * 60))) & 
                          (dfIbutton$date < (dfCoalLog[r, "date"] + dseconds((IB_INTERVAL/2) * 60))))
        idxEnd <- which((dfIbutton$date >= (timeOfNextLogFire - dseconds((IB_INTERVAL/2) * 60))) & 
                        (dfIbutton$date < (timeOfNextLogFire + dseconds((IB_INTERVAL/2) * 60))))
        
        if ((length(idxStart) < 1) | (length(idxEnd) < 1)) {
          stop("Uh-oh. Either no suitable idxStart or no suitable idxEnd found.")
        }
        if ((length(idxStart) > 1) | (length(idxEnd) > 1)) {
          stop("Uh-oh. Either more than one potential idxStart or more than one potential idxEnd found.")
        }
        dfIbutton[idxStart:idxEnd, "fire"] <- "yes"
        dfIbutton[idxStart, "log_ignition_time"] <- dfIbutton[idxStart, "date"]
        dfCoalLog[r, "ib_assigned"] <- TRUE
        dfCoalLog[r, "date"] <- dfIbutton[idxStart, "date"]
        dfCoalLog[r, "ib_ignition_time"] <- dfIbutton[idxStart, "date"]
        
        # sweep, if necessary
        if (dfCoalLog[r, "new_or_refill"] == "new") {
          dfIbutton[idxStart, "log_fire_type"] <- "new"
          dfIbutton[(idxStart - sweepSize):(idxStart - 1), "fire"] <- "no"
          next
        }
        
        # reaching this point means it is a refill fire, 
        # so join it up with any previous fire and then move on
        if (dfCoalLog[r, "new_or_refill"] == "refill") {
          dfIbutton[idxStart, "log_fire_type"] <- "refill"
          joinIdx <- idxStart - match(x = "yes", table = dfIbutton[(idxStart-1):1, "fire"])
          if (is.na(joinIdx)) {joinIdx <- 1}
          dfIbutton[joinIdx:idxStart, "fire"] <- "yes"
          next
        }
      }
    }
    
    # if the log (regardless of what the ib says) reports another 
    # fire on the same day (at dfCoalLog[r+1,]) but that fire is also a new fire,
    # insert this new log fire into the ibutton data, 
    # letting it run until at most until 1 hour before that other fire,
    # but not for more than 3 hours on end
    if (!is.null(timeOfNextLogFire)) {
      if (dfCoalLog[r+1, "new_or_refill"] == "new") {
        if (VERBOSE) {message("Scenario C")}
          
        idxStartThisFire <- which((dfIbutton$date >= (dfCoalLog[r, "date"] - dseconds((IB_INTERVAL/2) * 60))) & 
                                  (dfIbutton$date < (dfCoalLog[r, "date"] + dseconds((IB_INTERVAL/2) * 60))))
        
        if ((length(idxStartThisFire) < 1) | (length(idxStartThisFire) > 1)) {
          stop("Uh-oh.")
        }
      
        if (difftime(time1 = timeOfNextLogFire, 
                     time2 = dfIbutton[idxStartThisFire, "date"], 
                     tz = TIME_ZONE, 
                     units = "secs") >= dseconds(4*60*60)) { # 4 hours, to leave the required minimum gap of 1 hour between two consecutive new fires
          timeEnd <- dfCoalLog[r, "date"] + dseconds(3*60*60) 
        } else {
          timeEnd <- dfCoalLog[r+1, "date"] - dseconds(1*60*60)
        }
      
        idxEndThisFire <- which((dfIbutton$date >= (timeEnd - dseconds((IB_INTERVAL/2) * 60))) & 
                                  (dfIbutton$date < (timeEnd + dseconds((IB_INTERVAL/2) * 60))))
        
        if ((length(idxEndThisFire) < 1) | (length(idxEndThisFire) > 1)) {
          stop("Uh-oh.")
        }
        if (idxEndThisFire <= idxStartThisFire) {
          stop("Uh-oh.")
        }
      
        dfIbutton[idxStartThisFire:idxEndThisFire, "fire"] <- "yes"
        dfIbutton[idxStartThisFire, "log_ignition_time"] <- dfIbutton[idxStartThisFire, "date"]
        dfCoalLog[r, "ib_assigned"] <- TRUE
        dfCoalLog[r, "date"] <- dfIbutton[idxStartThisFire, "date"]
        dfCoalLog[r, "ib_ignition_time"] <- dfIbutton[idxStartThisFire, "date"]
        
        # sweep, if necessary
        if (dfCoalLog[r, "new_or_refill"] == "new") {
          dfIbutton[idxStartThisFire, "log_fire_type"] <- "new"
          dfIbutton[(idxStartThisFire - sweepSize):(idxStartThisFire - 1), "fire"] <- "no"
          next
        }
      
        # reaching this point means it is a refill fire, 
        # so join it up with any previous fire and then move on
        if (dfCoalLog[r, "new_or_refill"] == "refill") {
          dfIbutton[idxStartThisFire, "log_fire_type"] <- "refill"
          joinIdx <- idxStartThisFire - match(x = "yes", table = dfIbutton[(idxStartThisFire-1):1, "fire"])
          if (is.na(joinIdx)) {joinIdx <- 1}
          dfIbutton[joinIdx:idxStartThisFire, "fire"] <- "yes"
          next
        }
      }
    }
    
    # if the ibutton reports another fire on the same day, but the log doesn't,
    # insert the new fire of the log into the ibuttons, 
    # letting it run until it joins up with that other ib fire, 
    # so that they become one fire.
    if (!is.null(timeOfNextIbFire)) {
      if (VERBOSE) {message("Scenario D")}
      ## find the closest date match in the ibutton data as the starting point to match the new log fire on
      idxStart <- which((dfIbutton$date >= (dfCoalLog[r, "date"] - dseconds((IB_INTERVAL/2) * 60))) & 
                        (dfIbutton$date < (dfCoalLog[r, "date"] + dseconds((IB_INTERVAL/2) * 60))))
      
      if (length(idx) < 1) {
        stop("Uh-oh. No suitable ib date match found for log fire...0.0.")
      }
      if (length(idx) > 1) {
        stop("Uh-oh. More than one suitable ib date match found for log fire. Are you sure IB_INTERVAL is specified correctly?")
      }
      # reaching this point means length(idxx) is exactly one (which is what we want)
      
      # now add the log fire to the ibutton data over the determined indices
      idxEnd <- match(x = timeOfNextIbFire, table = dfIbutton$date)
      dfIbutton[idxStart:idxEnd, "fire"] <- "yes"
      dfIbutton[idxStart, "log_ignition_time"] <- dfIbutton[idxStart, "date"]
      dfCoalLog[r, "ib_assigned"] <- TRUE
      dfCoalLog[r, "date"] <- dfIbutton[idxStart, "date"]
      dfCoalLog[r, "ib_ignition_time"] <- dfIbutton[idxStart, "date"]
      
      # sweep, if necessary
      if (dfCoalLog[r, "new_or_refill"] == "new") {
        dfIbutton[idxStart:idxEnd, "log_fire_type"] <- "new"
        dfIbutton[(idxStart - sweepSize):(idxStart - 1), "fire"] <- "no"
        next
      }
      
      # reaching this point means it is a refill fire, 
      # so join it up with any previous fire and then move on
      if (dfCoalLog[r, "new_or_refill"] == "refill") {
        dfIbutton[idxStart:idxEnd, "log_fire_type"] <- "refill"
        joinIdx <- idxStart - match(x = "yes", table = dfIbutton[(idxStart-1):1, "fire"])
        if (is.na(joinIdx)) {joinIdx <- 1}
        dfIbutton[joinIdx:idxStart, "fire"] <- "yes"
        next
      }
    }
    
    if (VERBOSE) {message("Scenario UNKNOWN")}
  }
  
  # process unmatched ibutton fire episodes
  dfIbutton[which(dfIbutton$log_fire_type == "unmatched"), "log_fire_type"] <- NA
#   print(table(dfIbutton$log_fire_type))
#   for (r in 1:nrow(dfIbutton)) {
#     if (is.na(dfIbutton[r, "log_fire_type"])) {next}
#     if (dfIbutton[r, "log_fire_type"] != "unmatched") {next}
#     message("processing unmatched ib fire; r: ", r)
#     
#     # see if it can be matched up with a previous fire on the same day
#     joinIdx <- NULL
#     idxxDay <- which((dfIbutton$date < dfIbutton[r, "date"]) & 
#                     (yday(dfIbutton$date) == yday(dfIbutton[r, "date"])))
#     if (length(idxxDay) > 0) {
#       idxxYes <- grep(pattern = "yes", x = dfIbutton[idxxDay, "fire"], fixed = TRUE)
#       if (length(idxxYes) > 0) {
#         joinIdx <- idxxYes[length(idxxYes)]
#         dfIbutton[joinIdx:r, "fire"] <- "yes"
#         
#       }
#     }
#     
#     dfIbutton[r, "ignition"] <- NA
#     dfIbutton[r, "run_length"] <- NA
#     dfIbutton[r, "log_fire_type"] <- NA
#     dfIbutton[r, "log_ignition_time"] <- NA
#     
#     if (is.null(joinIdx)) {
#       # find the first "no" or NA in the fire field after the alleged ignition and 
#       # then remove all "yes"'s up to that point
#       idxStop <- r + match(x = "no", table = dfIbutton[r+1:nrow(dfIbutton), "fire"])
#       if (is.na(idxStop)) {
#         idxStop <- r + match(x = NA, table = dfIbutton[r+1:nrow(dfIbutton), "fire"])
#         if (is.na(idxStop)) {idxStop <- nrow(dfIbutton)}
#       }
#       dfIbutton[r:idxStop, "fire"] <- "no"
#       message("idxStop: ", idxStop)
#       message("Unmatched fire removed. Duration: ", (idxStop - r))
#     }
# 
#   }
  
  # fill the empty spaces in the log_fire_type field
  for (r in 2:nrow(dfIbutton)) {
    if (dfIbutton[r, "fire"] == "yes") {
      if (!is.na(dfIbutton[r, "log_fire_type"])) {next} # to accommodate refills
      if (!is.na(dfIbutton[r-1, "log_fire_type"])) {
        dfIbutton[r, "log_fire_type"] <- dfIbutton[r-1, "log_fire_type"]
      } 
    }
  }
  
  # remove the ignition field completely from the ibuttons afterwards, also the runlength field
  dfIbutton <- remove.cols(df = dfIbutton, colNames = c("ignition", "run_length"))
  
  hh@ibuttonData$data_df <- dfIbutton
  rm(dfIbutton)
  hh@coalLog$data_df <- dfCoalLog
  rm(dfCoalLog)
  
  return(hh)
}
# ---------------------------------------- #
# load the data
load(paste(rdadir, "houseHoldList.Rda", sep = ""))

# ---------------------------------------- #
# IDENTIFIED TRAINING HOUSEHOLDS: 
# 3152, 3313, 178, 1933, 434, 3169, 172, 383, 3791
trainingStndnos <- c("3152", "3313", "178", "1933", "434", "3169", "172", "383", "3791")
trainingHhs <- vector(mode = "list")

# extract the training hhs from the household list
houseHoldList <- lapply(X = houseHoldList, FUN = function(hh) {
  if (as.character(hh@address@standNumber) %in% trainingStndnos) {
    trainingHhs <<- c(trainingHhs, hh)
  }
  return(hh)
})
rm(houseHoldList, trainingStndnos)

# loop through each of the training hhs and discard their bad log/ibutton days
load(paste(rdadir, "days_selected.Rda", sep = ""))
trainingHhs <- lapply(X = trainingHhs, FUN = function(thh) {
  daysvector <- daysSelected[[thh@address@standNumber]]
  dfIbutton <- thh@ibuttonData$data_df
  dfIbutton <- dfIbutton[which(yday(dfIbutton$date) %in% daysvector),]
  thh@ibuttonData$data_df <- dfIbutton
  rm(dfIbutton)
  
  dfCoalLog <- thh@coalLog$data_df
  dfCoalLog <- dfCoalLog[which(yday(dfCoalLog$date) %in% daysvector),]
  thh@coalLog$data_df <- dfCoalLog
  rm(dfCoalLog, daysvector)
  
  return(thh)
})
rm(daysSelected)
names(trainingHhs) <- sapply(X = trainingHhs, FUN = function(thh) {
  return(thh@address@standNumber)
})

# do the hh-specific preparation
trainingHhs <- hhSpesPrep(trainingHhs)

# do the ibutton-log ignitions matching
trainingHhs <- lapply(X = trainingHhs, FUN = matchIbuttonAndLog)

archive(fileName = "trainingHhs.Rda", currentDir = rdadir, verbose = TRUE)
save(trainingHhs, file = paste(rdadir, "trainingHhs.Rda", sep = ""))

# ---------------------------------------- #
# now do some fine tuning on the training hhs
trainingHhs <- lapply(X = trainingHhs, FUN = fineTune)

# ---------------------------------------- #
# save 
archive(fileName = "trainingHhs.Rda", currentDir = rdadir, verbose = TRUE)
archive(fileName = "trainingHhs.Rda", currentDir = novafunctdir, verbose = TRUE)
save(trainingHhs, file = paste(rdadir, "trainingHhs.Rda", sep = ""))
save(trainingHhs, file = paste(novafunctdir, "trainingHhs.Rda", sep = ""))


# ---------------------------------------- #
# ---------------------------------------- #


# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##


# daysSelected <- lapply(X = trainingHhs, FUN = function(thh) {
#   return(unique(yday(thh@ibuttonData$data_df$date)))
# })
# names(daysSelected) <- sapply(X = trainingHhs, FUN = function(thh) {
#   return(thh@address@standNumber)
# })
# save(daysSelected, file = paste(rdadir, "days_selected.Rda", sep = ""))



# hh <- trainingHhs[[1]]
# hh <- matchIbuttonAndLog(hh)
# hh@ibuttonData$data_df$log_ignition_time <- as.POSIXct(x = hh@ibuttonData$data_df$log_ignition_time, 
#                                                          tz = TIME_ZONE, 
#                                                          origin = "1970-01-01 00:00:00")
# button <- hh@ibuttonData$data_df
# log <- hh@coalLog$data_df
# 
# trainingHhs <- lapply(X = trainingHhs, FUN = function(hh) {
#   hh@ibuttonData$data_df <- wees_vure(df = hh@ibuttonData$data_df, 
#                                       fireField = "fire", 
#                                       fireLabel = "yes", 
#                                       noFireLable = "no", 
#                                       minFireLength = ceiling(1.5*60 / IB_INTERVAL))
#   return(hh)
# })

# ---------------------------------------- #
# EXPLORE THE EXTENT OF THE QC PROBLEM
# ---------------------------------------- #
{
# load(paste(rdadir, "df_coal_logs.Rda", sep = ""))
# df_coal_logs$stand_number <- as.character(df_coal_logs$stand_number)
# 
# load(paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))
# df_ibutton_data_classified$stand_number <- as.character(df_ibutton_data_classified$stand_number)
# 
# time1 <- c(4:9)
# time2 <- c(10:14)
# time3 <- c(15:20)
# time4 <- c(0:3, 21:23)
# timeNames <- c("morning", "day", "evening", "night")
# 
# # add a 'time_of_day' field to the coal log data
# df_coal_logs$time_of_day <- NA
# df_coal_logs$time_of_day <- ifelse(hour(df_coal_logs$date) %in% time1, timeNames[[1]], 
#                                    ifelse(hour(df_coal_logs$date) %in% time2, timeNames[[2]], 
#                                           ifelse(hour(df_coal_logs$date) %in% time3, timeNames[[3]], 
#                                                  timeNames[[4]])))
# 
# # add a 'time_of_day' field to the ibutton data
# df_ibutton_data_classified$time_of_day <- NA
# df_ibutton_data_classified$time_of_day <- ifelse(hour(df_ibutton_data_classified$date) %in% time1, timeNames[[1]], 
#                                                  ifelse(hour(df_ibutton_data_classified$date) %in% time2, timeNames[[2]], 
#                                                         ifelse(hour(df_ibutton_data_classified$date) %in% time3, timeNames[[3]], 
#                                                                timeNames[[4]])))
# 
# # create the necessary (empty) fields
# df_coal_logs$yrday <- yday(df_coal_logs$date)
# df_ibutton_data_classified$yrday <- yday(df_ibutton_data_classified$date)
# 
# colNames <- c("stand_number", "extension", "type", "yrday", "time_of_day", "no_of_fires_log", "no_of_fires_ib", "mtrx_code")
# template <- data.frame(matrix(nrow = 4, ncol = length(colNames)), stringsAsFactors = FALSE)
# names(template) <- colNames
# template$time_of_day <- timeNames
# 
# # split the ibutton and coal log data according to stand numbers
# standsplits_logs <- split(x = df_coal_logs, f = df_coal_logs$stand_number)
# standsplits_ibb <- split(x = df_ibutton_data_classified, f = df_ibutton_data_classified$stand_number)
# 
# standslist <- vector(mode = "list", length = length(standsplits_logs))
# 
# for (si in 1:length(standsplits_logs)) {
#   
#   sdf_log <- standsplits_logs[[si]]
#   sno <- names(standsplits_logs)[si]
#   ext <- unique(sdf_log$extension)
#   minDate <- min(sdf_log$yrday, na.rm = TRUE)
#   maxDate <- max(sdf_log$yrday, na.rm = TRUE)
#   
#   daysplits_log <- split(x = sdf_log, sdf_log$yrday)
#   dayslist <- vector(mode = "list", length = (maxDate - minDate + 1))
#   names(dayslist) <- c(minDate:maxDate)
#   
#   sdf_ibb <- NULL
#   if (sno %in% names(standsplits_ibb)) {sdf_ibb <- standsplits_ibb[[sno]]}
#   
#   type <- ifelse(!is.null(sdf_ibb), 
#                  paste(as.character(sdf_ibb$energy_type), as.character(sdf_ibb$insulation_type), sep = "_"), 
#                  NA_character_)
#   
#   daysplits_ib <- NULL
#   if (!is.null(sdf_ibb)) {daysplits_ib <- split(x = sdf_ibb, f = sdf_ibb$yrday)}
#   
#   for (d in minDate:maxDate) {
#     
#     newdf <- template
#     newdf$stand_number <- sno
#     newdf$extension <- ext
#     newdf$type <- type
#     newdf$yrday <- d
#     
#     timesplits_log <- NULL
#     if (as.character(d) %in% names(daysplits_log)) {
#       ddf <- daysplits_log[[as.character(d)]]
#       timesplits_log <- split(x = ddf, f = ddf$time_of_day)
#     } 
#     
#     timesplits_ib <- NULL
#     if (!is.null(daysplits_ib)) {
#       if (as.character(d) %in% names(daysplits_ib)) {
#         ddf <- daysplits_ib[[as.character(d)]]
#         timesplits_ib <- split(x = ddf, f = ddf$time_of_day)
#       }
#     } 
#     
#     for (t in 1:length(timeNames)) {
#       tnm <- timeNames[t]
#       
#       if (!is.null(timesplits_log)) {
#         newdf[which(newdf$time_of_day == tnm), "no_of_fires_log"] <- ifelse(tnm %in% names(timesplits_log), 
#                                                                           nrow(timesplits_log[[tnm]]), 
#                                                                                0)
#       } else {
#         newdf[, "no_of_fires_log"] <- 0
#       }
#       
#       if (is.null(timesplits_ib)) {next}
#       
#       # reaching this point means timesplits_ib is not NULL
#       if (!(tnm %in% names(timesplits_ib))) {next}
#       
#       # reaching this point means tnm is in timesplits_ib
#       episodes <- table((rle(timesplits_ib[[tnm]]$fire))[["values"]])
#       if ("yes" %in% names(episodes)) {
#         newdf[, "no_of_fires_ib"] <- episodes[["yes"]]
#       } else {
#         if ("no" %in% names(episodes)) {
#           newdf[, "no_of_fires_ib"] <- 0
#         } else {
#           newdf[, "no_of_fires_ib"] <- NA
#         }
#       }
#       
#     }
#     
#     dayslist[[d]] <- newdf
#     rm(newdf)
#   }
#   
#   standslist[[si]] <- do.call("rbind", dayslist)
#   rm(dayslist)
# }
# 
# df_compare <- do.call("rbind", standslist)
# rm(standslist)
# 
# df_compare$mtrx_code <- ifelse(is.na(df_compare$no_of_fires_log) | is.na(df_compare$no_of_fires_log),
#                                NA, 
#                                ifelse(df_compare$no_of_fires_log == df_compare$no_of_fires_ib,
#                                      "[log = ib]", 
#                                      ifelse(df_compare$no_of_fires_log > df_compare$no_of_fires_ib,
#                                             "[log > ib]", "[log < ib]")))
# 
# df_compare <- df_compare[which((!is.na(df_compare$no_of_fires_log)) | (!is.na(df_compare$no_of_fires_ib))),]
}
# ---------------------------------------- #


# # now loop through each of the training hhs and discard their bad log/ibutton days
# trainingHhs <- lapply(X = trainingHhs, FUN = function(hh) {
#   
#   hh <- matchIbuttonAndLog(hh)
#   hh@ibuttonData$data_df$log_ignition_time <- as.POSIXct(x = hh@ibuttonData$data_df$log_ignition_time, 
#                                                          tz = TIME_ZONE, 
#                                                          origin = "1970-01-01 00:00:00")
# 
#   message("Before sweep: nrow of ibuttons: ", nrow(hh@ibuttonData$data_df))
#   message("Before sweep: nrow of log: ", nrow(hh@coalLog$data_df))
#   
#   ctch <- apply(X = as.array(unique(yday(hh@ibuttonData$data_df$date))), MARGIN = 1, FUN = function(d) {
#     if (!(d %in% unique(yday(hh@coalLog$data_df$date)))) {return(0)}
#     
#     if (sketchIbVsLog(hh, day = d) == -1) {return(0)}
#     
#     okay <- FALSE
#     keepOrNot <- 3
#     while(!okay) {
#       message("Should we keep this day? 1 for yes, 0 for no, 2 to skip:")
#       keepOrNot <- readline()
#       if (as.integer(keepOrNot) %in% c(2, 1, 0)) {okay <- TRUE}
#     }
#     
#     if (keepOrNot == "2") {return(2)}
#     if (keepOrNot == "1") {return(1)}
#     if (keepOrNot == "0") {
#       # remove the day from the ibutton data
#       idxx <- which(yday(hh@ibuttonData$data_df$date) == d)
#       hh@ibuttonData$data_df <<- hh@ibuttonData$data_df[-idxx,]
#       # remove the day from the coal log data
#       idxx <- which(yday(hh@coalLog$data_df$date) == d)
#       hh@coalLog$data_df <<- hh@coalLog$data_df[-idxx,]
#       return(0)
#     }
#     
#     return(1)
#   })
#   rm(ctch)
#   
#   message("After sweep: nrow of ibuttons: ", nrow(hh@ibuttonData$data_df))
#   message("After sweep: nrow of log: ", nrow(hh@coalLog$data_df))
#   
#   return(hh)
# })
# 
# hh <- trainingHhs[[3]]
# days <- unique(yday(hh@ibuttonData$data_df$date))
# ctch <- apply(X = as.array(days), MARGIN = 1, FUN = function(d) {
#   sketchIbVsLog(hh = hh, day = d)
# })
