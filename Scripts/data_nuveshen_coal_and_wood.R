# Date created: 30 November 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# Script to prepare coal and wood use estimations for Nuveshen through a log-informed ibutton approach
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
VERBOSE <- TRUE
DEBUG <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #

# load the data
load(paste(rdadir, "houseHoldList.Rda", sep = ""))

hhInfo <- lapply(X = houseHoldList, FUN = function(hh) {
  infoStruct <- vector(mode = "list", length = 3)
  names(infoStruct) <- c("avgTotCoalPf", "avgTotWoodPf", "ignitionsPerMonth")
  
  dfIbutton <- hh@ibuttonData$data_df
  dfCoalLog <- hh@coalLog$data_df
  
  if (is.null(dfIbutton) | is.null(dfCoalLog)) {
    return(NULL)
  }
  
  weight_wb <- hh@coalLog$meta_info$weight_of_wood_batch_kg
  weight_cc <- hh@coalLog$meta_info$weight_of_a_filled_container_kg
  fireTotals_coal <- NULL
  fireTotals_wood <- NULL
  
  fireTot_c <- 0
  fireTot_w <- 0
  
  for (r in 1:nrow(dfCoalLog)) {
    
    if (dfCoalLog[r, "new_or_refill"] == "new") {
      fireTot_c <- dfCoalLog[r, "no_of_coal_containers"] * weight_cc
      fireTot_w <- dfCoalLog[r, "no_of_wood_batches"] * weight_wb
    } else {
      if (dfCoalLog[r, "new_or_refill"] == "refill") {
        fireTot_c <- fireTot_c + dfCoalLog[r, "no_of_coal_containers"] * weight_cc
        fireTot_w <- fireTot_w + dfCoalLog[r, "no_of_wood_batches"] * weight_wb
      } else {"UEO detected!"}
    }
    
    if (r == nrow(dfCoalLog)) {
      fireTotals_coal <- c(fireTotals_coal, fireTot_c)
      fireTotals_wood <- c(fireTotals_wood, fireTot_w)
    } else {
      if (dfCoalLog[r+1, "new_or_refill"] == "new") {
        fireTotals_coal <- c(fireTotals_coal, fireTot_c)
        fireTotals_wood <- c(fireTotals_wood, fireTot_w)
      }
    }
    
  }
  
  infoStruct$avgTotCoalPf <- mean(fireTotals_coal, na.rm = TRUE)
  infoStruct$avgTotWoodPf <- mean(fireTotals_wood, na.rm = TRUE)
  rm(dfCoalLog)
  
  dfIbutton$ignition <- NA
  isPrevFire <- FALSE
  for (r in 1:nrow(dfIbutton)) {
    if (is.na(dfIbutton[r, "fire"])) {
      isPrevFire <- FALSE
      next
    }
    if (dfIbutton[r, "fire"] == "no") {
      isPrevFire <- FALSE
      next
    }
    if (dfIbutton[r, "fire"] == "yes" & isPrevFire == FALSE) {
      dfIbutton[r, "ignition"] <- TRUE
      isPrevFire <- TRUE
      next
    }
    if (dfIbutton[r, "fire"] == "yes") {
      isPrevFire <- TRUE
      next
    }
  }
  
  monthsplits <- split(x = dfIbutton, f = month(dfIbutton$date))
  infoStruct$ignitionsPerMonth <- sapply(X = monthsplits, FUN = function(mdf) {
    tble <- table(mdf$ignition)
    if ("TRUE" %in% names(tble)) {
      return(tble[[TRUE]])
    } else {return(0)}
  })  
  
  return(infoStruct)
  
})
names(hhInfo) <- sapply(X = houseHoldList, FUN = function(hh) {return(hh@address@standNumber)})

CARRIER <- "coal"

statFrame <- data.frame(matrix(nrow = 3, ncol = 4, data = 0), stringsAsFactors = FALSE)
rownames(statFrame) <- c("none", "basic", "full")
colnames(statFrame) <- c("control", "coal", "lpg", "elec")

statFrame_n <- statFrame
statFrame_total <- statFrame
statFrame_avg <- statFrame
statFrame_groupAvg <- statFrame


ctch <- lapply(X = houseHoldList, FUN = function(hh) {
  stnd <- hh@address@standNumber
  if (is.null(hhInfo[[stnd]])) {return(0)}
  if (is.null(hhInfo[[stnd]]$ignitionsPerMonth)) {return(0)}
  
  statFrame_n[hh@insulationType, hh@energyType] <<- statFrame_n[hh@insulationType, hh@energyType] + 1
  if (CARRIER == "coal") {var <- "avgTotCoalPf"} else {var <- "avgTotWoodPf"}
  
  totPerWinter <- (hhInfo[[stnd]][[var]]) * (hhInfo[[stnd]]$ignitionsPerMonth[["8"]]) * 4 
    # *4 to get the total for an average four-month winter
  
  statFrame_total[hh@insulationType, hh@energyType] <<- statFrame_total[hh@insulationType, hh@energyType] + totPerWinter
})
rm(ctch)

statFrame_avg <- statFrame_total / statFrame_n

# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##

# ctch <- sapply(X = hhInfo, FUN = function(infS) {print(infS$ignitionsPerMonth)})

